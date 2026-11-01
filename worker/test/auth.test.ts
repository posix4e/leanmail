import { createExecutionContext, waitOnExecutionContext } from "cloudflare:test";
import { beforeEach, describe, expect, it } from "vitest";
import { AUTH_RATE_PER_HOUR, PASSKEY_CHALLENGE_TTL_MS, mintPasskeyChallenge, verifyPasskeyChallenge } from "../src/auth";
import { stampHeaders } from "../src/container";
import type { Env } from "../src/env";
import { dailyTick, handleFetch } from "../src/index";
import { handleInternal, mailFrom } from "../src/internal";
import { b64uEncode, deriveSecret, signMac, signMacWithKey } from "../src/tokens";
import { SECRET, addDomain, captureLogs, env, resetDb } from "./helpers";

const S = "test-session-secret";
const ORIGIN = "https://leanmail.example";

interface Call {
  name: string;
  acct: string;
  method: string;
  path: string;
  search: string;
  headers: Record<string, string>;
  body: string;
  adminAccount?: string;
}

/** An env whose MAIL namespace records every proxied request instead of starting a container. */
function stubbed(reply: (c: Call) => Response = (c) => Response.json({ ok: true, via: c.name })) {
  const calls: Call[] = [];
  const MAIL = {
    idFromName: (n: string) => n,
    get: (name: string) => ({
      proxy: async (r: Request, acct: string, opts: { adminAccount?: string } = {}) => {
        const u = new URL(r.url);
        const c: Call = { name, acct, method: r.method, path: u.pathname, search: u.search, headers: Object.fromEntries(r.headers), body: new TextDecoder().decode(await r.arrayBuffer()), adminAccount: opts.adminAccount };
        calls.push(c);
        return reply(c);
      },
    }),
  };
  return { e: { ...env, MAIL } as unknown as Env, calls };
}

async function fetchVia(e: Env, path: string, init: RequestInit = {}): Promise<Response> {
  const ctx = createExecutionContext();
  const res = await handleFetch(new Request(ORIGIN + path, init), e, ctx);
  await waitOnExecutionContext(ctx);
  return res;
}

async function activeAccount(id: string, email: string, status = "active") {
  await env.DB.prepare("INSERT INTO accounts (id, email, status, updated_at) VALUES (?1, ?2, ?3, 0)").bind(id, email, status).run();
}

async function internalReq(method: string, path: string, body: unknown, acct: string): Promise<Request> {
  return new Request(ORIGIN + path, {
    method,
    headers: { "content-type": "application/json", "x-account-id": acct, "x-internal-secret": await deriveSecret(SECRET, acct) },
    body: body === undefined ? undefined : JSON.stringify(body),
  });
}

beforeEach(resetDb);

describe("stampHeaders (MailContainer.proxy)", () => {
  it("drops client x-account-id, x-internal-secret and every x-admin-*, then sets its own", () => {
    const h = stampHeaders(
      new Headers({ "x-admin-account": "evil", "X-Admin-Other": "x", "x-account-id": "b1", "x-internal-secret": "guess", "cf-connecting-ip": "1.2.3.4", cookie: "c=1" }),
      "a1",
      "derived",
    );
    expect(Object.fromEntries(h)).toEqual({ "x-account-id": "a1", "x-internal-secret": "derived", "x-lm-public": "1", "cf-connecting-ip": "1.2.3.4", cookie: "c=1" });
    // the passkey login's voucher (set by src/auth.ts) passes through to the container
    expect(stampHeaders(new Headers({ "x-passkey-challenge-ok": "v" }), "a1", "d").get("x-passkey-challenge-ok")).toBe("v");
    const admin = stampHeaders(new Headers({ "x-admin-account": "evil" }), "_operator", "op", { adminAccount: "a1" });
    expect(admin.get("x-admin-account")).toBe("a1");
  });
});

describe("/internal/email/send and MAIL_FROM", () => {
  it("defaults MAIL_FROM to no-reply@<host of PUBLIC_ORIGIN>, as the container does", () => {
    expect(mailFrom({ ...env, MAIL_FROM: undefined, PUBLIC_ORIGIN: "http://127.0.0.1:8787" } as Env)).toBe("no-reply@127.0.0.1");
    expect(mailFrom({ ...env, MAIL_FROM: undefined, PUBLIC_ORIGIN: "https://leanmail.io/x" } as Env)).toBe("no-reply@leanmail.io");
    expect(mailFrom({ ...env, MAIL_FROM: "Hello@Leanmail.io" } as Env)).toBe("hello@leanmail.io");
  });

  it("an account may no longer send as MAIL_FROM through /internal/email/send; the operator still may", async () => {
    const sent: string[] = [];
    const e = { ...env, MAIL_FROM: "no-reply@leanmail.io", EMAIL: { send: async (m: EmailMessage) => void sent.push(`${m.from}>${m.to}`) } } as unknown as Env;
    const body = (from: string) => ({ from, to: ["u@x.example"], raw_b64: btoa(`From: ${from}\r\nSubject: x\r\n\r\nx`) });
    const r = await handleInternal(await internalReq("POST", "/internal/email/send", body("No-Reply@leanmail.io"), "a1"), e);
    expect(r.status).toBe(403);
    expect(await r.json()).toEqual({ error: "from_not_allowed" });
    expect((await handleInternal(await internalReq("POST", "/internal/email/send", body("other@leanmail.io"), "a1"), e)).status).toBe(403);
    expect((await handleInternal(await internalReq("POST", "/internal/email/send", body("no-reply@leanmail.io"), "_operator"), e)).status).toBe(200);
    expect(sent).toEqual(["no-reply@leanmail.io>u@x.example"]);
  });
});

describe("item 3: PUT /internal/accounts replace_domains_for", () => {
  it("drops the account's unlisted domains, leaves others' alone, and keeps has_gmail when absent", async () => {
    await addDomain("old.example", "a1");
    await addDomain("keep.example", "a1");
    await addDomain("theirs.example", "a2");
    await env.DB.prepare("INSERT INTO accounts (id, email, status, has_gmail, updated_at) VALUES ('a1', 'a@x.example', 'pending', 1, 0)").run();
    const body = { accounts: [{ id: "a1", email: "a@x.example", status: "active" }], domains: [{ name: "keep.example", account_id: "a1" }, { name: "new.example", account_id: "a1" }], replace_domains_for: ["a1"] };
    expect((await handleInternal(await internalReq("PUT", "/internal/accounts", body, "_operator"), env)).status).toBe(200);
    const { results } = await env.DB.prepare("SELECT name, account_id FROM domains ORDER BY name").all();
    expect(results).toEqual([
      { name: "keep.example", account_id: "a1" },
      { name: "new.example", account_id: "a1" },
      { name: "theirs.example", account_id: "a2" },
    ]);
    expect(await env.DB.prepare("SELECT status, has_gmail FROM accounts WHERE id = 'a1'").first()).toEqual({ status: "active", has_gmail: 1 });

    // No domains listed for it: all of its rows go.
    const none = { accounts: [], domains: [], replace_domains_for: ["a1"] };
    expect((await handleInternal(await internalReq("PUT", "/internal/accounts", none, "_operator"), env)).status).toBe(200);
    expect((await env.DB.prepare("SELECT count(*) AS n FROM domains WHERE account_id = 'a1'").first())!.n).toBe(0);
    expect((await handleInternal(await internalReq("PUT", "/internal/accounts", { replace_domains_for: ["bad id"] }, "_operator"), env)).status).toBe(400);
  });
});

describe("item 4: POST /internal/wake/:acct {provision}", () => {
  it("proxies POST /internal/provision to the account and returns its reply; no body is a plain wake", async () => {
    const { e, calls } = stubbed(() => Response.json({ ok: true, created: true, welcome_sent: true }, { status: 200 }));
    const woke: string[] = [];
    const wake = async (_e: Env, a: string) => void woke.push(a);
    const provision = { email: "new@x.example", is_admin: false };
    const r = await handleInternal(await internalReq("POST", "/internal/wake/a9", { provision }, "_operator"), e, { wake });
    expect(r.status).toBe(200);
    expect(await r.json()).toEqual({ ok: true, created: true, welcome_sent: true });
    expect(calls).toHaveLength(1);
    expect(calls[0]).toMatchObject({ name: "a9", acct: "a9", method: "POST", path: "/internal/provision" });
    expect(JSON.parse(calls[0]!.body)).toEqual(provision);
    expect(woke).toEqual([]);

    expect((await handleInternal(await internalReq("POST", "/internal/wake/a9", undefined, "_operator"), e, { wake })).status).toBe(202);
    expect(woke).toEqual(["a9"]);

    const { e: e502 } = stubbed(() => Response.json({ error: "x" }, { status: 500 }));
    expect((await handleInternal(await internalReq("POST", "/internal/wake/a9", { provision }, "_operator"), e502, { wake })).status).toBe(500);
    expect((await handleInternal(await internalReq("POST", "/internal/wake/a9", { provision: { email: "nope" } }, "_operator"), e, { wake })).status).toBe(400);
    expect((await handleInternal(await internalReq("POST", "/internal/wake/a9", { provision }, "a9"), e, { wake })).status).toBe(403);
  });
});

describe("item 5: public auth routes", () => {
  it("POST /auth/signup goes to the operator with cf-connecting-ip, and nothing client-stamped", async () => {
    const { e, calls } = stubbed(() => Response.json({ ok: true }, { status: 202 }));
    const r = await fetchVia(e, "/auth/signup", { method: "POST", headers: { "content-type": "application/json", "cf-connecting-ip": "203.0.113.9" }, body: JSON.stringify({ email: "s@x.example" }) });
    expect(r.status).toBe(202);
    expect(calls).toHaveLength(1);
    expect(calls[0]).toMatchObject({ name: "_operator", acct: "_operator", path: "/auth/signup" });
    expect(calls[0]!.headers["cf-connecting-ip"]).toBe("203.0.113.9");
  });

  it("POST /auth/login answers 202 to all, asking only an active account's container for a link", async () => {
    await activeAccount("a1", "alice@x.example");
    await activeAccount("p1", "pending@x.example", "pending");
    const { e, calls } = stubbed(() => Response.json({ ok: true }, { status: 202 }));
    const post = (email: string, ip: string) =>
      fetchVia(e, "/auth/login", { method: "POST", headers: { "content-type": "application/json", "cf-connecting-ip": ip }, body: JSON.stringify({ email }) });
    const { lines } = await captureLogs(async () => {
      for (const [email, ip] of [["Alice@X.example", "1.1.1.1"], ["nobody@x.example", "1.1.1.2"], ["pending@x.example", "1.1.1.3"]] as const) {
        const r = await post(email, ip);
        expect(r.status).toBe(202);
        expect(await r.json()).toEqual({ ok: true });
      }
    });
    expect(calls).toHaveLength(1);
    expect(calls[0]).toMatchObject({ name: "a1", acct: "a1", method: "POST", path: "/internal/auth/magic" });
    expect(JSON.parse(calls[0]!.body)).toEqual({ email: "alice@x.example", purpose: "signin" });
    expect(lines.filter((l) => l.ev === "auth.login").map((l) => l.outcome).sort()).toEqual(["no_active_account", "no_active_account", "sent"]);
    expect(JSON.stringify(lines)).not.toMatch(/@/);
    expect((await post("not-an-email", "1.1.1.4")).status).toBe(400);
  });

  it("POST /auth/login with the no-JS form answers 303 /signin?sent=1 for any email", async () => {
    await activeAccount("a1", "alice@x.example");
    const { e, calls } = stubbed();
    for (const email of ["alice@x.example", "nobody@x.example"]) {
      const r = await fetchVia(e, "/auth/login", { method: "POST", headers: { "content-type": "application/x-www-form-urlencoded" }, body: new URLSearchParams({ email }).toString() });
      expect(r.status).toBe(303);
      expect(r.headers.get("location")).toBe("/signin?sent=1");
    }
    expect(calls.map((c) => `${c.acct} ${c.path}`)).toEqual(["a1 /internal/auth/magic"]);
  });

  it("rate-limits /auth/login and the code routes to 10 an hour per client IP", async () => {
    const { e } = stubbed();
    const login = (ip: string) => fetchVia(e, "/auth/login", { method: "POST", headers: { "content-type": "application/json", "cf-connecting-ip": ip }, body: '{"email":"x@x.example"}' });
    for (let i = 0; i < AUTH_RATE_PER_HOUR; i++) expect((await login("9.9.9.9")).status).toBe(202);
    const r = await login("9.9.9.9");
    expect(r.status).toBe(429);
    expect(r.headers.get("retry-after")).toBe("3600");
    expect((await login("9.9.9.8")).status).toBe(202);
    // Codes count separately from logins, and the two code routes share one bucket.
    const code = (path: string, body: string, ct: string) => fetchVia(e, path, { method: "POST", headers: { "content-type": ct, "cf-connecting-ip": "9.9.9.9" }, body });
    for (let i = 0; i < AUTH_RATE_PER_HOUR / 2; i++) {
      expect((await code("/auth/code", '{"email":"x@x.example","code":"123456"}', "application/json")).status).toBe(200);
      expect((await code("/signin/code", "email=x%40x.example&code=123456", "application/x-www-form-urlencoded")).status).toBe(200);
    }
    expect((await code("/auth/code", '{"email":"x@x.example","code":"123456"}', "application/json")).status).toBe(429);
    // Only the IP's hash is stored.
    const keys = (await env.DB.prepare("SELECT key FROM auth_rate").all<{ key: string }>()).results.map((r) => r.key);
    expect(keys.every((k) => /^(login|code):[0-9a-f]{64}$/.test(k))).toBe(true);
    // The daily cron forgets windows over an hour old.
    await env.DB.prepare("UPDATE auth_rate SET window_start = 0").run();
    await captureLogs(() => dailyTick(env, async () => {}));
    expect((await env.DB.prepare("SELECT count(*) AS n FROM auth_rate").first())!.n).toBe(0);
  });

  it("GET /auth/magic?t= goes to the account its MAC names; a bad link is a 400 page", async () => {
    const { e, calls } = stubbed(() => new Response(null, { status: 302, headers: { location: "/", "set-cookie": "lm_session=x" } }));
    const t = await signMac("v1", "a1", "rand.9999999999999", S);
    const r = await fetchVia(e, `/auth/magic?t=${encodeURIComponent(t)}`);
    expect(r.status).toBe(302);
    expect(calls[0]).toMatchObject({ acct: "a1", path: "/auth/magic", search: `?t=${encodeURIComponent(t)}` });
    const forged = await signMacWithKey("v1", "b1", "rand.1", await deriveSecret(S, "a1"));
    for (const bad of [forged, "v1.a1.x.y", "", await signMac("lm1", "a1", "r", S)]) {
      const b = await fetchVia(e, `/auth/magic?t=${encodeURIComponent(bad)}`);
      expect(b.status).toBe(400);
      expect(b.headers.get("content-type")).toContain("text/html");
    }
    expect(calls).toHaveLength(1);
  });

  it("POST /auth/code and /signin/code go to the email's active account, else the operator", async () => {
    await activeAccount("a1", "alice@x.example");
    const { e, calls } = stubbed();
    await fetchVia(e, "/auth/code", { method: "POST", headers: { "content-type": "application/json" }, body: JSON.stringify({ email: "ALICE@x.example", code: "123456", name: "phone" }) });
    await fetchVia(e, "/auth/code", { method: "POST", headers: { "content-type": "application/json" }, body: JSON.stringify({ email: "who@x.example", code: "123456" }) });
    await fetchVia(e, "/signin/code", { method: "POST", headers: { "content-type": "application/x-www-form-urlencoded" }, body: "email=alice%40x.example&code=123456" });
    await fetchVia(e, "/signin/code", { method: "POST", headers: { "content-type": "application/x-www-form-urlencoded" }, body: "email=who%40x.example&code=123456" });
    expect(calls.map((c) => `${c.acct} ${c.path}`)).toEqual(["a1 /auth/code", "_operator /auth/code", "a1 /signin/code", "_operator /signin/code"]);
    expect(JSON.parse(calls[0]!.body)).toEqual({ email: "ALICE@x.example", code: "123456", name: "phone" });
    expect(calls[2]!.body).toBe("email=alice%40x.example&code=123456");
  });
});

describe("item 6: /admin/*", () => {
  it("a JSON or script request goes to the operator with the cookie's account; a navigation to the account's shell", async () => {
    const { e: e0, calls } = stubbed();
    const e = { ...e0, PUBLIC_ORIGIN: ORIGIN } as Env;
    const cookie = `lm_session=${await signMac("v1", "a1", "sid.9999999999999.0", S)}`;
    await fetchVia(e, "/admin/signups", { headers: { cookie, accept: "application/json", "x-admin-account": "evil" } });
    await fetchVia(e, "/admin/signups/01J/approve", { method: "POST", headers: { cookie, origin: ORIGIN, accept: "text/html", "x-requested-with": "LeanMail" } });
    await fetchVia(e, "/admin/signups", { headers: { cookie, accept: "text/html,application/xhtml+xml" } });
    expect(calls.map((c) => `${c.name} ${c.acct} ${c.method} ${c.path} ${c.adminAccount ?? "-"}`)).toEqual([
      "_operator _operator GET /admin/signups a1",
      "_operator _operator POST /admin/signups/01J/approve a1",
      "a1 a1 GET /admin/signups -",
    ]);
  });

  it("without a valid cookie: 401 for JSON, 302 /signin for a navigation; a bearer is not enough", async () => {
    const { e, calls } = stubbed();
    const bearer = `Bearer ${await signMac("lm1", "a1", "r", S)}`;
    const forged = `lm_session=${await signMacWithKey("v1", "a1", "sid.1.0", S)}`;
    expect((await fetchVia(e, "/admin/signups", { headers: { accept: "application/json", "x-admin-account": "a1" } })).status).toBe(401);
    expect((await fetchVia(e, "/admin/signups", { headers: { accept: "application/json", authorization: bearer } })).status).toBe(401);
    expect((await fetchVia(e, "/admin/signups", { headers: { accept: "application/json", cookie: forged } })).status).toBe(401);
    const nav = await fetchVia(e, "/admin/accounts", { headers: { accept: "text/html" } });
    expect(nav.status).toBe(302);
    expect(nav.headers.get("location")).toBe("/signin");
    expect(calls).toEqual([]);
  });
});

describe("item 7 and the page addendum: everything else", () => {
  it("signed-out sign-in pages and static files go to the operator; with a cookie, to the account", async () => {
    const { e, calls } = stubbed();
    await fetchVia(e, "/signin", { headers: { accept: "text/html" } });
    await fetchVia(e, "/signin", { method: "POST", headers: { "content-type": "application/x-www-form-urlencoded" }, body: "email=a%40x.example" });
    await fetchVia(e, "/static/app.css");
    const cookie = `lm_session=${await signMac("v1", "a1", "sid.9999999999999.0", S)}`;
    await fetchVia(e, "/signin", { headers: { cookie, accept: "text/html" } });
    await fetchVia(e, "/static/app.css", { headers: { cookie } });
    expect(calls.map((c) => `${c.acct} ${c.method} ${c.path}`)).toEqual([
      "_operator GET /signin",
      "_operator POST /signin",
      "_operator GET /static/app.css",
      "a1 GET /signin",
      "a1 GET /static/app.css",
    ]);
  });

  it("a cookie or bearer reaches its account; signed out, a navigation is sent to /signin and an API call is 401", async () => {
    const { e, calls } = stubbed();
    const cookie = `lm_session=${await signMac("v1", "a1", "sid.9999999999999.0", S)}`;
    const bearer = `Bearer ${await signMac("lm1", "b1", "r", S)}`;
    await fetchVia(e, "/inbox", { headers: { cookie, accept: "text/html" } });
    await fetchVia(e, "/api/msgs", { headers: { authorization: bearer } });
    expect(calls.map((c) => `${c.acct} ${c.path}`)).toEqual(["a1 /inbox", "b1 /api/msgs"]);

    const nav = await fetchVia(e, "/inbox", { headers: { accept: "text/html" } });
    expect(nav.status).toBe(302);
    expect(nav.headers.get("location")).toBe("/signin");
    expect((await fetchVia(e, "/", { headers: { accept: "text/html" } })).headers.get("location")).toBe("/signin");
    expect((await fetchVia(e, "/api/msgs", { headers: { accept: "application/json" } })).status).toBe(401);
    expect((await fetchVia(e, "/inbox", { headers: { accept: "text/html", "x-requested-with": "LeanMail" } })).status).toBe(401);
    expect(calls).toHaveLength(2);
  });

  it("DEV_ACCOUNT still answers signed-out requests on localhost only", async () => {
    const { e, calls } = stubbed();
    const ctx = createExecutionContext();
    await handleFetch(new Request("http://localhost:8787/inbox", { headers: { accept: "text/html" } }), e, ctx);
    expect(calls.map((c) => `${c.acct} ${c.path}`)).toEqual(["dev /inbox"]);
  });
});

describe("M9b: passkey login", () => {
  const b64uText = (t: string) => b64uEncode(new TextEncoder().encode(t));
  /** An independent HMAC, so the voucher is not checked against the code that makes it. */
  async function hmacB64u(key: string, data: string): Promise<string> {
    const k = await crypto.subtle.importKey("raw", new TextEncoder().encode(key), { name: "HMAC", hash: "SHA-256" }, false, ["sign"]);
    return b64uEncode(new Uint8Array(await crypto.subtle.sign("HMAC", k, new TextEncoder().encode(data))));
  }
  function assertion(token: string, userHandle: string | undefined) {
    const clientDataJSON = b64uText(JSON.stringify({ type: "webauthn.get", challenge: b64uText(token), origin: ORIGIN }));
    return { id: "cred", rawId: "Y3JlZA", type: "public-key", response: { clientDataJSON, authenticatorData: "AAAA", signature: "MEUC", ...(userHandle === undefined ? {} : { userHandle }) } };
  }
  const login = (e: Env, body: unknown, headers: Record<string, string> = {}) =>
    fetchVia(e, "/auth/passkey/login", { method: "POST", headers: { "content-type": "application/json", ...headers }, body: JSON.stringify(body) });

  it("POST /auth/passkey/challenge mints v1.<rand>.<exp>.<mac> with the rp id; it verifies until it expires", async () => {
    const { e, calls } = stubbed();
    const e2 = { ...e, PUBLIC_ORIGIN: ORIGIN } as Env;
    const r = await fetchVia(e2, "/auth/passkey/challenge", { method: "POST" });
    expect(r.status).toBe(200);
    expect(r.headers.get("cache-control")).toBe("no-store");
    const j = (await r.json()) as { challenge: string; rpId: string; userVerification: string; timeout: number };
    expect(j.rpId).toBe("leanmail.example");
    expect(j.userVerification).toBe("preferred");
    expect(j.timeout).toBe(PASSKEY_CHALLENGE_TTL_MS);
    expect(j.challenge).toMatch(/^v1\.[A-Za-z0-9_-]{24}\.\d+\.[A-Za-z0-9_-]{43}$/);
    expect(await verifyPasskeyChallenge(j.challenge, S)).toBe(true);
    expect(await verifyPasskeyChallenge(j.challenge, S, Date.now() + PASSKEY_CHALLENGE_TTL_MS + 1)).toBe(false);
    expect(await verifyPasskeyChallenge(j.challenge, "another-secret")).toBe(false);
    const parts = j.challenge.split(".");
    expect(await verifyPasskeyChallenge([parts[0], parts[1], String(Number(parts[2]) + 1), parts[3]].join("."), S)).toBe(false);
    expect(await verifyPasskeyChallenge(j.challenge + "x", S)).toBe(false);
    expect(calls).toEqual([]);
  });

  it("a challenge is not a magic link, and a magic link or cookie is not a challenge", async () => {
    const { e, calls } = stubbed();
    const ch = await mintPasskeyChallenge(S);
    expect((await fetchVia(e, `/auth/magic?t=${encodeURIComponent(ch)}`)).status).toBe(400);
    expect(calls).toEqual([]);
    const link = await signMac("v1", "a1", `rand.${Date.now() + 60_000}`, S);
    expect(await verifyPasskeyChallenge(link, S)).toBe(false);
    // a 4-part v1 token under an account's key is not one either
    expect(await verifyPasskeyChallenge(await signMac("v1", "a1", String(Date.now() + 60_000), S), S)).toBe(false);
  });

  it("POST /auth/passkey/login goes to the account userHandle names, with the challenge vouched for under that account's key", async () => {
    await activeAccount("a1", "alice@x.example");
    const { e, calls } = stubbed(() => Response.json({ ok: true }, { status: 200, headers: { "set-cookie": "lm_session=x" } }));
    const ch = await mintPasskeyChallenge(S);
    const body = assertion(ch, b64uText("a1"));
    const r = await login(e, body, { "x-passkey-challenge-ok": "forged", "cf-connecting-ip": "5.5.5.5" });
    expect(r.status).toBe(200);
    expect(r.headers.get("set-cookie")).toBe("lm_session=x");
    expect(calls).toHaveLength(1);
    expect(calls[0]).toMatchObject({ name: "a1", acct: "a1", method: "POST", path: "/auth/passkey/login" });
    expect(JSON.parse(calls[0]!.body)).toEqual(body);
    expect(calls[0]!.headers["x-passkey-challenge-ok"]).toBe(await hmacB64u(await deriveSecret(S, "a1"), `passkey:${ch}`));
  });

  it("refuses, without waking a container: unknown, pending and _operator handles, no handle, a bad or expired challenge", async () => {
    await activeAccount("a1", "alice@x.example");
    await activeAccount("p1", "pending@x.example", "pending");
    await activeAccount("_operator", "op@x.example");
    const { e, calls } = stubbed();
    const ch = await mintPasskeyChallenge(S);
    const old = await mintPasskeyChallenge(S, Date.now() - PASSKEY_CHALLENGE_TTL_MS - 1);
    const forged = (await mintPasskeyChallenge("not-the-secret"));
    const cases: unknown[] = [
      assertion(ch, b64uText("nobody")),
      assertion(ch, b64uText("p1")),
      assertion(ch, b64uText("_operator")),
      assertion(ch, b64uText("a1/../x")),
      assertion(ch, undefined),
      assertion(ch, "!!"),
      assertion(old, b64uText("a1")),
      assertion(forged, b64uText("a1")),
      assertion(await signMac("v1", "a1", `rand.${Date.now() + 60_000}`, S), b64uText("a1")),
      { response: { userHandle: b64uText("a1"), clientDataJSON: "bm90IGpzb24" } },
    ];
    for (const c of cases) {
      const r = await login(e, c);
      expect(r.status).toBe(401);
      expect(await r.json()).toEqual({ error: "passkey not accepted" });
    }
    expect(calls).toEqual([]);
  });
});

describe("M9b: the sign-up page", () => {
  it("signed out, GET and POST /signup go to the operator; with a cookie, to the account", async () => {
    const { e, calls } = stubbed();
    await fetchVia(e, "/signup", { headers: { accept: "text/html" } });
    await fetchVia(e, "/signup?sent=1", { headers: { accept: "text/html" } });
    await fetchVia(e, "/signup", { method: "POST", headers: { "content-type": "application/x-www-form-urlencoded" }, body: "email=a%40x.example" });
    const cookie = `lm_session=${await signMac("v1", "a1", "sid.9999999999999.0", S)}`;
    await fetchVia(e, "/signup", { headers: { cookie, accept: "text/html" } });
    expect(calls.map((c) => `${c.acct} ${c.method} ${c.path}${c.search}`)).toEqual([
      "_operator GET /signup",
      "_operator GET /signup?sent=1",
      "_operator POST /signup",
      "a1 GET /signup",
    ]);
  });

  it("the form POST /auth/signup reaches the operator as JSON and answers 303 by the outcome", async () => {
    let status = 202;
    const { e, calls } = stubbed(() => Response.json({ ok: status === 202 }, { status }));
    const post = () =>
      fetchVia(e, "/auth/signup", { method: "POST", headers: { "content-type": "application/x-www-form-urlencoded", "cf-connecting-ip": "203.0.113.7" }, body: "email=+new%40x.example+&note=hello+there" });
    const r = await post();
    expect(r.status).toBe(303);
    expect(r.headers.get("location")).toBe("/signup?sent=1");
    expect(calls[0]).toMatchObject({ acct: "_operator", method: "POST", path: "/auth/signup" });
    expect(calls[0]!.headers["content-type"]).toBe("application/json");
    expect(calls[0]!.headers["cf-connecting-ip"]).toBe("203.0.113.7");
    expect(JSON.parse(calls[0]!.body)).toEqual({ email: "new@x.example", note: "hello there" });
    const where: string[] = [];
    for (const s of [400, 429, 404, 500]) {
      status = s;
      where.push((await post()).headers.get("location") ?? "");
    }
    expect(where).toEqual(["/signup?error=email", "/signup?error=rate", "/signup", "/signup?error=failed"]);
  });
});
