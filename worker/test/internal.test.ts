import { SELF, createExecutionContext } from "cloudflare:test";
import { beforeEach, describe, expect, it } from "vitest";
import type { Env } from "../src/env";
import { dailyTick, handleFetch, mapLimit } from "../src/index";
import { handleInternal } from "../src/internal";
import { MailContainer } from "../src/container";
import { accountFromRequest, deriveInternalSecret, deriveSecret, signMac, signMacWithKey, verifyMac } from "../src/tokens";
import { SECRET, addDomain, addRoute, captureLogs, env, resetDb } from "./helpers";

beforeEach(resetDb);

/** What the container for `acct` holds as INTERNAL_SECRET. */
const derived = (acct: string) => deriveInternalSecret(SECRET, acct);

/** A call from `acct`'s container: its own derived secret unless `secret` says otherwise. */
async function req(method: string, path: string, body: unknown, acct: string | null, secret?: string | null): Promise<Request> {
  if (secret === undefined) secret = acct ? await derived(acct) : null;
  const headers: Record<string, string> = { "content-type": "application/json" };
  if (secret !== null) headers["x-internal-secret"] = secret;
  if (acct !== null) headers["x-account-id"] = acct;
  return new Request(`https://leanmail.example${path}`, { method, headers, body: body === undefined ? undefined : JSON.stringify(body) });
}

function b64(s: string): string {
  return btoa(s);
}

describe("internal auth", () => {
  it("401s without, or with a wrong, secret, and when INTERNAL_SECRET is unset", async () => {
    expect((await handleInternal(await req("PUT", "/internal/routes", {}, "a1", null), env)).status).toBe(401);
    expect((await handleInternal(await req("PUT", "/internal/routes", {}, "a1", "nope"), env)).status).toBe(401);
    expect((await handleInternal(await req("PUT", "/internal/routes", {}, null), env)).status).toBe(401);
    const noSecret = { ...env, INTERNAL_SECRET: undefined } as Env;
    expect((await handleInternal(await req("PUT", "/internal/routes", {}, "a1", ""), noSecret)).status).toBe(401);
    expect((await handleInternal(await req("PUT", "/internal/routes", {}, "a1"), noSecret)).status).toBe(401);
  });

  it("the master secret itself is not a container credential", async () => {
    expect((await handleInternal(await req("POST", "/internal/push", {}, "a1", SECRET), env)).status).toBe(401);
    expect((await handleInternal(await req("POST", "/internal/push", {}, "_operator", SECRET), env)).status).toBe(401);
  });

  it("a container's secret speaks only for its own account", async () => {
    const aSecret = await derived("a1");
    await addDomain("b.example", "b1");
    const bRoutes = { account_id: "b1", routes: [{ address: "x@b.example", mail: "vault", forward_to: [] }] };
    // A's secret claiming to be B: refused before anything runs.
    expect((await handleInternal(await req("PUT", "/internal/routes", bRoutes, "b1", aSecret), env)).status).toBe(401);
    // A's secret claiming to be the operator: refused.
    const accounts = { accounts: [{ id: "a1", email: "a@x.example", status: "active" }] };
    expect((await handleInternal(await req("PUT", "/internal/accounts", accounts, "_operator", aSecret), env)).status).toBe(401);
    expect((await handleInternal(await req("POST", "/internal/wake/b1", undefined, "_operator", aSecret), env, { wake: async () => {} })).status).toBe(401);
    expect((await env.DB.prepare("SELECT count(*) AS n FROM routes").first())!.n).toBe(0);
    expect((await env.DB.prepare("SELECT count(*) AS n FROM accounts").first())!.n).toBe(0);
    // A's secret as A: authenticated (and then authorised normally: 200 here).
    expect((await handleInternal(await req("POST", "/internal/push", {}, "a1", aSecret), env)).status).toBe(200);
  });

  it("the operator's derived secret opens the operator-only routes", async () => {
    const op = await derived("_operator");
    const accounts = { accounts: [{ id: "a1", email: "a@x.example", status: "active" }] };
    expect((await handleInternal(await req("PUT", "/internal/accounts", accounts, "_operator", op), env)).status).toBe(200);
    const woke: string[] = [];
    expect((await handleInternal(await req("POST", "/internal/wake/a1", undefined, "_operator", op), env, { wake: async (_e, a) => void woke.push(a) })).status).toBe(202);
    expect(woke).toEqual(["a1"]);
  });

  it("derives base64url(HMAC-SHA256(INTERNAL_SECRET, account)), and the DO never copies the master", async () => {
    const key = await crypto.subtle.importKey("raw", new TextEncoder().encode(SECRET), { name: "HMAC", hash: "SHA-256" }, false, ["sign"]);
    const mac = new Uint8Array(await crypto.subtle.sign("HMAC", key, new TextEncoder().encode("a1")));
    const b64u = btoa(String.fromCharCode(...mac)).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
    expect(await derived("a1")).toBe(b64u);
    expect(await derived("a1")).not.toBe(await derived("a2"));
    const vars = MailContainer.varsFor("a1", { ...env, INTERNAL_SECRET: SECRET });
    expect(vars.INTERNAL_SECRET).toBeUndefined();
    expect(Object.values(vars)).not.toContain(SECRET);
    expect(vars).toMatchObject({ ACCOUNT_ID: "a1", ROLE: "account" });
    expect(MailContainer.varsFor("_operator", env).ROLE).toBe("operator");
  });

  it("is reached from fetch() over the public origin", async () => {
    const r = await SELF.fetch("https://leanmail.example/internal/push", { method: "POST", headers: { "x-internal-secret": await derived("a1"), "x-account-id": "a1" }, body: "{}" });
    expect(r.status).toBe(200);
    expect(await r.json()).toEqual({ sent: 0, dead: [], failed: 0 });
    expect((await SELF.fetch("https://leanmail.example/internal/push", { method: "POST" })).status).toBe(401);
  });
});

describe("GET /internal/health", () => {
  it("proxies to the named account's container, or DEV_ACCOUNT on localhost, behind that account's secret", async () => {
    const seen: string[] = [];
    const MAIL = { idFromName: (n: string) => n, get: (n: string) => ({ proxy: async (r: Request, a: string) => (seen.push(`${n} ${a} ${new URL(r.url).pathname}`), Response.json({ ok: true })) }) };
    const e = { ...env, MAIL } as unknown as Env;
    const get = (url: string, h: Record<string, string>) => handleInternal(new Request(url, { headers: h }), e);
    const dev = await derived("dev");
    expect((await get("http://localhost:8787/internal/health", {})).status).toBe(401);
    expect((await get("http://localhost:8787/internal/health", { "x-internal-secret": SECRET })).status).toBe(401);
    expect((await get("http://localhost:8787/internal/health", { "x-internal-secret": await derived("a1") })).status).toBe(401);
    expect((await get("http://localhost:8787/internal/health", { "x-internal-secret": dev })).status).toBe(200);
    // DEV_ACCOUNT is not a fallback when PUBLIC_ORIGIN is not a localhost origin (production);
    // the request's own host does not matter (wrangler dev rewrites it to the route's domain).
    const prod = { ...e, PUBLIC_ORIGIN: "https://leanmail.example" } as unknown as Env;
    const getProd = (url: string, h: Record<string, string>) => handleInternal(new Request(url, { headers: h }), prod);
    expect((await getProd("http://localhost:8787/internal/health", { "x-internal-secret": dev })).status).toBe(401);
    expect((await getProd("https://leanmail.example/internal/health", { "x-internal-secret": dev, "x-account-id": "a1" })).status).toBe(401);
    expect((await getProd("https://leanmail.example/internal/health", { "x-internal-secret": await derived("a1"), "x-account-id": "a1" })).status).toBe(200);
    expect(seen).toEqual(["dev dev /internal/health", "a1 a1 /internal/health"]);
  });
});

describe("POST /internal/email/send", () => {
  function stubEmail() {
    const sent: { from: string; to: string }[] = [];
    const e = { ...env, EMAIL: { send: async (m: EmailMessage) => void sent.push({ from: m.from, to: m.to }) } } as unknown as Env;
    return { e, sent };
  }

  it("sends once per recipient as an address the account may send from", async () => {
    await addRoute("me@ex.example", "a1", "vault", [], 1);
    const { e, sent } = stubEmail();
    const raw = "From: me@ex.example\r\nTo: x@y.example\r\nSubject: t\r\n\r\nhi\r\n";
    const r = await handleInternal(await req("POST", "/internal/email/send", { from: "me@ex.example", to: ["x@y.example", "z@y.example"], raw_b64: b64(raw) }, "a1"), e);
    expect(r.status).toBe(200);
    expect(await r.json()).toEqual({ ok: true, sent: 2 });
    expect(sent).toEqual([
      { from: "me@ex.example", to: "x@y.example" },
      { from: "me@ex.example", to: "z@y.example" },
    ]);
  });

  it("refuses a From the caller does not own or may not send from", async () => {
    await addRoute("me@ex.example", "a1", "vault", [], 1);
    await addRoute("ro@ex.example", "a1", "vault", [], 0);
    const { e, sent } = stubEmail();
    const body = (from: string) => ({ from, to: ["x@y.example"], raw_b64: b64(`From: Me <${from}>\r\n\r\nx`) });
    expect((await handleInternal(await req("POST", "/internal/email/send", body("me@ex.example"), "a2"), e)).status).toBe(403);
    expect((await handleInternal(await req("POST", "/internal/email/send", body("ro@ex.example"), "a1"), e)).status).toBe(403);
    expect((await handleInternal(await req("POST", "/internal/email/send", body("me@ex.example"), "_operator"), e)).status).toBe(200);
    expect((await handleInternal(await req("POST", "/internal/email/send", { from: "me@ex.example", to: [], raw_b64: "x" }, "a1"), e)).status).toBe(400);
    expect(sent).toHaveLength(1);
  });
});

describe("PUT /internal/routes", () => {
  const routes = (address: string) => ({ account_id: "a1", routes: [{ address, mail: "vault+forward", forward_to: ["Out@Else.example"], can_send: true }] });

  it("accepts routes on the account's own domains", async () => {
    await addDomain("ex.example", "a1");
    const r = await handleInternal(await req("PUT", "/internal/routes", routes("Me@Ex.example"), "a1"), env);
    expect(r.status).toBe(200);
    expect(await env.DB.prepare("SELECT address, account_id, mail, forward_to, can_send FROM routes").first()).toEqual({
      address: "me@ex.example",
      account_id: "a1",
      mail: "vault+forward",
      forward_to: '["out@else.example"]',
      can_send: 1,
    });
    expect((await handleInternal(await req("PUT", "/internal/routes", routes("*@ex.example"), "a1"), env)).status).toBe(200);
  });

  it("refuses another account's domain, another account's id, and bad input; the operator may do either", async () => {
    await addDomain("ex.example", "a1");
    await addDomain("theirs.example", "a2");
    const r = await handleInternal(await req("PUT", "/internal/routes", routes("x@theirs.example"), "a1"), env);
    expect(r.status).toBe(403);
    expect(await r.json()).toEqual({ error: "domain_not_granted", domains: ["theirs.example"] });
    expect((await handleInternal(await req("PUT", "/internal/routes", routes("x@unknown.example"), "a1"), env)).status).toBe(403);
    expect((await handleInternal(await req("PUT", "/internal/routes", routes("x@ex.example"), "a2"), env)).status).toBe(403);
    const bad = { account_id: "a1", routes: [{ address: "x@ex.example", mail: "keep", forward_to: [] }] };
    expect((await handleInternal(await req("PUT", "/internal/routes", bad, "a1"), env)).status).toBe(400);
    expect((await env.DB.prepare("SELECT count(*) AS n FROM routes").first())!.n).toBe(0);
    expect((await handleInternal(await req("PUT", "/internal/routes", routes("x@theirs.example"), "_operator"), env)).status).toBe(200);
  });
});

describe("PUT /internal/accounts and POST /internal/wake/:acct", () => {
  it("are operator only", async () => {
    const body = { accounts: [{ id: "a1", email: "One@x.example", status: "active", has_gmail: true }], domains: [{ name: "Ex.example", account_id: "a1" }] };
    expect((await handleInternal(await req("PUT", "/internal/accounts", body, "a1"), env)).status).toBe(403);
    expect((await handleInternal(await req("PUT", "/internal/accounts", body, "_operator"), env)).status).toBe(200);
    expect(await env.DB.prepare("SELECT id, email, status, has_gmail FROM accounts").first()).toEqual({ id: "a1", email: "one@x.example", status: "active", has_gmail: 1 });
    expect(await env.DB.prepare("SELECT name, account_id FROM domains").first()).toEqual({ name: "ex.example", account_id: "a1" });

    const woke: string[] = [];
    const wake = async (_e: Env, acct: string) => void woke.push(acct);
    expect((await handleInternal(await req("POST", "/internal/wake/a1", undefined, "a1"), env, { wake })).status).toBe(403);
    expect((await handleInternal(await req("POST", "/internal/wake/a1", undefined, "_operator"), env, { wake })).status).toBe(202);
    expect(woke).toEqual(["a1"]);
  });
});

describe("tokens", () => {
  const S = "test-session-secret";
  it("verifyMac round-trips v1 and lm1 tokens and rejects tampering", async () => {
    const session = await signMac("v1", "acct_1", "eyJleHAiOjF9.extra", S);
    expect(await verifyMac(session, S)).toEqual({ prefix: "v1", acct: "acct_1", body: "eyJleHAiOjF9.extra" });
    const api = await signMac("lm1", "acct-2", "r4nd0m", S);
    expect(await verifyMac(api, S)).toMatchObject({ acct: "acct-2", body: "r4nd0m" });

    expect(await verifyMac(api, "other-secret")).toBeNull();
    expect(await verifyMac(api.replace("acct-2", "acct-3"), S)).toBeNull();
    expect(await verifyMac(api.slice(0, -2) + (api.endsWith("A") ? "BB" : "AA"), S)).toBeNull();
    expect(await verifyMac(api, S, ["v1"])).toBeNull();
    expect(await verifyMac("v1.a.b", S)).toBeNull();
    expect(await verifyMac("v1.bad acct.x.y", S)).toBeNull();
    expect(await verifyMac(api, undefined)).toBeNull();
  });

  it("verifies under the key derived for the account the token names, never the master", async () => {
    const enc = new TextEncoder();
    const hmacB64u = async (key: string, data: string) => {
      const k = await crypto.subtle.importKey("raw", enc.encode(key), { name: "HMAC", hash: "SHA-256" }, false, ["sign"]);
      const mac = new Uint8Array(await crypto.subtle.sign("HMAC", k, enc.encode(data)));
      return btoa(String.fromCharCode(...mac)).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
    };
    // K_a1 = base64url(HMAC(master, "a1")), used as a UTF-8 string key, exactly as the container does.
    const kA = await hmacB64u(S, "a1");
    expect(await deriveSecret(S, "a1")).toBe(kA);
    expect(await signMac("lm1", "a1", "xyz", S)).toBe(`lm1.a1.xyz.${await hmacB64u(kA, "lm1.a1.xyz")}`);

    // Container A mints with its own key.
    const own = await signMacWithKey("lm1", "a1", "r", kA);
    expect(await verifyMac(own, S)).toMatchObject({ acct: "a1" });
    // Container A mints a token naming B: refused.
    const forged = await signMacWithKey("lm1", "b1", "r", kA);
    expect(await verifyMac(forged, S)).toBeNull();
    const forgedCookie = await signMacWithKey("v1", "b1", "sid.1.0", kA);
    expect(await accountFromRequest(new Request("https://x.example/", { headers: { cookie: `lm_session=${forgedCookie}` } }), S)).toBeNull();
    // A token under the master itself: refused, for any account.
    expect(await verifyMac(await signMacWithKey("lm1", "a1", "r", S), S)).toBeNull();
    expect(await verifyMac(await signMacWithKey("v1", "_operator", "r.1", S), S)).toBeNull();
  });

  it("the DO's env carries neither master secret", () => {
    const vars = MailContainer.varsFor("a1", { ...env, INTERNAL_SECRET: SECRET, SESSION_SECRET: S, MAIL_FROM: "no-reply@x.example" });
    expect(vars.SESSION_SECRET).toBeUndefined();
    expect(vars.INTERNAL_SECRET).toBeUndefined();
    expect(Object.values(vars)).not.toContain(S);
    expect(vars.MAIL_FROM).toBe("no-reply@x.example");
  });

  it("passes the AI and log settings through when set, and nothing when not; the Cloudflare token to _operator only", () => {
    const base = { ...env, CF_API_TOKEN: undefined, CF_ACCOUNT_ID: undefined, AI_BASE_URL: undefined, AI_API_KEY: undefined,
      AI_MODEL: undefined, LOG_LEVEL: undefined, DEV_ACCOUNT: undefined };
    const off = MailContainer.varsFor("a1", base);
    for (const k of ["CF_API_TOKEN", "CF_ACCOUNT_ID", "AI_BASE_URL", "AI_API_KEY", "AI_MODEL", "LOG_LEVEL", "DEV_AUTH_BYPASS"]) {
      expect(off[k]).toBeUndefined();
    }
    const on = MailContainer.varsFor("a1", { ...base, CF_API_TOKEN: "cft", CF_ACCOUNT_ID: "cfa", AI_BASE_URL: "https://ai.example/v1",
      AI_API_KEY: "aik", AI_MODEL: "m", LOG_LEVEL: "debug" });
    expect(on).toMatchObject({ AI_BASE_URL: "https://ai.example/v1", AI_API_KEY: "aik", AI_MODEL: "m", LOG_LEVEL: "debug" });
    expect(on.CF_API_TOKEN).toBeUndefined();
    expect(on.CF_ACCOUNT_ID).toBeUndefined();
    expect(Object.values(on)).not.toContain("cft");
    expect(on.DEV_AUTH_BYPASS).toBeUndefined();
    const op = MailContainer.varsFor("_operator", { ...base, CF_API_TOKEN: "cft", CF_ACCOUNT_ID: "cfa" });
    expect(op).toMatchObject({ ROLE: "operator", CF_API_TOKEN: "cft", CF_ACCOUNT_ID: "cfa" });
  });

  it("asks for DEV_AUTH_BYPASS only when DEV_ACCOUNT is set", () => {
    const base = { ...env, DEV_ACCOUNT: undefined };
    expect(MailContainer.varsFor("dev", base).DEV_AUTH_BYPASS).toBeUndefined();
    expect(MailContainer.varsFor("dev", { ...base, DEV_ACCOUNT: "dev" }).DEV_AUTH_BYPASS).toBe("1");
  });

  it("tests run on the vitest bindings, not .dev.vars", () => {
    expect(env.INTERNAL_SECRET).toBe(SECRET);
    expect(env.SESSION_SECRET).toBe(S);
  });

  it("accountFromRequest reads the cookie, then the bearer", async () => {
    const cookie = await signMac("v1", "c1", "p", S);
    const bearer = await signMac("lm1", "b1", "r", S);
    const r = (h: Record<string, string>) => accountFromRequest(new Request("https://x.example/", { headers: h }), S);
    expect(await r({ cookie: `a=b; lm_session=${cookie}` })).toBe("c1");
    expect(await r({ authorization: `Bearer ${bearer}` })).toBe("b1");
    expect(await r({ authorization: `Bearer ${cookie}` })).toBeNull();
    expect(await r({})).toBeNull();
  });
});

describe("fetch()", () => {
  it("serves /health, and 401s an unauthenticated request when PUBLIC_ORIGIN is not local even with DEV_ACCOUNT set", async () => {
    expect(env.DEV_ACCOUNT).toBe("dev");
    const h = await SELF.fetch("https://leanmail.example/health");
    expect(await h.json()).toMatchObject({ ok: true, worker: "leanmail" });
    // The dev fallback is keyed on PUBLIC_ORIGIN, not the request host: production (an https
    // origin) never falls back, whatever host the request carries.
    const prod = { ...env, PUBLIC_ORIGIN: "https://leanmail.example" } as unknown as Env;
    const r = await handleFetch(new Request("http://localhost:8787/api/msgs"), prod, createExecutionContext());
    expect(r.status).toBe(401);
    expect(await r.json()).toEqual({ error: "unauthorized" });
  });
});

describe("scheduled", () => {
  it("ticks every active account, at most five at once, logging failures", async () => {
    const ids = Array.from({ length: 12 }, (_, i) => `a${String(i).padStart(2, "0")}`);
    await env.DB.batch([
      ...ids.map((id) => env.DB.prepare("INSERT INTO accounts (id, email, status, updated_at) VALUES (?1, ?2, 'active', 0)").bind(id, `${id}@x.example`)),
      env.DB.prepare("INSERT INTO accounts (id, email, status, updated_at) VALUES ('p1', 'p1@x.example', 'pending', 0)"),
    ]);
    let inFlight = 0;
    let peak = 0;
    const ticked: string[] = [];
    const tick = async (_e: Env, acct: string) => {
      inFlight++;
      peak = Math.max(peak, inFlight);
      await new Promise((r) => setTimeout(r, 5));
      inFlight--;
      ticked.push(acct);
      if (acct === "a03") throw new Error("boom");
    };
    const { result, lines } = await captureLogs(() => dailyTick(env, tick));
    expect(result).toEqual({ ok: 11, failed: 1 });
    expect(ticked.sort()).toEqual(ids);
    expect(peak).toBe(5);
    expect(lines.find((l) => l.ev === "cron.tick_failed")).toMatchObject({ acct: "a03" });
  });

  it("mapLimit handles empty input", async () => {
    await mapLimit([], 5, async () => {});
  });
});
