import { createExecutionContext, waitOnExecutionContext } from "cloudflare:test";
import { beforeEach, describe, expect, it } from "vitest";
import { AUTH_RATE_PER_HOUR, rateLimited } from "../src/auth";
import {
  DOCKER_HOST,
  MailContainer,
  RETRY_BODY_MAX,
  retryable,
  sendRetryingOnce,
  stampHeaders,
  workerOriginFor,
  type StampOptions,
} from "../src/container";
import { FORWARD_BREAKER_PER_HOUR, handleEmail } from "../src/email";
import type { Env } from "../src/env";
import { handleGmailHook } from "../src/gmail";
import { handleFetch } from "../src/index";
import { SIGNIN_MAIL_PER_HOUR, handleInternal, headerFrom, mailFrom, signinMessage } from "../src/internal";
import { canonicalPath, firstSegment } from "../src/paths";
import { ipv6Net64 } from "../src/rate";
import { deriveSecret, signMac } from "../src/tokens";
import { callContainer } from "../src/wake";
import { SECRET, addDomain, addRoute, captureLogs, env, fakeMessage, resetDb } from "./helpers";

const S = "test-session-secret";
const ORIGIN = "https://leanmail.example";

beforeEach(resetDb);

async function internalReq(method: string, path: string, body: unknown, acct: string, raw?: string): Promise<Request> {
  return new Request(ORIGIN + path, {
    method,
    headers: { "content-type": "application/json", "x-account-id": acct, "x-internal-secret": await deriveSecret(SECRET, acct) },
    body: raw ?? (body === undefined ? undefined : JSON.stringify(body)),
  });
}

interface Proxied {
  name: string;
  acct: string;
  path: string;
  headers: Record<string, string>;
  opts: StampOptions;
}

function stubbed(reply: () => Response = () => Response.json({ ok: true })) {
  const calls: Proxied[] = [];
  const MAIL = {
    idFromName: (n: string) => n,
    get: (name: string) => ({
      proxy: async (r: Request, acct: string, opts: StampOptions = {}) => {
        calls.push({ name, acct, path: new URL(r.url).pathname, headers: Object.fromEntries(r.headers), opts });
        await r.body?.cancel();
        return reply();
      },
    }),
  };
  return { e: { ...env, MAIL, PUBLIC_ORIGIN: ORIGIN } as unknown as Env, calls };
}

async function fetchVia(e: Env, url: string, init: RequestInit = {}): Promise<Response> {
  const ctx = createExecutionContext();
  const res = await handleFetch(new Request(url.startsWith("http") ? url : ORIGIN + url, init), e, ctx);
  await waitOnExecutionContext(ctx);
  return res;
}

const cookieFor = async (acct: string) => `lm_session=${await signMac("v1", acct, "sid.9999999999999.0", S)}`;

// ---- 1. the Cloudflare token stays with the operator; grants; routing forwards ----

describe("1: Cloudflare credentials, /internal/grants, /internal/routing/*", () => {
  it("varsFor gives CF_API_TOKEN and CF_ACCOUNT_ID to _operator only", () => {
    const e = { ...env, CF_API_TOKEN: "tok", CF_ACCOUNT_ID: "acc" };
    for (const acct of ["a1", "dev", "operator", "_operator2"]) {
      const v = MailContainer.varsFor(acct, e);
      expect(v.CF_API_TOKEN).toBeUndefined();
      expect(v.CF_ACCOUNT_ID).toBeUndefined();
    }
    expect(MailContainer.varsFor("_operator", e)).toMatchObject({ CF_API_TOKEN: "tok", CF_ACCOUNT_ID: "acc" });
  });

  it("GET /internal/grants answers the caller's own domains, sorted, behind its secret", async () => {
    await addDomain("zeta.example", "a1");
    await addDomain("alpha.example", "a1");
    await addDomain("theirs.example", "a2");
    const r = await handleInternal(await internalReq("GET", "/internal/grants", undefined, "a1"), env);
    expect(r.status).toBe(200);
    expect(await r.json()).toEqual({ domains: ["alpha.example", "zeta.example"] });
    expect(await (await handleInternal(await internalReq("GET", "/internal/grants", undefined, "a3"), env)).json()).toEqual({ domains: [] });
    const anon = new Request(ORIGIN + "/internal/grants", { headers: { "x-account-id": "a1", "x-internal-secret": await deriveSecret(SECRET, "a2") } });
    expect((await handleInternal(anon, env)).status).toBe(401);
  });

  it("POST /internal/routing/{plan,apply} go to the operator with the caller's body and x-routing-account", async () => {
    const seen: { acct: string; path: string; body: unknown; opts: unknown }[] = [];
    const call = (async (_e: Env, acct: string, path: string, body?: unknown, opts?: unknown) => {
      seen.push({ acct, path, body, opts });
      return Response.json({ plan: [], for: (opts as { routingAccount?: string }).routingAccount }, { status: path.endsWith("apply") ? 202 : 200 });
    }) as typeof callContainer;
    const plan = { domain: "ex.example", addresses: [{ address: "me@ex.example", mail: "vault" }] };
    const r1 = await handleInternal(await internalReq("POST", "/internal/routing/plan", plan, "a1"), env, { call });
    expect(r1.status).toBe(200);
    expect(await r1.json()).toEqual({ plan: [], for: "a1" });
    const r2 = await handleInternal(await internalReq("POST", "/internal/routing/apply", { domain: "ex.example" }, "a2"), env, { call });
    expect(r2.status).toBe(202);
    expect(seen).toEqual([
      { acct: "_operator", path: "/internal/routing/plan", body: plan, opts: { routingAccount: "a1" } },
      { acct: "_operator", path: "/internal/routing/apply", body: { domain: "ex.example" }, opts: { routingAccount: "a2" } },
    ]);
    expect((await handleInternal(await internalReq("POST", "/internal/routing/plan", undefined, "a1", "not json"), env, { call })).status).toBe(400);
    expect((await handleInternal(await internalReq("GET", "/internal/routing/plan", undefined, "a1"), env, { call })).status).toBe(404);
    expect((await handleInternal(await internalReq("POST", "/internal/routing/delete", {}, "a1"), env, { call })).status).toBe(404);
    expect(seen).toHaveLength(2);
  });

  it("callContainer is an internal call, and carries the routing account to the DO", async () => {
    const { e, calls } = stubbed();
    await callContainer(e, "_operator", "/internal/routing/plan", { x: 1 }, { routingAccount: "a1" });
    await callContainer(e, "a1", "/internal/wake");
    expect(calls.map((c) => [c.name, c.path, c.opts])).toEqual([
      ["_operator", "/internal/routing/plan", { internal: true, routingAccount: "a1" }],
      ["a1", "/internal/wake", { internal: true, routingAccount: undefined }],
    ]);
  });

  it("stampHeaders sets x-routing-account only on an internal call, never from the client", () => {
    const client = new Headers({ "x-routing-account": "victim", "x-lm-public": "0" });
    expect(Object.fromEntries(stampHeaders(client, "_operator", "op"))).toEqual({ "x-account-id": "_operator", "x-internal-secret": "op", "x-lm-public": "1" });
    expect(Object.fromEntries(stampHeaders(client, "_operator", "op", { routingAccount: "a1" }))).toEqual({ "x-account-id": "_operator", "x-internal-secret": "op", "x-lm-public": "1" });
    expect(Object.fromEntries(stampHeaders(client, "_operator", "op", { internal: true, routingAccount: "a1" }))).toEqual({
      "x-account-id": "_operator",
      "x-internal-secret": "op",
      "x-routing-account": "a1",
    });
  });
});

// ---- 2. the container's /internal/* is never reachable from the public proxy ----

/** A MailContainer's `proxy` run on a stand-in `this`, recording what reaches containerFetch. */
function fakeContainer(replies: (() => Response | Promise<Response>)[] = []) {
  const sent: { path: string; headers: Record<string, string>; body: string }[] = [];
  const self = {
    internalSecret: "derived-a1",
    defaultPort: 8080,
    async ensureStarted() {},
    async containerFetch(r: Request) {
      sent.push({ path: new URL(r.url).pathname, headers: Object.fromEntries(r.headers), body: await r.text() });
      const next = replies.shift();
      return next ? next() : Response.json({ ok: true });
    },
    deleteSchedules() {},
    async schedule() {},
  };
  const proxy = (r: Request, acct = "a1", opts: StampOptions = {}) =>
    (MailContainer.prototype.proxy as (this: unknown, r: Request, a: string, o?: StampOptions) => Promise<Response>).call(self, r, acct, opts);
  return { proxy, sent };
}

describe("2: x-lm-public and path canonicalisation", () => {
  it("canonicalPath decodes once, collapses slashes, resolves dot segments, and refuses the rest", () => {
    expect(canonicalPath("/")).toBe("/");
    expect(canonicalPath("/inbox")).toBe("/inbox");
    expect(canonicalPath("//inbox/")).toBe("/inbox");
    expect(canonicalPath("/%69nternal/health")).toBe("/internal/health");
    expect(canonicalPath("//internal/tick")).toBe("/internal/tick");
    expect(canonicalPath("/internal/../internal/x")).toBe("/internal/x");
    expect(canonicalPath("/a/./b/../../internal/x")).toBe("/internal/x");
    expect(canonicalPath("/a/%2e%2e/internal")).toBe("/internal");
    expect(canonicalPath("/settings/routing/addresses/me%2Btag%40ex.example/delete")).toBe("/settings/routing/addresses/me+tag@ex.example/delete");
    expect(canonicalPath("/api/config/addresses/*@ex.example")).toBe("/api/config/addresses/*@ex.example");
    for (const bad of ["/%E0%A4%A", "/inbox/%ZZ", "/%2Finternal/x", "/a%2Fb", "/x/%5Cinternal", "/x/%2569nternal", "/x/a%20b", "/x/%00", "/x/%C3%A9", "/x/%3F", "/%40/x", "/in%2Bbox"]) {
      expect(canonicalPath(bad), bad).toBeNull();
    }
    expect(firstSegment("/Internal/x")).toBe("internal");
    expect(firstSegment("/")).toBe("");
  });

  it("fetch() 404s client paths that only canonicalise to /internal/*, before any container", async () => {
    const { e, calls } = stubbed();
    const cookie = await cookieFor("a1");
    const op = await deriveSecret(SECRET, "_operator");
    for (const p of ["/%69nternal/health", "//internal/tick", "/x/../%69nternal/tick", "/INTERNAL/tick", "/Internal", "/%2Finternal/tick", "/inbox/%E0%A4%A"]) {
      for (const headers of [{ cookie, accept: "application/json" }, { "x-account-id": "_operator", "x-internal-secret": op }] as Record<string, string>[]) {
        const r = await fetchVia(e, p, { method: "POST", headers });
        expect(r.status, p).toBe(404);
      }
    }
    // `..` is resolved by URL parsing already, so this is /internal/x itself: the Worker's own
    // handler, which wants a container secret and knows no such route.
    expect((await fetchVia(e, "/internal/../internal/x", { headers: { cookie } })).status).toBe(401);
    expect((await fetchVia(e, "/%2e/internal/x", { headers: { cookie } })).status).toBe(401);
    expect((await fetchVia(e, "/internal/../internal/x", { headers: { "x-account-id": "_operator", "x-internal-secret": op } })).status).toBe(404);
    expect(calls).toEqual([]);
  });

  it("fetch() routes, and forwards, the canonical path", async () => {
    const { e, calls } = stubbed();
    const cookie = await cookieFor("a1");
    await fetchVia(e, "//inbox/", { headers: { cookie } });
    await fetchVia(e, "/settings/routing/addresses/me%40ex.example/delete", { method: "POST", headers: { cookie } });
    await fetchVia(e, "/x/../static/app.css");
    await fetchVia(e, "//health");
    expect(calls.map((c) => `${c.acct} ${c.path}`)).toEqual(["a1 /inbox", "a1 /settings/routing/addresses/me@ex.example/delete", "_operator /static/app.css"]);
    expect(calls.every((c) => !c.opts.internal)).toBe(true);
  });

  it("MailContainer.proxy marks client requests public, strips a client's mark, and refuses /internal/* for them", async () => {
    const { proxy, sent } = fakeContainer();
    await proxy(new Request("http://c/inbox", { headers: { "x-lm-public": "0", "x-internal-secret": "guess" } }));
    await proxy(new Request("http://c/internal/tick", { method: "POST", body: "{}" }), "a1", { internal: true });
    for (const p of ["/internal/tick", "/%69nternal/tick", "//internal/tick", "/Internal/x", "/%ZZ"]) {
      const r = await proxy(new Request(`http://c${p}`, { method: "POST" }));
      expect(r.status, p).toBe(404);
    }
    expect(sent.map((s) => [s.path, s.headers["x-lm-public"] ?? "-", s.headers["x-internal-secret"]])).toEqual([
      ["/inbox", "1", "derived-a1"],
      ["/internal/tick", "-", "derived-a1"],
    ]);
  });

  it("the health check and the scheduled wake are internal calls", async () => {
    const opts: StampOptions[] = [];
    const MAIL = { idFromName: (n: string) => n, get: () => ({ proxy: async (_r: Request, _a: string, o: StampOptions = {}) => (opts.push(o), Response.json({ ok: true })) }) };
    const e = { ...env, MAIL } as unknown as Env;
    const h = new Request(ORIGIN + "/internal/health", { headers: { "x-account-id": "a1", "x-internal-secret": await deriveSecret(SECRET, "a1") } });
    expect((await handleInternal(h, e)).status).toBe(200);
    expect(opts).toEqual([{ internal: true }]);

    const seen: StampOptions[] = [];
    const self = { ctx: { id: { name: "a1" } }, proxy: async (_r: Request, _a: string, o: StampOptions) => (seen.push(o), new Response(null)) };
    await (MailContainer.prototype.wake as (this: unknown) => Promise<void>).call(self);
    expect(seen).toEqual([{ internal: true }]);
  });
});

// ---- 3. sign-in mail is written by the Worker; From: must match ----

describe("3: /internal/email/signin and the From: header", () => {
  function stubEmail(fail = false) {
    const sent: { from: string; to: string }[] = [];
    const e = {
      ...env,
      PUBLIC_ORIGIN: ORIGIN,
      EMAIL: {
        send: async (m: EmailMessage) => {
          if (fail) throw new Error("refused");
          sent.push({ from: m.from, to: m.to });
        },
      },
    } as unknown as Env;
    return { e, sent };
  }
  const link = async (acct = "a1", origin = ORIGIN) => `${origin}/auth/magic?t=${encodeURIComponent(await signMac("v1", acct, `rand.${Date.now() + 600_000}`, S))}`;
  const account = (id: string, email: string) => env.DB.prepare("INSERT INTO accounts (id, email, status, updated_at) VALUES (?1, ?2, 'active', 0)").bind(id, email).run();

  it("headerFrom reads the one From: of the header block", () => {
    const h = (s: string) => headerFrom(new TextEncoder().encode(s));
    expect(h("From: me@ex.example\r\n\r\nbody")).toBe("me@ex.example");
    expect(h('from: "A <b@c.example>" <Me@Ex.example>\r\nTo: x@y.example\r\n\r\n')).toBe("me@ex.example");
    expect(h("Subject: x\r\nFrom: Me\r\n <me@ex.example>\r\n\r\n")).toBe("me@ex.example");
    expect(h("From: me@ex.example\n\nFrom: other@ex.example")).toBe("me@ex.example");
    expect(h("Subject: x\r\n\r\nFrom: me@ex.example")).toBeNull();
    expect(h("From: a@ex.example\r\nFrom: b@ex.example\r\n\r\n")).toBeNull();
    expect(h("From: a@ex.example, b@ex.example\r\n\r\n")).toBeNull();
    expect(h("x")).toBeNull();
  });

  it("/internal/email/send needs the raw's From: to be the envelope from, for every caller", async () => {
    await addRoute("me@ex.example", "a1", "vault", [], 1);
    const { e, sent } = stubEmail();
    const go = async (raw: string, acct = "a1") => {
      const r = await handleInternal(await internalReq("POST", "/internal/email/send", { from: "me@ex.example", to: ["x@y.example"], raw_b64: btoa(raw) }, acct), e);
      return [r.status, ((await r.json()) as { error?: string }).error ?? "ok"];
    };
    expect(await go("From: Me <me@ex.example>\r\nSubject: s\r\n\r\nhi")).toEqual([200, "ok"]);
    expect(await go("From: boss@ex.example\r\n\r\nhi")).toEqual([403, "header_from_mismatch"]);
    expect(await go("From: boss@ex.example\r\n\r\nhi", "_operator")).toEqual([403, "header_from_mismatch"]);
    expect(await go("Subject: none\r\n\r\nhi")).toEqual([400, "bad_header_from"]);
    expect(await go("From: me@ex.example\r\nFrom: boss@ex.example\r\n\r\nhi")).toEqual([400, "bad_header_from"]);
    expect(sent).toHaveLength(1);
  });

  it("sends the Worker's own sign-in mail to the caller's account email, from MAIL_FROM", async () => {
    await account("a1", "alice@x.example");
    const { e, sent } = stubEmail();
    // worker/.dev.vars' MAIL_FROM passes the address check (item 9).
    expect(env.MAIL_FROM).toBe("no-reply@localhost.test");
    expect(mailFrom(e)).toBe("no-reply@localhost.test");
    const body = { to: "Alice@X.example", kind: "signin", link: await link(), code: "123456" };
    const { result: r, lines } = await captureLogs(async () => handleInternal(await internalReq("POST", "/internal/email/signin", body, "a1"), e));
    expect(r.status).toBe(200);
    expect(await r.json()).toEqual({ ok: true });
    expect(sent).toEqual([{ from: "no-reply@localhost.test", to: "alice@x.example" }]);
    expect(lines.find((l) => l.ev === "internal.email_signin")).toEqual({ ev: "internal.email_signin", acct: "a1", kind: "signin" });
    const welcome = await handleInternal(await internalReq("POST", "/internal/email/signin", { ...body, kind: "welcome" }, "a1"), e);
    expect(welcome.status).toBe(200);
  });

  it("refuses another address, a foreign or malformed link, a bad code or kind", async () => {
    await account("a1", "alice@x.example");
    await account("a2", "bob@x.example");
    const { e, sent } = stubEmail();
    const ok = { to: "alice@x.example", kind: "signin", link: await link(), code: "123456" };
    const cases: [Record<string, unknown>, number, string][] = [
      [{ ...ok, to: "bob@x.example" }, 403, "not_your_email"],
      [{ ...ok, to: "evil@else.example" }, 403, "not_your_email"],
      [{ ...ok, link: await link("a1", "https://evil.example") }, 400, "bad_link"],
      [{ ...ok, link: `${ORIGIN}/auth/magic?t=x&next=https://evil.example` }, 400, "bad_link"],
      [{ ...ok, link: (await link()) + "\r\nBcc: x@y.example" }, 400, "bad_link"],
      [{ ...ok, link: await link("a2") }, 400, "bad_link"],
      [{ ...ok, link: `${ORIGIN}/auth/magic?t=${encodeURIComponent(await signMac("lm1", "a1", "r", S))}` }, 400, "bad_link"],
      [{ ...ok, link: `${ORIGIN}/auth/magic?t=%E0%A4%A` }, 400, "bad_link"],
      [{ ...ok, code: "12345" }, 400, "bad_code"],
      [{ ...ok, code: "12345a" }, 400, "bad_code"],
      [{ ...ok, kind: "reset" }, 400, "bad_kind"],
      [{ ...ok, to: "not an address" }, 400, "bad_to"],
    ];
    for (const [body, status, error] of cases) {
      const r = await handleInternal(await internalReq("POST", "/internal/email/signin", body, "a1"), e);
      expect([r.status, ((await r.json()) as { error: string }).error], JSON.stringify(body)).toEqual([status, error]);
    }
    // The account the email names is the caller's, not one it claims.
    expect((await handleInternal(await internalReq("POST", "/internal/email/signin", { ...ok, link: await link("a2"), to: "bob@x.example" }, "a1"), e)).status).toBe(400);
    expect(sent).toEqual([]);
  });

  it("is limited to 20 an hour per account, and a send failure is a 502", async () => {
    await account("a1", "alice@x.example");
    await account("a2", "bob@x.example");
    const { e, sent } = stubEmail();
    const body = async (acct: string, to: string) => ({ to, kind: "signin", link: await link(acct), code: "000000" });
    const t0 = 1_761_900_000_000;
    const post = async (acct: string, to: string, now = t0) => handleInternal(await internalReq("POST", "/internal/email/signin", await body(acct, to), acct), e, { now: () => now });
    for (let i = 0; i < SIGNIN_MAIL_PER_HOUR; i++) expect((await post("a1", "alice@x.example")).status).toBe(200);
    const r = await post("a1", "alice@x.example");
    expect(r.status).toBe(429);
    expect(r.headers.get("retry-after")).toBe("3600");
    expect((await post("a2", "bob@x.example")).status).toBe(200);
    expect((await post("a1", "alice@x.example", t0 + 3_600_000)).status).toBe(200);
    expect(sent).toHaveLength(SIGNIN_MAIL_PER_HOUR + 2);
    const { e: failing } = stubEmail(true);
    expect((await handleInternal(await internalReq("POST", "/internal/email/signin", await body("a2", "bob@x.example"), "a2"), failing)).status).toBe(502);
  });

  it("signinMessage writes a plain-text message with From, To, Subject, Date and Message-ID", () => {
    const now = Date.UTC(2026, 10, 1, 7, 0, 0);
    const msg = signinMessage({ from: "no-reply@leanmail.io", to: "a@x.example", kind: "signin", link: `${ORIGIN}/auth/magic?t=v1.a1.r.1.m`, code: "123456", now, id: "01JTEST" });
    const cut = msg.indexOf("\r\n\r\n");
    const head = msg.slice(0, cut);
    const text = msg.slice(cut + 4);
    expect(head.split("\r\n")).toEqual([
      "From: LeanMail <no-reply@leanmail.io>",
      "To: a@x.example",
      "Subject: Sign in to LeanMail",
      "Date: Sun, 01 Nov 2026 07:00:00 +0000",
      "Message-ID: <01JTEST@leanmail.io>",
      "MIME-Version: 1.0",
      "Content-Type: text/plain; charset=utf-8",
      "Content-Transfer-Encoding: 7bit",
      "Auto-Submitted: auto-generated",
    ]);
    expect(text).toContain(`${ORIGIN}/auth/magic?t=v1.a1.r.1.m\r\n`);
    expect(text).toContain("Or enter this code: 123456");
    expect(headerFrom(new TextEncoder().encode(msg))).toBe("no-reply@leanmail.io");
    expect(/^[\x00-\x7f]*$/.test(msg)).toBe(true);
    const welcome = signinMessage({ from: "no-reply@leanmail.io", to: "a@x.example", kind: "welcome", link: "l", code: "1", now, id: "x" });
    expect(welcome).toContain("Subject: Welcome to LeanMail\r\n");
  });
});

// ---- 4. forward-only routes are loop-guarded and breaker-limited ----

describe("4: forward-only loop guard and breaker", () => {
  async function deliver(msg: ReturnType<typeof fakeMessage>, now = 1_761_900_000_000) {
    const ctx = createExecutionContext();
    const out = await captureLogs(async () => {
      await handleEmail(msg, env, ctx, { now: () => now, wake: async () => {} });
      await waitOnExecutionContext(ctx);
    });
    return out.lines.filter((l) => l.ev === "email.decision");
  }

  it("refuses looped or bulk mail without forwarding it", async () => {
    await addDomain("ex.example", "acct1");
    await addRoute("fwd@ex.example", "acct1", "forward", ["out@else.example"]);
    for (const headers of [{ "X-LeanMail-Forwarded": "x@ex.example" }, { Precedence: "bulk" }, { "Auto-Submitted": "auto-replied" }] as Record<string, string>[]) {
      const msg = fakeMessage({ to: "fwd@ex.example", headers });
      const decisions = await deliver(msg);
      expect(msg.forwards).toEqual([]);
      expect(msg.rejected).toBe("not forwarded: possible mail loop");
      expect(decisions[0]).toMatchObject({ mail: "forward", forwarded: 0 });
      expect(decisions[0]!.refused).toBeTruthy();
    }
    expect((await env.DB.prepare("SELECT count(*) AS n FROM forward_window").first())!.n).toBe(0);
  });

  it("counts against the breaker, and once it trips fails temporarily without forwarding", async () => {
    await addDomain("ex.example", "acct1");
    await addRoute("fwd@ex.example", "acct1", "forward", ["out@else.example"]);
    const now = 1_761_900_000_000;
    const ok = fakeMessage({ to: "fwd@ex.example" });
    await deliver(ok, now);
    expect(ok.forwards).toHaveLength(1);
    expect(await env.DB.prepare("SELECT count FROM forward_window WHERE account_id = 'acct1'").first()).toEqual({ count: 1 });
    await env.DB.prepare("UPDATE forward_window SET count = ?1").bind(FORWARD_BREAKER_PER_HOUR).run();
    const tripped = fakeMessage({ to: "fwd@ex.example" });
    await expect(deliver(tripped, now)).rejects.toThrow(/breaker/);
    expect(tripped.forwards).toEqual([]);
    expect(tripped.rejected).toBeNull();
    expect((await env.RAW.list()).objects).toEqual([]);
  });
});

// ---- 5. /admin/* writes need our Origin and header ----

describe("5: non-GET /admin/*", () => {
  it("needs Origin = PUBLIC_ORIGIN and X-Requested-With: LeanMail", async () => {
    const { e, calls } = stubbed();
    const cookie = await cookieFor("a1");
    const post = (h: Record<string, string>) => fetchVia(e, "/admin/accounts/a2/disable", { method: "POST", headers: { cookie, accept: "application/json", ...h } });
    expect((await post({})).status).toBe(403);
    expect((await post({ origin: ORIGIN })).status).toBe(403);
    expect((await post({ "x-requested-with": "LeanMail" })).status).toBe(403);
    expect((await post({ origin: "https://evil.example", "x-requested-with": "LeanMail" })).status).toBe(403);
    expect((await post({ origin: "null", "x-requested-with": "LeanMail" })).status).toBe(403);
    const put = await fetchVia(e, "/admin/accounts/a2/domains", { method: "PUT", headers: { cookie, accept: "text/html" } });
    expect(put.status).toBe(403);
    expect(calls).toEqual([]);
    expect((await post({ origin: ORIGIN, "x-requested-with": "LeanMail" })).status).toBe(200);
    expect((await fetchVia(e, "/admin/accounts", { headers: { cookie, accept: "application/json" } })).status).toBe(200);
    expect(calls.map((c) => `${c.acct} ${c.path} ${c.opts.adminAccount}`)).toEqual(["_operator /admin/accounts/a2/disable a1", "_operator /admin/accounts a1"]);
    // Signed out is still 401 first.
    expect((await fetchVia(e, "/admin/accounts/a2/disable", { method: "POST", headers: { accept: "application/json" } })).status).toBe(401);
  });
});

// ---- 6. sign-in rate limits by IPv6 /64 too ----

describe("6: IPv6 /64 rate limit", () => {
  it("ipv6Net64 expands and truncates IPv6, and ignores IPv4", () => {
    expect(ipv6Net64("2001:db8::1")).toBe("2001:0db8:0000:0000::/64");
    expect(ipv6Net64("2001:DB8:1:2:3:4:5:6")).toBe("2001:0db8:0001:0002::/64");
    expect(ipv6Net64("2001:db8:1:2::")).toBe("2001:0db8:0001:0002::/64");
    expect(ipv6Net64("::1")).toBe("0000:0000:0000:0000::/64");
    expect(ipv6Net64("[2001:db8::1]")).toBe("2001:0db8:0000:0000::/64");
    expect(ipv6Net64("fe80::1%eth0")).toBe("fe80:0000:0000:0000::/64");
    expect(ipv6Net64("64:ff9b::192.0.2.1")).toBe("0064:ff9b:0000:0000::/64");
    expect(ipv6Net64("::ffff:192.0.2.1")).toBeNull();
    for (const bad of ["192.0.2.1", "", "garbage", "1::2::3", "1:2:3:4:5:6:7:8:9", "12345::1", "::ffff:1.2.3"]) expect(ipv6Net64(bad), bad).toBeNull();
  });

  it("limits a whole /64 to 10 an hour, across its addresses; other networks and IPv4 count apart", async () => {
    const req = (ip: string) => new Request(ORIGIN + "/auth/login", { method: "POST", headers: { "cf-connecting-ip": ip } });
    for (let i = 0; i < AUTH_RATE_PER_HOUR; i++) expect(await rateLimited(env.DB, "login", req(`2001:db8:1:2::${i + 1}`))).toBe(false);
    expect(await rateLimited(env.DB, "login", req("2001:db8:1:2:ffff::9"))).toBe(true);
    expect(await rateLimited(env.DB, "login", req("2001:db8:1:3::1"))).toBe(false);
    expect(await rateLimited(env.DB, "code", req("2001:db8:1:2::1"))).toBe(false);
    expect(await rateLimited(env.DB, "login", req("192.0.2.1"))).toBe(false);
    const keys = (await env.DB.prepare("SELECT key FROM auth_rate").all<{ key: string }>()).results.map((r) => r.key);
    expect(keys.every((k) => /^(login|code):(net:)?[0-9a-f]{64}$/.test(k))).toBe(true);
    expect(keys.filter((k) => k.startsWith("login:net:"))).toHaveLength(2);
  });

  it("applies to /auth/login and the code routes through fetch()", async () => {
    const { e } = stubbed();
    const login = (ip: string) => fetchVia(e, "/auth/login", { method: "POST", headers: { "content-type": "application/json", "cf-connecting-ip": ip }, body: '{"email":"x@x.example"}' });
    for (let i = 0; i < AUTH_RATE_PER_HOUR; i++) expect((await login(`2001:db8::${i + 1}`)).status).toBe(202);
    expect((await login("2001:db8::abcd")).status).toBe(429);
    const code = (ip: string) => fetchVia(e, "/auth/code", { method: "POST", headers: { "content-type": "application/json", "cf-connecting-ip": ip }, body: '{"email":"x@x.example","code":"1"}' });
    for (let i = 0; i < AUTH_RATE_PER_HOUR; i++) expect((await code(`2001:db8:9::${i + 1}`)).status).toBe(400);
    expect((await code("2001:db8:9::ffff")).status).toBe(429);
  });
});

// ---- 7. production config; decodeURIComponent never throws ----

describe("7: malformed escapes are a 400, never a throw", () => {
  it("in the Gmail hook and /internal/wake/:acct", async () => {
    const ctx = createExecutionContext();
    const r = await handleGmailHook(new Request(ORIGIN + "/hooks/gmail/%E0%A4%A/sig", { method: "POST", body: "{}" }), env, ctx);
    expect(r.status).toBe(400);
    const r2 = await handleGmailHook(new Request(ORIGIN + "/hooks/gmail/a1/%ZZ", { method: "POST", body: "{}" }), env, ctx);
    expect(r2.status).toBe(400);
    const w = await handleInternal(await internalReq("POST", "/internal/wake/%E0%A4%A", undefined, "_operator"), env, { wake: async () => {} });
    expect(w.status).toBe(400);
    expect(await w.json()).toEqual({ error: "bad_account_id" });
  });
});

// ---- 8. containers reach the Worker on the host under wrangler dev ----

describe("8: WORKER_ORIGIN under wrangler dev", () => {
  it("is host.docker.internal for a localhost PUBLIC_ORIGIN, keeping the port; PUBLIC_ORIGIN as is", () => {
    expect(workerOriginFor("http://127.0.0.1:8787")).toBe(`http://${DOCKER_HOST}:8787`);
    expect(workerOriginFor("http://localhost:9999")).toBe("http://host.docker.internal:9999");
    expect(workerOriginFor("http://localhost")).toBe("http://host.docker.internal:8787");
    expect(workerOriginFor("http://[::1]:8787")).toBe("http://host.docker.internal:8787");
    expect(workerOriginFor("https://leanmail.io")).toBe("https://leanmail.io");
    expect(workerOriginFor("https://localhost.leanmail.io")).toBe("https://localhost.leanmail.io");
    expect(workerOriginFor("not a url")).toBe("not a url");
    const dev = MailContainer.varsFor("dev", { ...env, PUBLIC_ORIGIN: "http://127.0.0.1:8787" });
    expect(dev).toMatchObject({ WORKER_ORIGIN: "http://host.docker.internal:8787", PUBLIC_ORIGIN: "http://127.0.0.1:8787" });
    const prod = MailContainer.varsFor("a1", { ...env, PUBLIC_ORIGIN: "https://leanmail.io" });
    expect(prod).toMatchObject({ WORKER_ORIGIN: "https://leanmail.io", PUBLIC_ORIGIN: "https://leanmail.io" });
  });

  it("tests and wrangler dev take PUBLIC_ORIGIN from .dev.vars, not production's", () => {
    expect(env.PUBLIC_ORIGIN).toBe("http://127.0.0.1:8787");
    expect(MailContainer.varsFor("dev", env).WORKER_ORIGIN).toBe("http://host.docker.internal:8787");
  });
});

// ---- 10. the first-start port race ----

describe("10: start, wait, and retry once", () => {
  const closed = () => new Response("Error proxying request to container: Container port connection closed unexpectedly", { status: 500 });

  it("sendRetryingOnce resends the whole request once after a dropped connection", async () => {
    const bodies: string[] = [];
    const replies = [closed, () => Response.json({ ok: true })];
    const send = async (r: Request) => (bodies.push(await r.text()), replies.shift()!());
    const r = await sendRetryingOnce(send, new Request("http://c/internal/wake", { method: "POST", body: '{"ulid":"x"}' }), true, 1);
    expect(r.status).toBe(200);
    expect(bodies).toEqual(['{"ulid":"x"}', '{"ulid":"x"}']);
  });

  it("does not retry twice, nor on another 500 (whose body is kept), nor when told not to", async () => {
    let n = 0;
    const always = async () => (n++, closed());
    expect((await sendRetryingOnce(always, new Request("http://c/"), true, 1)).status).toBe(500);
    expect(n).toBe(2);
    n = 0;
    const appError = async () => (n++, new Response("boom from the app", { status: 500, headers: { "x-app": "1" } }));
    const r = await sendRetryingOnce(appError, new Request("http://c/"), true, 1);
    expect([r.status, await r.text(), r.headers.get("x-app"), n]).toEqual([500, "boom from the app", "1", 1]);
    n = 0;
    expect((await sendRetryingOnce(always, new Request("http://c/", { method: "POST", body: "x" }), false, 1)).status).toBe(500);
    expect(n).toBe(1);
    n = 0;
    const throws = async () => {
      if (n++ === 0) throw new Error("Container port connection closed unexpectedly");
      return new Response("ok");
    };
    expect(await (await sendRetryingOnce(throws, new Request("http://c/"), true, 1)).text()).toBe("ok");
    const other = async () => {
      throw new Error("something else");
    };
    await expect(sendRetryingOnce(other, new Request("http://c/"), true, 1)).rejects.toThrow("something else");
  });

  it("retries only what it can afford to copy", () => {
    expect(retryable(new Request("http://c/"), false)).toBe(true);
    expect(retryable(new Request("http://c/", { method: "POST", body: "x" }), true)).toBe(true);
    expect(retryable(new Request("http://c/", { method: "POST", body: "x", headers: { "content-length": "1" } }), false)).toBe(true);
    expect(retryable(new Request("http://c/", { method: "POST", body: "x", headers: { "content-length": String(RETRY_BODY_MAX + 1) } }), false)).toBe(false);
    expect(retryable(new Request("http://c/", { method: "POST", body: new ReadableStream() }), false)).toBe(false);
  });

  it("proxy() waits for the port before the first request, and retries a dropped first connection", async () => {
    const { proxy, sent } = fakeContainer([closed, () => Response.json({ ok: true })]);
    const r = await proxy(new Request("http://c/inbox"));
    expect(r.status).toBe(200);
    expect(sent.map((s) => s.path)).toEqual(["/inbox", "/inbox"]);

    const waited: unknown[] = [];
    let status = "stopped";
    const self = {
      defaultPort: 8080,
      ctx: { id: { name: "a1" } },
      getState: async () => ({ status, lastChange: 0 }),
      startAndWaitForPorts: async (p: unknown) => void waited.push(p),
    };
    const ensure = (MailContainer.prototype as unknown as { ensureStarted: (this: unknown) => Promise<void> }).ensureStarted;
    await ensure.call(self);
    status = "healthy";
    await ensure.call(self);
    expect(waited).toEqual([8080]);
    status = "stopped";
    const failing = { ...self, startAndWaitForPorts: async () => Promise.reject(new Error("no instance")) };
    const { lines } = await captureLogs(() => ensure.call(failing));
    expect(lines.find((l) => l.ev === "container.start_wait_failed")).toMatchObject({ account: "a1" });
  });
});
