import { createExecutionContext, waitOnExecutionContext } from "cloudflare:test";
import { beforeEach, describe, expect, it } from "vitest";
import { AUTH_RATE_PER_HOUR } from "../src/auth";
import type { StampOptions } from "../src/container";
import type { Env } from "../src/env";
import { GATE_COOKIE, gateToken, gateTokenOk } from "../src/gate";
import { handleFetch } from "../src/index";
import { signMac } from "../src/tokens";
import { env, resetDb } from "./helpers";

const ORIGIN = "https://mail.leanmail.example";
const PASS = "correct horse battery staple";

beforeEach(resetDb);

function gated(passphrase: string | null = PASS) {
  const calls: { acct: string; path: string; headers: Record<string, string> }[] = [];
  const MAIL = {
    idFromName: (n: string) => n,
    get: (name: string) => ({
      proxy: async (r: Request, acct: string, _opts: StampOptions = {}) => {
        calls.push({ acct, path: new URL(r.url).pathname, headers: Object.fromEntries(r.headers) });
        await r.body?.cancel();
        return new Response(`page from ${name}`, { status: 200, headers: { "content-type": "text/html" } });
      },
    }),
  };
  return { e: { ...env, MAIL, PUBLIC_ORIGIN: ORIGIN, GATE_PASSPHRASE: passphrase ?? undefined } as unknown as Env, calls };
}

async function fetchAs(e: Env, path: string, headers: Record<string, string> = {}): Promise<Response> {
  const ctx = createExecutionContext();
  const res = await handleFetch(new Request(ORIGIN + path, { headers: { "cf-connecting-ip": "203.0.113.9", ...headers } }), e, ctx);
  await waitOnExecutionContext(ctx);
  return res;
}

const basic = (pw: string) => `Basic ${btoa(`anyone:${pw}`)}`;
const browser = { accept: "text/html" };

describe("passphrase gate", () => {
  it("is off without GATE_PASSPHRASE: a signed-out browser still goes to /signin", async () => {
    const { e } = gated(null);
    const res = await fetchAs(e, "/", browser);
    expect(res.status).toBe(302);
    expect(res.headers.get("location")).toBe("/signin");
  });

  it("answers a bare 401 with a Basic challenge to anyone signed out, even for /signin and /signup", async () => {
    const { e, calls } = gated();
    for (const path of ["/", "/signin", "/signup", "/static/app.css", "/api/msgs"]) {
      const res = await fetchAs(e, path, browser);
      expect(res.status, path).toBe(401);
      expect(res.headers.get("www-authenticate")).toContain("Basic");
      expect(await res.text()).not.toContain("LeanMail");
    }
    expect(calls).toHaveLength(0);
  });

  it("refuses a wrong passphrase", async () => {
    const { e, calls } = gated();
    const res = await fetchAs(e, "/signin", { ...browser, authorization: basic("nope") });
    expect(res.status).toBe(401);
    expect(calls).toHaveLength(0);
  });

  it("lets the right passphrase through, sets the gate cookie, and strips the Basic header", async () => {
    const { e, calls } = gated();
    const res = await fetchAs(e, "/signin", { ...browser, authorization: basic(PASS) });
    expect(res.status).toBe(200);
    const sc = res.headers.get("set-cookie") ?? "";
    expect(sc).toMatch(new RegExp(`^${GATE_COOKIE}=v1\\.`));
    expect(sc).toContain("HttpOnly");
    expect(sc).toContain("Secure");
    expect(calls).toHaveLength(1);
    expect(calls[0]!.acct).toBe("_operator");
    expect(calls[0]!.headers.authorization).toBeUndefined();

    const cookie = sc.split(";")[0]!;
    const again = await fetchAs(e, "/signup", { ...browser, cookie });
    expect(again.status).toBe(200);
    expect(again.headers.get("set-cookie")).toBeNull();
  });

  it("lets a signed-in session or API token through without the passphrase", async () => {
    const { e, calls } = gated();
    const exp = Math.floor(Date.now() / 1000) + 3600;
    const session = await signMac("v1", "alice", `sid.${exp}.1`, env.SESSION_SECRET!);
    const token = await signMac("lm1", "alice", "rand", env.SESSION_SECRET!);
    expect((await fetchAs(e, "/api/msgs", { cookie: `lm_session=${session}` })).status).toBe(200);
    expect((await fetchAs(e, "/api/msgs", { authorization: `Bearer ${token}` })).status).toBe(200);
    expect(calls.map((c) => c.acct)).toEqual(["alice", "alice"]);
  });

  it("keeps a signed-in browser's cached Basic header from reaching the container", async () => {
    const { e, calls } = gated();
    const exp = Math.floor(Date.now() / 1000) + 3600;
    const session = await signMac("v1", "alice", `sid.${exp}.1`, env.SESSION_SECRET!);
    const res = await fetchAs(e, "/api/msgs", { cookie: `lm_session=${session}`, authorization: basic(PASS) });
    expect(res.status).toBe(200);
    expect(calls[0]!.headers.authorization).toBeUndefined();
  });

  it("leaves /health open", async () => {
    const { e } = gated();
    expect((await fetchAs(e, "/health")).status).toBe(200);
  });

  it("rate-limits passphrase attempts", async () => {
    const { e } = gated();
    for (let i = 0; i < AUTH_RATE_PER_HOUR; i++) {
      expect((await fetchAs(e, "/", { authorization: basic(`guess${i}`) })).status).toBe(401);
    }
    expect((await fetchAs(e, "/", { authorization: basic(PASS) })).status).toBe(429);
  });

  it("voids gate cookies when the passphrase changes, and when they expire", async () => {
    const { e } = gated();
    const t = await gateToken(e);
    expect(await gateTokenOk(e, t)).toBe(true);
    expect(await gateTokenOk({ ...e, GATE_PASSPHRASE: "another" } as Env, t)).toBe(false);
    expect(await gateTokenOk(e, t, Date.now() + 91 * 24 * 3600 * 1000)).toBe(false);
    expect(await gateTokenOk(e, t.slice(0, -2) + "AA")).toBe(false);
  });
});

describe("RP_ID", () => {
  it("is used only when it is the origin's host or a parent domain of it", async () => {
    const { rpIdFor } = await import("../src/auth");
    const o = "https://mail.leanmail.io";
    expect(rpIdFor({ PUBLIC_ORIGIN: o })).toBe("mail.leanmail.io");
    expect(rpIdFor({ PUBLIC_ORIGIN: o, RP_ID: "leanmail.io" })).toBe("leanmail.io");
    expect(rpIdFor({ PUBLIC_ORIGIN: o, RP_ID: "LeanMail.io " })).toBe("leanmail.io");
    expect(rpIdFor({ PUBLIC_ORIGIN: o, RP_ID: "mail.leanmail.io" })).toBe("mail.leanmail.io");
    expect(rpIdFor({ PUBLIC_ORIGIN: o, RP_ID: "io" })).toBe("mail.leanmail.io");
    expect(rpIdFor({ PUBLIC_ORIGIN: o, RP_ID: "evil.example" })).toBe("mail.leanmail.io");
    expect(rpIdFor({ PUBLIC_ORIGIN: o, RP_ID: "anleanmail.io" })).toBe("mail.leanmail.io");
  });
});
