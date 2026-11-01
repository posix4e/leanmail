import { SELF } from "cloudflare:test";
import { describe, expect, it } from "vitest";
import { nextWakeSeconds } from "../src/container";
import { gmailHookSig, handleGmailHook, handleOAuthCallback, pushPayload } from "../src/gmail";
import { b64uEncode, deriveInternalSecret, signMac } from "../src/tokens";
import { SECRET, env } from "./helpers";

const SESSION = "test-session-secret";

/** A fake ExecutionContext that keeps what was handed to waitUntil. */
function fakeCtx(): ExecutionContext & { waits: Promise<unknown>[] } {
  const waits: Promise<unknown>[] = [];
  return { waits, waitUntil: (p: Promise<unknown>) => void waits.push(p), passThroughOnException() {} } as unknown as ExecutionContext & {
    waits: Promise<unknown>[];
  };
}

function pubsub(inner: unknown): string {
  return JSON.stringify({ message: { data: btoa(JSON.stringify(inner)), messageId: "1" }, subscription: "projects/p/subscriptions/s" });
}

describe("gmail push hook", () => {
  it("signs base64url(HMAC-SHA256(the account's derived internal secret, 'gmail-push:' + acct))", async () => {
    const k = await deriveInternalSecret(SECRET, "a1");
    const key = await crypto.subtle.importKey("raw", new TextEncoder().encode(k), { name: "HMAC", hash: "SHA-256" }, false, ["sign"]);
    const mac = new Uint8Array(await crypto.subtle.sign("HMAC", key, new TextEncoder().encode("gmail-push:a1")));
    expect(await gmailHookSig(SECRET, "a1")).toBe(b64uEncode(mac));
    expect(await gmailHookSig(SECRET, "a1")).not.toBe(await gmailHookSig(SECRET, "a2"));
  });

  it("forwards {emailAddress, historyId} to the account's /internal/gmail/push and answers 204", async () => {
    const calls: { acct: string; path: string; body: unknown }[] = [];
    const call = async (_e: unknown, acct: string, path: string, body?: unknown) => {
      calls.push({ acct, path, body });
      return new Response("{}", { status: 200 });
    };
    const sig = await gmailHookSig(SECRET, "a1");
    const ctx = fakeCtx();
    const res = await handleGmailHook(
      new Request(`https://leanmail.example/hooks/gmail/a1/${sig}`, { method: "POST", body: pubsub({ emailAddress: "alex@gmail.com", historyId: 12345 }) }),
      env,
      ctx,
      { call },
    );
    expect(res.status).toBe(204);
    await Promise.all(ctx.waits);
    expect(calls).toEqual([{ acct: "a1", path: "/internal/gmail/push", body: { emailAddress: "alex@gmail.com", historyId: "12345" } }]);
  });

  it("refuses a wrong signature, another account's signature, a missing secret and other methods", async () => {
    const calls: string[] = [];
    const call = async (_e: unknown, acct: string) => {
      calls.push(acct);
      return new Response("{}");
    };
    const body = pubsub({ emailAddress: "a@gmail.com", historyId: "1" });
    const post = (path: string) => new Request(`https://leanmail.example${path}`, { method: "POST", body });
    const a2sig = await gmailHookSig(SECRET, "a2");
    expect((await handleGmailHook(post("/hooks/gmail/a1/nope"), env, fakeCtx(), { call })).status).toBe(403);
    expect((await handleGmailHook(post(`/hooks/gmail/a1/${a2sig}`), env, fakeCtx(), { call })).status).toBe(403);
    // the master secret's own HMAC is not the account's
    const masterSig = b64uEncode(
      new Uint8Array(
        await crypto.subtle.sign(
          "HMAC",
          await crypto.subtle.importKey("raw", new TextEncoder().encode(SECRET), { name: "HMAC", hash: "SHA-256" }, false, ["sign"]),
          new TextEncoder().encode("gmail-push:a1"),
        ),
      ),
    );
    expect((await handleGmailHook(post(`/hooks/gmail/a1/${masterSig}`), env, fakeCtx(), { call })).status).toBe(403);
    const sig = await gmailHookSig(SECRET, "a1");
    expect((await handleGmailHook(post(`/hooks/gmail/a1/${sig}`), { ...env, INTERNAL_SECRET: undefined }, fakeCtx(), { call })).status).toBe(403);
    expect((await handleGmailHook(new Request(`https://leanmail.example/hooks/gmail/a1/${sig}`), env, fakeCtx(), { call })).status).toBe(405);
    expect(calls).toEqual([]);
  });

  it("acknowledges a signed push with no usable data without waking anything", async () => {
    const calls: string[] = [];
    const call = async (_e: unknown, acct: string) => {
      calls.push(acct);
      return new Response("{}");
    };
    const sig = await gmailHookSig(SECRET, "a1");
    const ctx = fakeCtx();
    const res = await handleGmailHook(new Request(`https://leanmail.example/hooks/gmail/a1/${sig}`, { method: "POST", body: "not json" }), env, ctx, { call });
    expect(res.status).toBe(204);
    expect(ctx.waits.length).toBe(0);
    expect(pushPayload(pubsub({ emailAddress: "x@gmail.com" }))).toEqual({ emailAddress: "x@gmail.com" });
    expect(pushPayload("{}")).toBeNull();
  });

  it("is routed by the front door before any credential check", async () => {
    const res = await SELF.fetch("https://leanmail.example/hooks/gmail/a1/forged", { method: "POST", body: "{}" });
    expect(res.status).toBe(403);
  });
});

describe("google oauth callback", () => {
  it("routes to the account the state's MAC names", async () => {
    const state = await signMac("v1", "a1", `goauth.nonce123.${Date.now() + 600000}`, SESSION);
    const routed: string[] = [];
    const proxy = async (_e: unknown, acct: string, req: Request) => {
      routed.push(`${acct} ${new URL(req.url).pathname}`);
      return new Response(null, { status: 302, headers: { location: "/settings/gmail?connected=1" } });
    };
    const res = await handleOAuthCallback(new Request(`https://leanmail.example/oauth/google/callback?code=c&state=${encodeURIComponent(state)}`), env, { proxy });
    expect(res.status).toBe(302);
    expect(routed).toEqual(["a1 /oauth/google/callback"]);
  });

  it("refuses a forged state, a magic-link-shaped token and one signed for another account", async () => {
    const routed: string[] = [];
    const proxy = async (_e: unknown, acct: string) => {
      routed.push(acct);
      return new Response(null);
    };
    const magic = await signMac("v1", "a1", `rand.${Date.now() + 600000}`, SESSION);
    const forged = (await signMac("v1", "a1", "goauth.n.1", SESSION)).replace(/.$/, (c) => (c === "A" ? "B" : "A"));
    // a1's container key used to sign a state naming a2
    const { signMacWithKey, deriveSecret } = await import("../src/tokens");
    const crossed = await signMacWithKey("v1", "a2", "goauth.n.1", await deriveSecret(SESSION, "a1"));
    for (const state of [magic, forged, crossed, ""]) {
      const res = await handleOAuthCallback(new Request(`https://leanmail.example/oauth/google/callback?code=c&state=${encodeURIComponent(state)}`), env, { proxy });
      expect(res.status).toBe(400);
    }
    expect(routed).toEqual([]);
    const viaSelf = await SELF.fetch(`https://leanmail.example/oauth/google/callback?code=c&state=${encodeURIComponent(magic)}`);
    expect(viaSelf.status).toBe(400);
  });
});

describe("X-Next-Wake", () => {
  it("is whole seconds clamped to a minute .. a day", () => {
    expect(nextWakeSeconds("300")).toBe(300);
    expect(nextWakeSeconds("1800")).toBe(1800);
    expect(nextWakeSeconds("5")).toBe(60);
    expect(nextWakeSeconds("999999")).toBe(86400);
    expect(nextWakeSeconds(null)).toBeNull();
    expect(nextWakeSeconds("1.5")).toBeNull();
    expect(nextWakeSeconds("-60")).toBeNull();
  });
});
