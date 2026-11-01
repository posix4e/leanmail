import { afterEach, describe, expect, it, vi } from "vitest";
import type { Env } from "../src/env";
import { sendApns } from "../src/apns";
import { apnsConfig, handleInternal } from "../src/internal";
import { deriveInternalSecret } from "../src/tokens";
import { SECRET, env } from "./helpers";

const TOKEN_A = "a".repeat(64);
const TOKEN_B = "b".repeat(64);

async function pushReq(body: unknown, acct = "a1"): Promise<Request> {
  return new Request("https://leanmail.example/internal/push", {
    method: "POST",
    headers: { "content-type": "application/json", "x-account-id": acct, "x-internal-secret": await deriveInternalSecret(SECRET, acct) },
    body: JSON.stringify(body),
  });
}

/** A fresh P-256 key as the PEM-armoured PKCS#8 .p8 Apple hands out. */
async function p8(): Promise<string> {
  const pair = (await crypto.subtle.generateKey({ name: "ECDSA", namedCurve: "P-256" }, true, ["sign", "verify"])) as CryptoKeyPair;
  const der = new Uint8Array((await crypto.subtle.exportKey("pkcs8", pair.privateKey)) as ArrayBuffer);
  return `-----BEGIN PRIVATE KEY-----\n${btoa(String.fromCharCode(...der))}\n-----END PRIVATE KEY-----`;
}

const apnsEnv = (keyP8 = "unused") => ({ ...env, APNS_TEAM_ID: "TEAM123456", APNS_KEY_ID: "KEY1234567", APNS_KEY_P8: keyP8, APNS_TOPIC: "io.leanmail.app" }) as unknown as Env;

afterEach(() => vi.restoreAllMocks());

describe("/internal/push", () => {
  it("answers {sent:0, dead:[], failed:0} with no tokens, configured or not", async () => {
    const r = await handleInternal(await pushReq({ tokens: [], payload: { id: "x" } }), env);
    expect(r.status).toBe(200);
    expect(await r.json()).toEqual({ sent: 0, dead: [], failed: 0 });
  });

  it("is 503 with tokens but no APNs key, and 400 on a malformed token", async () => {
    const r = await handleInternal(await pushReq({ tokens: [{ token: TOKEN_A, env: "prod" }], payload: { id: "m1", from: "A", subject: "S" } }), env);
    expect(r.status).toBe(503);
    expect(apnsConfig(env)).toBeNull();
    const bad = await handleInternal(await pushReq({ tokens: [{ token: "not hex", env: "prod" }], payload: { id: "m1" } }), apnsEnv());
    expect(bad.status).toBe(400);
    const badEnv = await handleInternal(await pushReq({ tokens: [{ token: TOKEN_A, env: "staging" }], payload: { id: "m1" } }), apnsEnv());
    expect(badEnv.status).toBe(400);
  });

  it("sends {id, from, subject} to every device through the APNs transport and returns what it says", async () => {
    const calls: { devices: unknown; payload: Record<string, unknown> }[] = [];
    const stub = (async (_cfg, devices, payload) => {
      calls.push({ devices, payload });
      return { sent: 1, dead: [TOKEN_B], failed: 0 };
    }) as typeof sendApns;
    const r = await handleInternal(
      await pushReq({ tokens: [{ token: TOKEN_A, env: "prod" }, { token: TOKEN_B, env: "sandbox" }], payload: { id: "m1", from: "Alex", subject: "Hello" } }),
      apnsEnv(),
      { sendApns: stub },
    );
    expect(r.status).toBe(200);
    expect(await r.json()).toEqual({ sent: 1, dead: [TOKEN_B], failed: 0 });
    expect(calls).toHaveLength(1);
    expect(calls[0]!.devices).toEqual([{ token: TOKEN_A, env: "prod" }, { token: TOKEN_B, env: "sandbox" }]);
    expect(calls[0]!.payload).toMatchObject({ id: "m1", aps: { alert: { title: "Alex", body: "Hello" } } });
  });

  it("the real transport signs an ES256 provider token and reports a 410 device as dead", async () => {
    const seen: { url: string; auth: string | null; topic: string | null; body: string }[] = [];
    vi.spyOn(globalThis, "fetch").mockImplementation(async (input, init) => {
      const url = typeof input === "string" ? input : input instanceof URL ? input.href : input.url;
      const h = new Headers(init?.headers);
      seen.push({ url, auth: h.get("authorization"), topic: h.get("apns-topic"), body: String(init?.body ?? "") });
      if (url.includes(TOKEN_B)) return new Response(JSON.stringify({ reason: "Unregistered" }), { status: 410 });
      return new Response(null, { status: 200 });
    });
    const r = await handleInternal(
      await pushReq({ tokens: [{ token: TOKEN_A, env: "sandbox" }, { token: TOKEN_B, env: "prod" }], payload: { id: "m2", from: "Bea", subject: "Hi" } }),
      apnsEnv(await p8()),
    );
    expect(r.status).toBe(200);
    expect(await r.json()).toEqual({ sent: 1, dead: [TOKEN_B], failed: 0 });
    expect(seen.map((s) => s.url)).toEqual([`https://api.sandbox.push.apple.com/3/device/${TOKEN_A}`, `https://api.push.apple.com/3/device/${TOKEN_B}`]);
    expect(seen[0]!.auth).toMatch(/^bearer [\w-]+\.[\w-]+\.[\w-]+$/);
    expect(seen[0]!.topic).toBe("io.leanmail.app");
    expect(JSON.parse(seen[0]!.body)).toMatchObject({ id: "m2", aps: { alert: { title: "Bea", body: "Hi" } } });
  });
});
