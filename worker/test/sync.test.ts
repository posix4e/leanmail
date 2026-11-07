import { createExecutionContext, waitOnExecutionContext } from "cloudflare:test";
import { beforeEach, describe, expect, it } from "vitest";
import { handleEmail } from "../src/email";
import type { Env } from "../src/env";
import { dailyTick, handleFetch } from "../src/index";
import { sha256Hex } from "../src/rate";
import { deriveInternalSecret, signMac } from "../src/tokens";
import { ulid } from "../src/ulid";
import { SECRET, addRoute, env as testEnv, fakeMessage, resetDb } from "./helpers";

const SESSION = "test-session-secret";
const env = { ...testEnv, DEV_ACCOUNT: undefined } as unknown as Env; // production: no dev fallback
const ALL = ["read", "draft", "send", "act", "junk"];

interface Page {
  entries: { seq: number; kind: string; at: number; data: Record<string, any> }[];
  head: number;
  more: boolean;
  settings?: Record<string, { value: unknown; version: number }>;
}

let acct: string;

/** A fresh active account: a Durable Object keeps its storage between the tests in this file. */
async function newAccount(status = "active"): Promise<string> {
  const id = `a${ulid().toLowerCase()}`;
  await env.DB.prepare("INSERT INTO accounts (id, email, status, updated_at) VALUES (?1, ?2, ?3, 1)").bind(id, `${id}@owner.example`, status).run();
  return id;
}

async function call(path: string, init: RequestInit = {}): Promise<Response> {
  const ctx = createExecutionContext();
  const r = await handleFetch(new Request(`https://mail.example${path}`, init), env, ctx);
  await waitOnExecutionContext(ctx);
  return r;
}

/** What `who`'s container does after POST /api/tokens: mint, then tell the Worker. */
async function tokenFor(who: string, scopes: string[] = ALL): Promise<string> {
  const token = await signMac("lm1", who, crypto.randomUUID(), SESSION);
  const r = await internal(who, "PUT", "/internal/tokens", { hash: await sha256Hex(token), scopes });
  expect(r.status).toBe(200);
  return token;
}

async function internal(who: string, method: string, path: string, body: unknown): Promise<Response> {
  return call(path, {
    method,
    headers: { "x-account-id": who, "x-internal-secret": await deriveInternalSecret(SECRET, who), "content-type": "application/json" },
    body: JSON.stringify(body),
  });
}

const bearer = (token: string) => ({ authorization: `Bearer ${token}` });

async function sync(token: string, query = ""): Promise<Page> {
  const r = await call(`/sync${query}`, { headers: bearer(token) });
  expect(r.status).toBe(200);
  return r.json();
}

function patch(token: string, body: unknown): Promise<Response> {
  return call("/sync/settings", { method: "PATCH", headers: { ...bearer(token), "content-type": "application/json" }, body: JSON.stringify(body) });
}

async function deliver(to: string, raw: string, now = Date.now()): Promise<void> {
  const ctx = createExecutionContext();
  await handleEmail(fakeMessage({ to, from: "sender@else.example", raw }), env, ctx, { now: () => now, wake: async () => {} });
  await waitOnExecutionContext(ctx);
}

beforeEach(async () => {
  await resetDb();
  const listed = await env.RAW.list();
  if (listed.objects.length) await env.RAW.delete(listed.objects.map((o) => o.key));
  acct = await newAccount();
  await addRoute(`me@${acct}.example`, acct, "vault", [], 1);
});

describe("a client following the mailbox", () => {
  it("sees delivered mail in order, reads it, and only ever asks for what is new", async () => {
    const token = await tokenFor(acct);
    expect(await sync(token)).toMatchObject({ entries: [], head: 0, more: false, settings: {} });

    const first = "From: sender@else.example\r\nSubject: one\r\n\r\nfirst\r\n";
    await deliver(`me@${acct}.example`, first, 1_800_000_000_000);
    await deliver(`me+tag@${acct}.example`, "From: sender@else.example\r\nSubject: two\r\n\r\nsecond\r\n", 1_800_000_000_001);

    // A phone on a short leash: one entry a page, following `more`.
    const one = await sync(token, "?limit=1");
    expect(one.more).toBe(true);
    expect(one.entries).toHaveLength(1);
    expect(one.entries[0]).toMatchObject({
      kind: "mail.received",
      at: 1_800_000_000_000,
      data: { rcpt: `me@${acct}.example`, from: "sender@else.example", source: "route", size: first.length },
    });
    const two = await sync(token, `?after=${one.entries[0]!.seq}`);
    expect(two.more).toBe(false);
    expect(two.settings).toBeUndefined();
    expect(two.entries.map((e) => e.data.rcpt)).toEqual([`me+tag@${acct}.example`]);

    const raw = await call(`/sync/raw/${one.entries[0]!.data.ulid}`, { headers: bearer(token) });
    expect(raw.status).toBe(200);
    expect(raw.headers.get("content-type")).toBe("message/rfc822");
    expect(await raw.text()).toBe(first);

    expect(await sync(token, `?after=${two.head}`)).toMatchObject({ entries: [], more: false });
  });

  it("gets a 404, not an error, for a message that has expired", async () => {
    const token = await tokenFor(acct);
    await deliver(`me@${acct}.example`, "From: a@b.example\r\n\r\nx\r\n");
    const id = (await sync(token)).entries[0]!.data.ulid;
    await env.RAW.delete(`raw/${acct}/${id}`);
    expect((await call(`/sync/raw/${id}`, { headers: bearer(token) })).status).toBe(404);
  });
});

describe("mail the log missed", () => {
  it("fills in on the first sync: mail from before the log, Gmail's old dates, and nothing twice", async () => {
    const token = await tokenFor(acct);
    const old = ulid(1_500_000_000_000);
    const gmail = ulid(1_400_000_000_000);
    await env.RAW.put(`raw/${acct}/${old}`, "old mail", { customMetadata: { rcpt: "me@x.example", from: "a@b.example", received_at: "1500000000000", source: "route" } });
    await env.RAW.put(`raw/${acct}/${gmail}`, "gmail", { customMetadata: { received_at: "1400000000000", source: "gmail" } });
    await env.RAW.put(`raw/${acct}/${ulid(1_300_000_000_000)}`, "no metadata at all");
    await env.RAW.put(`raw/${acct}/not-a-ulid`, "stray");
    await env.RAW.put(`raw/${acct}x/${ulid()}`, "the next account along");

    expect((await sync(token)).entries).toEqual([]); // the sweep runs behind this answer
    const filled = await sync(token);
    expect(filled.entries.map((e) => e.at)).toEqual([1_300_000_000_000, 1_400_000_000_000, 1_500_000_000_000]);
    expect(filled.entries[1]!.data).toMatchObject({ ulid: gmail, source: "gmail", size: 5 });

    await deliver(`me@${acct}.example`, "From: a@b.example\r\n\r\nnew\r\n");
    await dailyTick(env, async () => {});
    await dailyTick(env, async () => {});
    expect((await sync(token)).entries).toHaveLength(4);
  });

  it("delivery still succeeds when the log cannot be written, and the daily sweep adds the message", async () => {
    const token = await tokenFor(acct);
    await sync(token);
    const broken = { ...env, LOG: { idFromName: () => { throw new Error("log down"); } } } as unknown as Env;
    const ctx = createExecutionContext();
    await handleEmail(fakeMessage({ to: `me@${acct}.example`, raw: "From: a@b.example\r\n\r\nx\r\n" }), broken, ctx, { wake: async () => {} });
    await waitOnExecutionContext(ctx);
    expect((await sync(token)).entries).toEqual([]);

    await dailyTick(env, async () => {});
    expect((await sync(token)).entries).toHaveLength(1);
  });
});

describe("who gets in", () => {
  it("refuses no credential, a bad MAC, a token never registered, and the session cookie", async () => {
    expect((await call("/sync")).status).toBe(401);
    const unregistered = await signMac("lm1", acct, "r", SESSION);
    expect((await call("/sync", { headers: bearer(unregistered) })).status).toBe(401);
    expect((await call("/sync", { headers: bearer(`${unregistered}x`) })).status).toBe(401);
    const cookie = `lm_session=${await signMac("v1", acct, "sid.9999999999999.1", SESSION)}`;
    expect((await call("/sync", { headers: { cookie } })).status).toBe(401);
  });

  it("stops a token as soon as its container revokes it, and a late registration cannot bring it back", async () => {
    const token = await tokenFor(acct);
    await sync(token);
    const hash = await sha256Hex(token);
    expect((await internal(acct, "POST", "/internal/tokens/revoke", { hash })).status).toBe(200);
    expect((await call("/sync", { headers: bearer(token) })).status).toBe(401);
    await internal(acct, "PUT", "/internal/tokens", { hash, scopes: ALL });
    expect((await call("/sync", { headers: bearer(token) })).status).toBe(401);
  });

  it("holds a token to its scopes", async () => {
    const sendOnly = await tokenFor(acct, ["send"]);
    await deliver(`me@${acct}.example`, "From: a@b.example\r\n\r\nx\r\n");
    const reader = await tokenFor(acct, ["read"]);
    const id = (await sync(reader)).entries[0]!.data.ulid;
    expect((await call("/sync", { headers: bearer(sendOnly) })).status).toBe(403);
    expect((await call(`/sync/raw/${id}`, { headers: bearer(sendOnly) })).status).toBe(403);
    expect((await call("/sync/settings", { headers: bearer(sendOnly) })).status).toBe(403);
    expect((await patch(reader, { display_name: "x" })).status).toBe(403);
  });

  it("keeps accounts apart", async () => {
    const mine = await tokenFor(acct);
    await deliver(`me@${acct}.example`, "From: a@b.example\r\n\r\nx\r\n");
    const id = (await sync(mine)).entries[0]!.data.ulid;

    const other = await newAccount();
    const theirs = await tokenFor(other);
    expect((await sync(theirs)).entries).toEqual([]);
    expect((await call(`/sync/raw/${id}`, { headers: bearer(theirs) })).status).toBe(404);
    // A token is on one account's list only, and a container can only write to its own.
    const hash = await sha256Hex(mine);
    await internal(other, "POST", "/internal/tokens/revoke", { hash });
    expect((await call("/sync", { headers: bearer(mine) })).status).toBe(200);
    const forged = await signMac("lm1", other, "r", SESSION);
    await internal(acct, "PUT", "/internal/tokens", { hash: await sha256Hex(forged), scopes: ALL });
    expect((await call("/sync", { headers: bearer(forged) })).status).toBe(401);
  });

  it("refuses a disabled account's token", async () => {
    const token = await tokenFor(acct);
    await env.DB.prepare("UPDATE accounts SET status = 'disabled' WHERE id = ?").bind(acct).run();
    expect((await call("/sync", { headers: bearer(token) })).status).toBe(401);
  });

  it("refuses a bad cursor and a path that is not a ULID", async () => {
    const token = await tokenFor(acct);
    for (const q of ["?after=x", "?after=-1", "?limit=0", "?limit=1e3"]) {
      expect((await call(`/sync${q}`, { headers: bearer(token) })).status).toBe(400);
    }
    for (const p of ["/sync/raw/..%2f..%2fetc", "/sync/raw/abc", "/sync/other"]) {
      expect((await call(p, { headers: bearer(token) })).status).toBe(404);
    }
    expect((await call("/sync", { method: "POST", headers: bearer(token) })).status).toBe(405);
  });
});

describe("settings across devices", () => {
  it("reaches a second device through the log and its first snapshot, with a version per key", async () => {
    const phone = await tokenFor(acct);
    const laptop = await tokenFor(acct);
    const head = (await sync(laptop)).head;

    const r = await patch(phone, { display_name: "  Ada  ", prefs: { theme: "dark" } });
    expect(r.status).toBe(200);
    expect(await r.json()).toMatchObject({ settings: { display_name: { value: "Ada", version: 1 }, prefs: { value: { theme: "dark" }, version: 1 } } });
    await patch(phone, { display_name: "Ada L" });

    const seen = await sync(laptop, `?after=${head}`);
    expect(seen.entries.map((e) => [e.kind, e.data.key, e.data.value, e.data.version])).toEqual([
      ["settings.changed", "display_name", "Ada", 1],
      ["settings.changed", "prefs", { theme: "dark" }, 1],
      ["settings.changed", "display_name", "Ada L", 2],
    ]);

    const fresh = await tokenFor(acct);
    expect((await sync(fresh)).settings).toMatchObject({ display_name: { value: "Ada L", version: 2 } });
    const got = await call("/sync/settings", { headers: bearer(fresh) });
    expect(await got.json()).toMatchObject({ settings: { prefs: { value: { theme: "dark" } } } });
  });

  it("adds nothing to the log when a device saves what is already set", async () => {
    const token = await tokenFor(acct);
    await patch(token, { display_name: "Ada" });
    const { head } = await sync(token);
    await patch(token, { display_name: "Ada" });
    expect((await sync(token)).head).toBe(head);
  });

  it("takes only the known keys, checked, and changes nothing when any one is bad", async () => {
    const token = await tokenFor(acct);
    const bad: [unknown, string][] = [
      [{ ai_api_key: "sk-secret" }, "unknown_setting"],
      [{ gmail_refresh_token: "t" }, "unknown_setting"],
      [{ constructor: "x" }, "unknown_setting"],
      [{ display_name: "x".repeat(201) }, "display_name_too_long"],
      [{ display_name: 1 }, "bad_display_name"],
      [{ default_from: "not an address" }, "bad_default_from"],
      [{ default_from: "someone@else.example" }, "default_from_not_sendable"],
      [{ prefs: [] }, "bad_prefs"],
      [{ prefs: { big: "x".repeat(17 * 1024) } }, "prefs_too_large"],
      [{ display_name: "ok", prefs: 3 }, "bad_prefs"],
      [[], "bad_json"],
    ];
    for (const [body, error] of bad) {
      const r = await patch(token, body);
      expect(r.status).toBe(400);
      expect(await r.json()).toEqual({ error });
    }
    expect(await sync(token)).toMatchObject({ entries: [], settings: {} });
  });

  it("lets default_from be an address this account can send as, or empty", async () => {
    const token = await tokenFor(acct);
    await addRoute(`noreply@${acct}.example`, acct, "vault", [], 0);
    expect((await patch(token, { default_from: `noreply@${acct}.example` })).status).toBe(400);
    await addRoute("theirs@else.example", "someone-else", "vault", [], 1);
    expect((await patch(token, { default_from: "theirs@else.example" })).status).toBe(400);
    const r = await patch(token, { default_from: `  ME@${acct}.example ` });
    expect(r.status).toBe(200);
    expect(await r.json()).toMatchObject({ settings: { default_from: { value: `me@${acct}.example` } } });
    expect((await patch(token, { default_from: "" })).status).toBe(200);
  });
});
