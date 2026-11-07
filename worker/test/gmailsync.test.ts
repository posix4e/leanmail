import { createExecutionContext, runDurableObjectAlarm, runInDurableObject, waitOnExecutionContext } from "cloudflare:test";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { logFor } from "../src/accountlog";
import type { Env } from "../src/env";
import { gmailHookSig, handleOAuthCallback } from "../src/gmail";
import { GMAIL_RAW_MAX, RETRY_ATTEMPTS } from "../src/gmailsync";
import { dailyTick, handleFetch } from "../src/index";
import { sha256Hex } from "../src/rate";
import { deriveInternalSecret, signMac } from "../src/tokens";
import { ulid } from "../src/ulid";
import { SECRET, captureLogs, env as testEnv, resetDb } from "./helpers";

const SESSION = "test-session-secret";
const env = { ...testEnv, DEV_ACCOUNT: undefined, GOOGLE_PUBSUB_TOPIC: undefined } as unknown as Env;
const ALL = ["read", "draft", "send", "act", "junk"];
const CLIENT = { client_id: "1234.apps.googleusercontent.com", client_secret: "the-owners-secret" };

interface FakeMessage {
  id: string;
  raw: string;
  labels: string[];
  date: number;
  size?: number;
  fail?: number; // answer this status to messages.get
}

/** Google, as much of it as the sync reaches: one mailbox and everything it was asked. */
class FakeGoogle {
  messages: FakeMessage[] = [];
  /** historyId → message ids added at that point. */
  history: [number, string[]][] = [];
  cursor = 100;
  grantRevoked = false;
  accessExpired = false;
  historyGone = false;
  watchFails = false;
  down: number | null = null;
  gets: string[] = [];
  calls: string[] = [];
  tokenForms: URLSearchParams[] = [];
  historyPageSize = 1000;

  add(id: string, labels = ["INBOX"], date = 1_700_000_000_000, extra: Partial<FakeMessage> = {}): void {
    this.messages.push({ id, raw: `From: a@b.example\r\nSubject: ${id}\r\n\r\nbody of ${id}\r\n`, labels, date, ...extra });
  }

  arrive(id: string, labels = ["INBOX"]): void {
    this.add(id, labels, 1_800_000_000_000);
    this.cursor += 1;
    this.history.push([this.cursor, [id]]);
  }

  fetch = async (input: RequestInfo | URL, init?: RequestInit): Promise<Response> => {
    const url = new URL(input instanceof Request ? input.url : String(input));
    const json = (body: unknown, status = 200) => new Response(JSON.stringify(body), { status });
    this.calls.push(`${init?.method ?? "GET"} ${url.pathname}`);
    if (url.hostname === "oauth2.googleapis.com") {
      const form = new URLSearchParams(String(init?.body));
      this.tokenForms.push(form);
      if (this.grantRevoked) return json({ error: "invalid_grant" }, 400);
      return json(form.get("grant_type") === "authorization_code"
        ? { access_token: "at-code", refresh_token: "the-refresh-token" }
        : { access_token: "at-refreshed" });
    }
    if (url.hostname !== "gmail.googleapis.com") throw new Error(`unexpected fetch ${url}`);
    if (this.down !== null) return json({}, this.down);
    if (this.accessExpired && new Headers(init?.headers).get("authorization") === "Bearer at-code") return json({}, 401);
    const path = url.pathname.replace("/gmail/v1/users/me", "");
    if (path === "/profile") return json({ emailAddress: "Me@Gmail.example", historyId: String(this.cursor), messagesTotal: this.messages.length });
    if (this.watchFails && path === "/watch") return json({}, 404);
    if (path === "/watch") return json({ historyId: String(this.cursor), expiration: String(Date.now() + 7 * 86_400_000) });
    if (path === "/stop") return json({});
    if (path === "/history") {
      if (this.historyGone) return json({}, 404);
      const start = Number(url.searchParams.get("startHistoryId"));
      const records = this.history.filter(([h]) => h > start);
      const from = Number(url.searchParams.get("pageToken") ?? 0);
      const page = records.slice(from, from + this.historyPageSize);
      const next = from + this.historyPageSize < records.length ? { nextPageToken: String(from + this.historyPageSize) } : {};
      return json({
        historyId: String(this.cursor),
        history: page.map(([id, ids]) => ({ id: String(id), messagesAdded: ids.map((m) => ({ message: { id: m } })) })),
        ...next,
      });
    }
    if (path === "/messages") {
      const size = Number(url.searchParams.get("maxResults"));
      const from = Number(url.searchParams.get("pageToken") ?? 0);
      const page = this.messages.slice(from, from + size).map((m) => ({ id: m.id }));
      return json({ messages: page, ...(from + size < this.messages.length ? { nextPageToken: String(from + size) } : {}) });
    }
    const m = /^\/messages\/([^/]+)$/.exec(path);
    const msg = this.messages.find((x) => x.id === m?.[1]);
    if (!msg) return json({}, 404);
    if (url.searchParams.get("format") === "minimal") {
      return json({ id: msg.id, threadId: `t-${msg.id}`, labelIds: msg.labels, sizeEstimate: msg.size ?? msg.raw.length });
    }
    this.gets.push(msg.id);
    if (msg.fail) return json({}, msg.fail);
    const raw = btoa(msg.raw).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
    return json({ id: msg.id, threadId: `t-${msg.id}`, labelIds: msg.labels, internalDate: String(msg.date), raw });
  };
}

let google: FakeGoogle;
let acct: string;
let token: string;

async function call(path: string, init: RequestInit = {}): Promise<Response> {
  const ctx = createExecutionContext();
  const r = await handleFetch(new Request(`https://mail.example${path}`, init), env, ctx);
  await waitOnExecutionContext(ctx);
  return r;
}

async function asClient(path: string, method = "GET", body?: unknown, t = token): Promise<Response> {
  return call(path, { method, headers: { authorization: `Bearer ${t}`, "content-type": "application/json" }, ...(body === undefined ? {} : { body: JSON.stringify(body) }) });
}

async function tokenFor(who: string, scopes = ALL): Promise<string> {
  const t = await signMac("lm1", who, crypto.randomUUID(), SESSION);
  await call("/internal/tokens", {
    method: "PUT",
    headers: { "x-account-id": who, "x-internal-secret": await deriveInternalSecret(SECRET, who), "content-type": "application/json" },
    body: JSON.stringify({ hash: await sha256Hex(t), scopes }),
  });
  return t;
}

/** Everything the owner does: save the client, take the URL to a browser, and Google redirects back. */
async function connect(): Promise<URL> {
  expect((await asClient("/sync/gmail/client", "PUT", CLIENT)).status).toBe(200);
  const r = await asClient("/sync/gmail/authorize", "POST");
  expect(r.status).toBe(200);
  const consent = new URL(((await r.json()) as { url: string }).url);
  const back = await call(`/oauth/google/callback?code=the-code&state=${encodeURIComponent(consent.searchParams.get("state")!)}`);
  expect(back.status).toBe(200);
  expect(await back.text()).toContain("Gmail is connected");
  return consent;
}

const inObject = <T>(fn: (state: DurableObjectState) => T | Promise<T>) => runInDurableObject(logFor(env, acct) as never, (_, state) => fn(state));
const sql = (query: string, ...args: SqlStorageValue[]) => inObject((s) => void s.storage.sql.exec(query, ...args));
const alarmAt = () => inObject((s) => s.storage.getAlarm());
const alarmNow = () => inObject((s) => s.storage.setAlarm(Date.now()));

/**
 * One run of the alarm. A due alarm also fires by itself here, as it does for real, and it reads as
 * null while it runs, so a run that got there first counts once the next alarm is set.
 */
async function alarm(): Promise<void> {
  if (await runDurableObjectAlarm(logFor(env, acct) as never)) return;
  await nextAlarm();
}

async function nextAlarm(): Promise<number | null> {
  for (let i = 0; i < 60; i++) {
    const at = await alarmAt();
    if (at !== null) return at;
    await scheduler.wait(5);
  }
  return null;
}

/** Run alarms until the object has nothing due. */
async function settle(limit = 200): Promise<void> {
  for (let i = 0; i < limit; i++) {
    const at = await nextAlarm();
    if (at === null || at > Date.now() + 1000) return;
    await runDurableObjectAlarm(logFor(env, acct) as never);
  }
  throw new Error("the alarm never settled");
}

/** The access token from connecting has run out, as it has after any restart of the object. */
async function expireAccess(): Promise<void> {
  google.accessExpired = true;
  await sql("UPDATE gmail_conn SET poll_due = 1");
  await alarmNow();
  await alarm();
  await nextAlarm();
  await alarmNow();
}

async function status(): Promise<Record<string, any>> {
  return (await asClient("/sync/gmail")).json();
}

async function mail(): Promise<{ data: Record<string, any>; at: number }[]> {
  const r = await asClient("/sync");
  const body = (await r.json()) as { entries: { kind: string; data: Record<string, any>; at: number }[] };
  return body.entries.filter((e) => e.kind === "mail.received");
}

async function push(historyId: number, email = "me@gmail.example"): Promise<Response> {
  const data = btoa(JSON.stringify({ emailAddress: email, historyId }));
  return call(`/hooks/gmail/${acct}/${encodeURIComponent(await gmailHookSig(SECRET, acct))}`, { method: "POST", body: JSON.stringify({ message: { data } }) });
}

beforeEach(async () => {
  await resetDb();
  const listed = await env.RAW.list();
  if (listed.objects.length) await env.RAW.delete(listed.objects.map((o) => o.key));
  acct = `a${ulid().toLowerCase()}`;
  await env.DB.prepare("INSERT INTO accounts (id, email, status, updated_at) VALUES (?1, ?2, 'active', 1)").bind(acct, `${acct}@o.example`).run();
  token = await tokenFor(acct);
  google = new FakeGoogle();
  vi.stubGlobal("fetch", google.fetch);
});

afterEach(() => {
  vi.unstubAllGlobals();
});

describe("connecting", () => {
  it("asks only to read, and leaves no secret in storage or in what clients can see", async () => {
    const consent = await connect();
    expect(consent.searchParams.get("scope")).toBe("https://www.googleapis.com/auth/gmail.readonly");
    expect(consent.searchParams.get("redirect_uri")).toBe(`${env.PUBLIC_ORIGIN}/oauth/google/callback`);
    expect(consent.searchParams.get("access_type")).toBe("offline");
    expect(google.tokenForms[0]!.get("client_secret")).toBe(CLIENT.client_secret);

    const s = await status();
    expect(s).toMatchObject({ connected: true, email: "me@gmail.example", client_id: CLIENT.client_id });
    expect(s.hook_url).toContain(`/hooks/gmail/${acct}/`);
    expect(JSON.stringify(s)).not.toContain("the-owners-secret");
    expect(JSON.stringify(s)).not.toContain("the-refresh-token");

    const stored = await inObject((s) => JSON.stringify(s.storage.sql.exec("SELECT * FROM gmail_conn").toArray()));
    expect(stored).toContain(CLIENT.client_id);
    expect(stored).not.toContain("the-owners-secret");
    expect(stored).not.toContain("the-refresh-token");
    expect(JSON.stringify(await (await asClient("/sync")).json())).not.toContain("the-refresh-token");
  });

  it("unseals the stored grant when the access token runs out", async () => {
    await connect();
    await settle();
    await expireAccess();
    await settle();
    expect(await status()).toMatchObject({ last_error: null });
    const refresh = google.tokenForms.find((f) => f.get("grant_type") === "refresh_token");
    expect(refresh?.get("refresh_token")).toBe("the-refresh-token");
    expect(refresh?.get("client_secret")).toBe(CLIENT.client_secret);
  });

  it("refuses a used, expired or denied consent, and a state it cannot verify", async () => {
    expect((await asClient("/sync/gmail/client", "PUT", CLIENT)).status).toBe(200);
    const url = new URL(((await (await asClient("/sync/gmail/authorize", "POST")).json()) as { url: string }).url);
    const state = encodeURIComponent(url.searchParams.get("state")!);
    expect((await call(`/oauth/google/callback?error=access_denied&state=${state}`)).status).toBe(400);
    expect((await status()).connected).toBe(false);
    // the nonce went with that attempt, so the same state is no longer this object's to answer
    let container = 0;
    const proxy = (async () => {
      container++;
      return new Response(null, { status: 302 });
    }) as never;
    await handleOAuthCallback(new Request(`https://mail.example/oauth/google/callback?code=c&state=${state}`), env, { proxy });
    expect(container).toBe(1);
    expect((await status()).connected).toBe(false);

    const next = new URL(((await (await asClient("/sync/gmail/authorize", "POST")).json()) as { url: string }).url);
    const nextState = encodeURIComponent(next.searchParams.get("state")!);
    expect((await call(`/oauth/google/callback?code=c&state=${nextState}x`)).status).toBe(400);
    await sql("UPDATE gmail_conn SET nonce_at = 0");
    expect((await call(`/oauth/google/callback?code=c&state=${nextState}`)).status).toBe(400);
    expect((await status()).connected).toBe(false);
  });

  it("takes only a real client id, and only a full token may change the connection", async () => {
    expect((await asClient("/sync/gmail/client", "PUT", { ...CLIENT, client_id: "x" })).status).toBe(400);
    expect((await asClient("/sync/gmail/client", "PUT", { ...CLIENT, client_secret: "s" })).status).toBe(400);
    expect((await asClient("/sync/gmail/client", "PUT", { ...CLIENT, pubsub_topic: "nope" })).status).toBe(400);
    const agent = await tokenFor(acct, ["read", "act"]);
    expect((await asClient("/sync/gmail/client", "PUT", CLIENT, agent)).status).toBe(403);
    expect((await asClient("/sync/gmail/authorize", "POST", undefined, agent)).status).toBe(403);
    expect((await asClient("/sync/gmail", "DELETE", undefined, agent)).status).toBe(403);
    expect((await asClient("/sync/gmail", "GET", undefined, agent)).status).toBe(200);
    expect((await call("/sync/gmail")).status).toBe(401);
  });

  it("forgets the connection and stops the watch, keeping the mail", async () => {
    await connect();
    google.add("m1");
    await asClient("/sync/gmail/backfill", "POST", {});
    await settle();
    expect((await asClient("/sync/gmail", "DELETE")).status).toBe(200);
    expect(google.calls).toContain("POST /gmail/v1/users/me/stop");
    expect(await status()).toMatchObject({ client_configured: false, connected: false });
    expect(await mail()).toHaveLength(1);
  });
});

describe("the backfill", () => {
  it("brings the whole mailbox into the log, over many alarms, with what a client files it by", async () => {
    await connect();
    for (let i = 0; i < 180; i++) google.add(`old${i}`, i % 2 ? ["INBOX"] : ["CATEGORY_UPDATES", "Label_7"], 1_600_000_000_000 + i);
    google.add("sent", ["SENT"]);
    google.add("spam", ["SPAM"]);
    google.add("draft", ["DRAFT"]);
    const r = await asClient("/sync/gmail/backfill", "POST", {});
    expect(r.status).toBe(200);
    expect((await asClient("/sync/gmail/backfill", "POST", {})).status).toBe(409);
    await settle();

    const s = await status();
    expect(s.backfill).toMatchObject({ state: "done", listed: 183, imported: 182, skipped: 1, planned_total: 183 });
    expect(s).toMatchObject({ pending: 0, failed: 0, held: 182 });

    const entries = await mail();
    expect(entries).toHaveLength(182);
    const sent = entries.find((e) => e.data.gmail.id === "sent")!;
    expect(sent.data).toMatchObject({ source: "import", gmail: { thread: "t-sent", labels: ["SENT"] } });
    expect(Number(sent.data.expires_at)).toBeGreaterThan(Date.now());

    const raw = await asClient(`/sync/raw/${sent.data.ulid}`);
    expect(await raw.text()).toContain("Subject: sent");
    const object = await env.RAW.head(`raw/${acct}/${sent.data.ulid}`);
    expect(object?.customMetadata).toMatchObject({ source: "import", folder: "sent", gm_id: "sent" });
    // the container, while there is one, picks these up as it does routed mail
    expect(await env.RAW.head(`in/${acct}/${sent.data.ulid}`)).not.toBeNull();
    const spam = entries.find((e) => e.data.gmail.id === "spam")!;
    expect((await env.RAW.head(`raw/${acct}/${spam.data.ulid}`))?.customMetadata).toMatchObject({ junk_kind: "spam", archived_at: "1700000000000" });
  });

  it("stays within its budget in each alarm", async () => {
    await connect();
    await settle();
    for (let i = 0; i < 250; i++) google.add(`m${i}`);
    const { lines } = await captureLogs(async () => {
      await asClient("/sync/gmail/backfill", "POST", {});
      await settle();
    });
    const runs = lines.filter((l) => l.ev === "gmail.run" && Number(l.calls) > 0).map((l) => Number(l.calls));
    expect(runs.length).toBeGreaterThan(2);
    expect(Math.max(...runs)).toBeLessThanOrEqual(400);
    expect((await status()).backfill).toMatchObject({ state: "done", imported: 250 });
  });

  it("keeps forever when asked, and fetches nothing it already holds on a second run", async () => {
    await connect();
    google.add("m1");
    google.add("m2");
    await asClient("/sync/gmail/backfill", "POST", { keep_forever: true });
    await settle();
    expect((await mail())[0]!.data.expires_at).toBe("never");
    google.gets = [];
    await asClient("/sync/gmail/backfill", "POST", {});
    await settle();
    expect(google.gets).toEqual([]);
    expect((await status()).backfill).toMatchObject({ state: "done", imported: 0, duplicates: 2 });
    expect(await mail()).toHaveLength(2);
  });

  it("pauses, resumes and cancels", async () => {
    await connect();
    for (let i = 0; i < 150; i++) google.add(`m${i}`);
    await asClient("/sync/gmail/backfill", "POST", {});
    await asClient("/sync/gmail/backfill/pause", "POST");
    await settle();
    const part = (await status()).backfill.imported;
    await alarmNow();
    await settle();
    expect((await status()).backfill).toMatchObject({ state: "paused", imported: part });

    await asClient("/sync/gmail/backfill/resume", "POST");
    await settle();
    expect((await status()).backfill).toMatchObject({ state: "done", imported: 150 });

    google.add("late");
    google.down = 503; // so the alarm cannot finish it before the cancel lands
    await asClient("/sync/gmail/backfill", "POST", {});
    expect((await asClient("/sync/gmail/backfill/cancel", "POST")).status).toBe(200);
    google.down = null;
    await alarmNow();
    await settle();
    expect((await status()).backfill.state).toBe("cancelled");
    expect(google.gets).not.toContain("late");
    expect((await asClient("/sync/gmail/backfill/pause", "POST")).status).toBe(404);
    expect((await asClient("/sync/gmail/backfill/nonsense", "POST")).status).toBe(404);
  });

  it("does not let one bad message hold up the pages after it", async () => {
    await connect();
    await settle();
    google.add("broken", ["INBOX"], 1_700_000_000_000, { fail: 422 });
    for (let i = 0; i < 510; i++) google.add(`m${i}`);
    await asClient("/sync/gmail/backfill", "POST", {});
    await settle();
    expect(await status()).toMatchObject({ pending: 1, backfill: { state: "running", imported: 510 } });

    await sql("UPDATE gmail_queue SET next_at = 0");
    google.messages[0]!.fail = 0;
    await alarmNow();
    await settle();
    expect(await status()).toMatchObject({ pending: 0, backfill: { state: "done", imported: 511 } });
  });

  it("leaves out what is too big to hold, says so, and carries on", async () => {
    await connect();
    google.add("huge", ["INBOX"], 1_700_000_000_000, { size: GMAIL_RAW_MAX + 1 });
    google.add("fine");
    await asClient("/sync/gmail/backfill", "POST", {});
    await settle();
    expect(google.gets).toEqual(["fine"]);
    expect(await status()).toMatchObject({ failed: 1, backfill: { state: "done", oversize: 1, imported: 1 } });
  });
});

describe("new mail", () => {
  it("reaches the log from a push while no device is doing anything, once each", async () => {
    await connect();
    await settle();
    google.arrive("new1");
    expect((await push(google.cursor)).status).toBe(204);
    await settle();
    expect((await mail()).map((e) => [e.data.gmail.id, e.data.source])).toEqual([["new1", "gmail"]]);

    // Pub/Sub repeats itself; the same notification finds nothing to do
    await push(google.cursor);
    await settle();
    expect(await mail()).toHaveLength(1);
    expect(google.gets).toEqual(["new1"]);
    expect((await status()).last_push_at).toBeGreaterThan(0);
  });

  it("is found by the periodic poll when no push ever comes", async () => {
    await connect();
    await settle();
    google.arrive("quiet");
    await sql("UPDATE gmail_conn SET last_poll_at = 0");
    await alarmNow();
    await settle();
    expect((await mail()).map((e) => e.data.gmail.id)).toEqual(["quiet"]);
  });

  it("walks a long history across alarms without losing a message", async () => {
    await connect();
    await settle();
    google.historyPageSize = 1;
    for (let i = 0; i < 25; i++) google.arrive(`burst${i}`);
    await push(google.cursor);
    await settle();
    expect((await mail()).map((e) => e.data.gmail.id).sort()).toEqual(Array.from({ length: 25 }, (_, i) => `burst${i}`).sort());
  });

  it("recovers from a history gap by listing what may have been missed", async () => {
    await connect();
    google.add("held");
    await asClient("/sync/gmail/backfill", "POST", {});
    await settle();
    google.add("in-the-gap");
    google.cursor += 5;
    google.historyGone = true;
    await push(google.cursor);
    await settle();
    expect((await mail()).map((e) => e.data.gmail.id).sort()).toEqual(["held", "in-the-gap"]);
    expect(google.gets.filter((g) => g === "held")).toHaveLength(1);
    google.historyGone = false;
    expect(await status()).toMatchObject({ pending: 0, last_error: null });
  });

  it("ignores a push for another mailbox, and sends one for an unconnected account to the container", async () => {
    await connect();
    await settle();
    google.arrive("x");
    await push(google.cursor, "someone@else.example");
    await settle();
    expect(await mail()).toEqual([]);
    expect(await logFor(env, `${acct}-none`).gmailPush(undefined, "5", Date.now())).toBe(false);
  });
});

describe("when Google is not cooperating", () => {
  it("backs off an outage and picks up where it stopped", async () => {
    await connect();
    await settle();
    google.arrive("wait-for-it");
    google.down = 503;
    await push(google.cursor);
    await alarm();
    expect(await status()).toMatchObject({ last_error: "http_503" });
    expect((await alarmAt())! - Date.now()).toBeGreaterThan(30_000);

    google.down = null;
    await alarmNow();
    await settle();
    expect(await mail()).toHaveLength(1);
    expect(await status()).toMatchObject({ last_error: null });
  });

  it("treats an outage during a fetch as an outage, not the message's fault", async () => {
    await connect();
    await settle();
    google.add("flaky", ["INBOX"], 1_800_000_000_000, { fail: 503 });
    google.cursor += 1;
    google.history.push([google.cursor, ["flaky"]]);
    await push(google.cursor);
    await alarm();
    await nextAlarm();
    expect(await status()).toMatchObject({ last_error: "http_503", pending: 1 });
    const attempts = await inObject((s) => s.storage.sql.exec("SELECT attempts FROM gmail_queue").one().attempts);
    expect(attempts).toBe(0);

    google.messages[0]!.fail = 0;
    await alarmNow();
    await settle();
    expect(await mail()).toHaveLength(1);
  });

  it("stops on a revoked grant, says so, and a reconnect starts it again", async () => {
    await connect();
    await settle();
    google.arrive("after");
    google.grantRevoked = true;
    await expireAccess();
    await settle();
    expect(await status()).toMatchObject({ needs_reauth: true, last_error: "refresh_rejected:invalid_grant" });
    expect(await alarmAt()).toBeNull();
    expect((await asClient("/sync/gmail/backfill", "POST", {})).status).toBe(400);

    google.grantRevoked = false;
    google.accessExpired = false;
    await connect();
    await push(google.cursor);
    await settle();
    expect(await status()).toMatchObject({ needs_reauth: false, last_error: null });
    expect(await mail()).toHaveLength(1);
  });

  it("gives one bad message its own retries without holding up the rest", async () => {
    await connect();
    await settle();
    google.add("broken", ["INBOX"], 1_800_000_000_000, { fail: 422 });
    google.cursor += 1;
    google.history.push([google.cursor, ["broken"]]);
    google.arrive("good");
    await push(google.cursor);
    await settle();
    expect((await mail()).map((e) => e.data.gmail.id)).toEqual(["good"]);
    expect(await status()).toMatchObject({ pending: 1, failed: 0 });

    await sql("UPDATE gmail_queue SET attempts = ? WHERE gm_id = 'broken'", RETRY_ATTEMPTS);
    expect(await status()).toMatchObject({ pending: 0, failed: 1 });
    await asClient("/sync/gmail/backfill/retry-failed", "POST");
    expect(await status()).toMatchObject({ pending: 1, failed: 0 });
  });

  it("drops a message Gmail no longer has", async () => {
    await connect();
    await settle();
    google.cursor += 1;
    google.history.push([google.cursor, ["vanished"]]);
    await push(google.cursor);
    await settle();
    expect(await status()).toMatchObject({ pending: 0, failed: 0 });
    expect(await mail()).toEqual([]);
  });

  it("does not store a message twice when a run dies between R2 and the log", async () => {
    await connect();
    await settle();
    google.arrive("once");
    await push(google.cursor);
    await settle();
    const [entry] = await mail();
    await sql("DELETE FROM gmail_held");
    await sql("INSERT INTO gmail_queue (gm_id, source) VALUES ('once', 'gmail')");
    await alarmNow();
    await settle();
    expect((await env.RAW.list({ prefix: `raw/${acct}/` })).objects).toHaveLength(1);
    expect((await mail()).map((e) => e.data.ulid)).toEqual([entry!.data.ulid]);
  });

  it("restores the Gmail fields when the sweep has to rebuild an entry", async () => {
    await connect();
    await settle();
    google.arrive("swept", ["INBOX", "Label_1"]);
    await push(google.cursor);
    await settle();
    const [entry] = await mail();
    await sql("DELETE FROM log WHERE ulid = ?", entry!.data.ulid);
    await dailyTick(env, async () => {});
    expect((await mail())[0]!.data.gmail).toEqual({ id: "swept", thread: "t-swept", labels: ["INBOX"] });
  });
});

describe("the daily cron", () => {
  it("restores a lost alarm", async () => {
    await connect();
    await settle();
    await inObject((s) => s.storage.deleteAlarm());
    await dailyTick(env, async () => {});
    expect(await nextAlarm()).not.toBeNull();
  });
});

describe("the watch", () => {
  it("costs only speed when the topic does not work, and is not retried in every alarm", async () => {
    google.watchFails = true;
    await asClient("/sync/gmail/client", "PUT", { ...CLIENT, pubsub_topic: "projects/p/topics/wrong" });
    const url = new URL(((await (await asClient("/sync/gmail/authorize", "POST")).json()) as { url: string }).url);
    await call(`/oauth/google/callback?code=c&state=${encodeURIComponent(url.searchParams.get("state")!)}`);
    await settle();
    google.add("m1");
    await asClient("/sync/gmail/backfill", "POST", {});
    await settle();
    expect(await status()).toMatchObject({ last_error: null, backfill: { state: "done", imported: 1 } });
    expect(google.calls.filter((c) => c.endsWith("/watch"))).toHaveLength(1);
  });

  it("starts when a topic is set and renews near its end", async () => {
    await asClient("/sync/gmail/client", "PUT", { ...CLIENT, pubsub_topic: "projects/p/topics/gmail" });
    const url = new URL(((await (await asClient("/sync/gmail/authorize", "POST")).json()) as { url: string }).url);
    await call(`/oauth/google/callback?code=c&state=${encodeURIComponent(url.searchParams.get("state")!)}`);
    await settle();
    const s = await status();
    expect(s.pubsub_topic).toBe("projects/p/topics/gmail");
    expect(s.watch_expiration).toBeGreaterThan(Date.now());
    const watches = () => google.calls.filter((c) => c.endsWith("/watch")).length;
    expect(watches()).toBe(1);

    await alarmNow();
    await settle();
    expect(watches()).toBe(1);

    await sql("UPDATE gmail_conn SET watch_expiration = ?, watch_tried_at = 0", Date.now() + 86_400_000);
    await alarmNow();
    await settle();
    expect(watches()).toBe(2);
  });
});
