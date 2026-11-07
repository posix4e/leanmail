import { Buffer } from "node:buffer";
import type { MailEntry } from "./accountlog";
import { errText, log } from "./log";
import { b64uDecode, b64uEncode, deriveSecret } from "./tokens";
import { ulidFor } from "./ulid";

/**
 * Gmail into the account's log, run by the account's Durable Object (src/accountlog.ts) with no
 * client and no container. A client only starts OAuth; Google then talks to this object, so the
 * mail keeps coming while every device is off, there is one writer, and Google sees Cloudflare's
 * address rather than a device's.
 *
 * It does what lean/Server/Gmail/{OAuth,Poll,Backfill,Watch}.lean do, around one queue:
 *
 *   poll      `users.history.list` from the cursor adds new ids to gmail_queue, in the transaction
 *             that moves the cursor. A 404 for the cursor is a history gap (Gmail keeps about a
 *             week): every message since a day before the last poll is queued, and the cursor
 *             moves to the mailbox's current id.
 *   backfill  `users.messages.list` adds a page of ids, in the transaction that moves the page
 *             token, once the last page has been fetched. Its token is the only cursor, so a
 *             backfill survives anything.
 *   drain     takes rows off the queue, new mail first. A message is written to R2 under a ULID
 *             that its Gmail id fixes, so doing one twice writes the same object and the same
 *             log entry. The container, while there is one, reads it through the same `in/`
 *             marker and metadata as routed mail (lean/Server/Spool.lean), or its orphan walk
 *             would delete it after a day.
 *
 * One alarm does all three within a budget, then sets the next: at once while there is work, later
 * after a failure, and otherwise for the next periodic poll, which is what finds mail if Pub/Sub
 * is not set up or misses one. Failures are enum codes, so logs stay content-free.
 *
 * Secrets are sealed with AES-GCM under a key derived from SESSION_SECRET. That keeps them out of
 * a copy of this object's storage. It is no protection from Cloudflare, which holds both.
 *
 * Scope is `gmail.readonly`: nothing here changes the mailbox. The mirror, spam reports and sending
 * through Gmail are the container's for now, and would need a wider consent here.
 */

const TOKEN_URL = "https://oauth2.googleapis.com/token";
const AUTH_URL = "https://accounts.google.com/o/oauth2/v2/auth";
const API = "https://gmail.googleapis.com/gmail/v1/users/me";
const SCOPE = "https://www.googleapis.com/auth/gmail.readonly";

export const STATE_PURPOSE = "goauth";
const STATE_TTL_MS = 10 * 60_000;
const ACCESS_TTL_MS = 50 * 60_000;

/** Larger messages are left out and reported: the object holds base64 and bytes in 128 MB at once. */
export const GMAIL_RAW_MAX = 25 * 1024 * 1024;

/** Calls to Google and R2 in one alarm, four a message, and the time it may run. */
const ALARM_CALLS = 400;
const ALARM_MS = 5 * 60_000;
const HISTORY_PAGES = 10;
const GAP_PAGES = 10;
const GAP_MARGIN_MS = 86_400_000;
const LIST_PAGE = 500;

const RETRY_BASE_MS = 15 * 60_000;
const RETRY_MAX_MS = 6 * 3_600_000;
export const RETRY_ATTEMPTS = 24;

const STALL_BASE_MS = 60_000;
const STALL_MAX_MS = 3_600_000;

/** How often to look without being told: often when no watch is running, rarely as a backstop. */
const POLL_UNWATCHED_MS = 15 * 60_000;
const POLL_WATCHED_MS = 6 * 3_600_000;
const WATCH_RENEW_WITHIN_MS = 2 * 86_400_000;
const WATCH_RETRY_MS = 3_600_000;

export const GMAIL_SCHEMA = `
CREATE TABLE IF NOT EXISTS gmail_conn (
  id               INTEGER PRIMARY KEY CHECK (id = 1),
  acct             TEXT NOT NULL,
  client_id        TEXT NOT NULL,
  client_secret    TEXT NOT NULL,
  refresh_token    TEXT,
  email            TEXT,
  history_id       TEXT,
  topic            TEXT,
  watch_expiration INTEGER,
  watch_tried_at   INTEGER,
  nonce            TEXT,
  nonce_at         INTEGER,
  poll_due         INTEGER NOT NULL DEFAULT 0,
  last_push_at     INTEGER,
  last_poll_at     INTEGER,
  last_error       TEXT,
  stalls           INTEGER NOT NULL DEFAULT 0
);
CREATE TABLE IF NOT EXISTS gmail_held (gm_id TEXT PRIMARY KEY);
CREATE TABLE IF NOT EXISTS gmail_queue (
  gm_id    TEXT PRIMARY KEY,
  source   TEXT NOT NULL,
  attempts INTEGER NOT NULL DEFAULT 0,
  next_at  INTEGER NOT NULL DEFAULT 0,
  code     TEXT
);
CREATE TABLE IF NOT EXISTS gmail_backfill (
  id            INTEGER PRIMARY KEY CHECK (id = 1),
  state         TEXT NOT NULL,
  page_token    TEXT,
  listed_all    INTEGER NOT NULL DEFAULT 0,
  planned_total INTEGER,
  listed        INTEGER NOT NULL DEFAULT 0,
  imported      INTEGER NOT NULL DEFAULT 0,
  duplicates    INTEGER NOT NULL DEFAULT 0,
  skipped       INTEGER NOT NULL DEFAULT 0,
  oversize      INTEGER NOT NULL DEFAULT 0,
  bytes         INTEGER NOT NULL DEFAULT 0,
  expires_at    TEXT NOT NULL,
  started_at    INTEGER NOT NULL,
  finished_at   INTEGER
);
`;

export interface GmailEnv {
  RAW: R2Bucket;
  PUBLIC_ORIGIN: string;
  RETENTION_DAYS_DEFAULT: string;
  SESSION_SECRET?: string;
  GOOGLE_PUBSUB_TOPIC?: string;
}

interface Conn {
  acct: string;
  client_id: string;
  client_secret: string;
  refresh_token: string | null;
  email: string | null;
  history_id: string | null;
  topic: string | null;
  watch_expiration: number | null;
  watch_tried_at: number | null;
  nonce: string | null;
  nonce_at: number | null;
  poll_due: number;
  last_push_at: number | null;
  last_poll_at: number | null;
  last_error: string | null;
  stalls: number;
}

interface Backfill {
  state: string;
  page_token: string | null;
  listed_all: number;
  planned_total: number | null;
  listed: number;
  imported: number;
  duplicates: number;
  skipped: number;
  oversize: number;
  bytes: number;
  expires_at: string;
  started_at: number;
  finished_at: number | null;
}

class GoogleError extends Error {
  constructor(readonly code: string) {
    super(code);
  }
}

/** Trouble only the owner can clear; the object stops until they connect again. */
function needsAPerson(error: string | null): boolean {
  return !!error && (error.startsWith("refresh_rejected") || error === "insufficient_scope");
}

/** "Not now": the whole alarm stops and backs off. Anything else belongs to the one message. */
function transient(code: string): boolean {
  return ["network", "forbidden", "unauthorized", "http_429"].includes(code) || /^http_5|^token_/.test(code);
}

const enc = new TextEncoder();

async function sealKey(master: string | undefined, acct: string): Promise<CryptoKey> {
  if (!master) throw new GoogleError("not_configured");
  const raw = b64uDecode(await deriveSecret(master, `gmail-seal:${acct}`));
  return crypto.subtle.importKey("raw", raw, "AES-GCM", false, ["encrypt", "decrypt"]);
}

/** base64url(iv + ciphertext), bound to the account and the field it is stored in. */
export async function seal(master: string | undefined, acct: string, field: string, text: string): Promise<string> {
  const iv = crypto.getRandomValues(new Uint8Array(12));
  const key = await sealKey(master, acct);
  const ct = await crypto.subtle.encrypt({ name: "AES-GCM", iv, additionalData: enc.encode(field) }, key, enc.encode(text));
  const out = new Uint8Array(12 + ct.byteLength);
  out.set(iv);
  out.set(new Uint8Array(ct), 12);
  return b64uEncode(out);
}

async function unseal(master: string | undefined, acct: string, field: string, sealed: string): Promise<string> {
  const bytes = b64uDecode(sealed);
  const key = await sealKey(master, acct);
  const pt = await crypto.subtle.decrypt({ name: "AES-GCM", iv: bytes.subarray(0, 12), additionalData: enc.encode(field) }, key, bytes.subarray(12));
  return new TextDecoder().decode(pt);
}

/** Google writes `historyId` and `internalDate` as strings, and has written them as numbers. */
function strNum(v: unknown): string | null {
  if (typeof v === "string" && v) return v;
  return typeof v === "number" ? String(v) : null;
}

function messageIds(j: { messages?: { id?: unknown }[] }): string[] {
  return (j.messages ?? []).map((m) => m.id).filter((id): id is string => typeof id === "string" && id !== "");
}

export class GmailSync {
  private access: { token: string; until: number } | null = null;
  private calls = 0;
  private deadline = 0;

  constructor(
    private readonly ctx: DurableObjectState,
    private readonly env: GmailEnv,
    private readonly appendMail: (entry: MailEntry) => boolean,
  ) {}

  private get sql(): SqlStorage {
    return this.ctx.storage.sql;
  }

  private conn(): Conn | null {
    return this.sql.exec<Conn & Record<string, SqlStorageValue>>("SELECT * FROM gmail_conn WHERE id = 1").toArray()[0] ?? null;
  }

  private backfill(): Backfill | null {
    return this.sql.exec<Backfill & Record<string, SqlStorageValue>>("SELECT * FROM gmail_backfill WHERE id = 1").toArray()[0] ?? null;
  }

  private set(columns: string, ...values: SqlStorageValue[]): void {
    this.sql.exec(`UPDATE gmail_conn SET ${columns} WHERE id = 1`, ...values);
  }

  // ---- connecting ----

  /** Store the owner's OAuth client. Another client cannot use the refresh token, so that one starts over. */
  async saveClient(acct: string, clientId: string, secret: string, topic: string | null): Promise<void> {
    const sealed = await seal(this.env.SESSION_SECRET, acct, "client_secret", secret);
    const was = this.conn();
    if (!was) {
      this.sql.exec("INSERT INTO gmail_conn (id, acct, client_id, client_secret, topic) VALUES (1, ?, ?, ?, ?)", acct, clientId, sealed, topic);
    } else if (was.client_id === clientId) {
      this.set("client_secret = ?, topic = ?", sealed, topic);
    } else {
      this.set("client_id = ?, client_secret = ?, topic = ?, refresh_token = NULL, last_error = NULL", clientId, sealed, topic);
      this.access = null;
    }
    log("gmail.client_saved", { acct });
  }

  /** Google's consent URL for a `state` the caller has minted around `nonce`. */
  authorizeUrl(nonce: string, state: string, now: number): string | null {
    const c = this.conn();
    if (!c) return null;
    this.set("nonce = ?, nonce_at = ?", nonce, now);
    const q = new URLSearchParams({
      client_id: c.client_id,
      redirect_uri: this.redirectUri(),
      response_type: "code",
      scope: SCOPE,
      access_type: "offline",
      prompt: "consent",
      state,
    });
    return `${AUTH_URL}?${q}`;
  }

  private redirectUri(): string {
    return `${this.env.PUBLIC_ORIGIN.replace(/\/+$/, "")}/oauth/google/callback`;
  }

  /**
   * Google's redirect came back. Null when this object did not start that consent, which leaves it
   * to the container's own flow; otherwise the code of what went wrong, or "" for connected.
   */
  async finishAuthorize(nonce: string, code: string, now: number): Promise<string | null> {
    const c = this.conn();
    if (!c || c.nonce !== nonce) return null;
    this.set("nonce = NULL, nonce_at = NULL");
    if (now > (c.nonce_at ?? 0) + STATE_TTL_MS) return "expired";
    if (!code) return "denied";
    try {
      const j = await this.tokenCall({
        code,
        client_id: c.client_id,
        client_secret: await this.clientSecret(c),
        redirect_uri: this.redirectUri(),
        grant_type: "authorization_code",
      });
      if (!j.ok) return j.error;
      const { refresh_token: refresh, access_token: token } = j.body;
      if (typeof refresh !== "string" || typeof token !== "string") return "no_refresh_token";
      const p = (await this.api(token, "/profile")) as { emailAddress?: string; historyId?: unknown };
      const email = p.emailAddress?.toLowerCase();
      if (!email) return "no_profile";
      const sealed = await seal(this.env.SESSION_SECRET, c.acct, "refresh_token", refresh);
      // A different mailbox than before starts its cursor and its watch afresh.
      const hid = c.email === email ? (c.history_id ?? strNum(p.historyId)) : strNum(p.historyId);
      this.set("refresh_token = ?, email = ?, history_id = ?, watch_expiration = ?, last_error = NULL, stalls = 0",
        sealed, email, hid, c.email === email ? c.watch_expiration : null);
      this.access = { token, until: Date.now() + ACCESS_TTL_MS };
      await this.ctx.storage.setAlarm(now);
      log("gmail.connected", { acct: c.acct, same_mailbox: c.email === email });
      return "";
    } catch (e) {
      if (e instanceof GoogleError) return e.code;
      log("gmail.connect_failed", { acct: c.acct, error: errText(e) });
      return "failed";
    }
  }

  /** Forget the connection, stopping the watch while the token still works. Stored mail stays. */
  async forget(): Promise<void> {
    const c = this.conn();
    if (c?.refresh_token && !needsAPerson(c.last_error)) {
      try {
        await this.api(await this.accessToken(c), "/stop", { method: "POST" });
      } catch (e) {
        log("gmail.stop_watch_failed", { acct: c.acct, code: e instanceof GoogleError ? e.code : "error" });
      }
    }
    this.sql.exec("DELETE FROM gmail_conn");
    this.sql.exec("DELETE FROM gmail_queue");
    this.sql.exec("UPDATE gmail_backfill SET state = 'cancelled' WHERE state IN ('running', 'paused')");
    this.access = null;
    await this.ctx.storage.deleteAlarm();
    if (c) log("gmail.forgotten", { acct: c.acct });
  }

  status(): Record<string, unknown> {
    const c = this.conn();
    const count = (where: string): number =>
      this.sql.exec<{ n: number }>(`SELECT count(*) AS n FROM gmail_queue WHERE ${where}`, RETRY_ATTEMPTS).one().n;
    const b = this.backfill();
    return {
      client_configured: !!c,
      client_id: c?.client_id ?? null,
      connected: !!c?.refresh_token,
      needs_reauth: needsAPerson(c?.last_error ?? null),
      email: c?.email ?? null,
      pubsub_topic: c?.topic || this.env.GOOGLE_PUBSUB_TOPIC || null,
      watch_expiration: c?.watch_expiration ?? null,
      last_push_at: c?.last_push_at ?? null,
      last_poll_at: c?.last_poll_at ?? null,
      last_error: c?.last_error ?? null,
      held: this.sql.exec<{ n: number }>("SELECT count(*) AS n FROM gmail_held").one().n,
      pending: count("attempts < ?"),
      failed: count("attempts >= ?"),
      backfill: b && {
        state: b.state,
        planned_total: b.planned_total,
        listed: b.listed,
        imported: b.imported,
        duplicates: b.duplicates,
        skipped: b.skipped,
        oversize: b.oversize,
        bytes: b.bytes,
        keep_forever: b.expires_at === "never",
        started_at: b.started_at,
        finished_at: b.finished_at,
      },
    };
  }

  // ---- Google ----

  private async clientSecret(c: Conn): Promise<string> {
    return unseal(this.env.SESSION_SECRET, c.acct, "client_secret", c.client_secret);
  }

  private async tokenCall(form: Record<string, string>): Promise<{ ok: true; body: Record<string, unknown> } | { ok: false; error: string; status: number }> {
    this.calls++;
    let res: Response;
    try {
      res = await fetch(TOKEN_URL, { method: "POST", headers: { "content-type": "application/x-www-form-urlencoded" }, body: new URLSearchParams(form) });
    } catch {
      throw new GoogleError("network");
    }
    const body = (await res.json().catch(() => ({}))) as Record<string, unknown>;
    if (res.ok) return { ok: true, body };
    const why = typeof body.error === "string" ? body.error.slice(0, 40) : String(res.status);
    return { ok: false, error: why, status: res.status };
  }

  private async accessToken(c: Conn): Promise<string> {
    if (this.access && Date.now() < this.access.until) return this.access.token;
    if (!c.refresh_token) throw new GoogleError("not_authorized");
    const j = await this.tokenCall({
      client_id: c.client_id,
      client_secret: await this.clientSecret(c),
      refresh_token: await unseal(this.env.SESSION_SECRET, c.acct, "refresh_token", c.refresh_token),
      grant_type: "refresh_token",
    });
    if (!j.ok) {
      // invalid_grant: the seven-day expiry of a Testing consent screen, or consent revoked.
      throw new GoogleError(j.status === 400 || j.status === 401 ? `refresh_rejected:${j.error}` : `token_${j.status}`);
    }
    if (typeof j.body.access_token !== "string") throw new GoogleError("token_no_access_token");
    this.access = { token: j.body.access_token, until: Date.now() + ACCESS_TTL_MS };
    return j.body.access_token;
  }

  private async api(token: string, path: string, init: RequestInit = {}): Promise<unknown> {
    this.calls++;
    let res: Response;
    try {
      res = await fetch(API + path, { ...init, headers: { authorization: `Bearer ${token}`, ...init.headers } });
    } catch {
      throw new GoogleError("network");
    }
    if (res.status === 401) {
      await res.body?.cancel();
      this.access = null;
      throw new GoogleError("unauthorized");
    }
    if (res.status === 403) {
      const t = await res.text();
      throw new GoogleError(/insufficientPermissions|ACCESS_TOKEN_SCOPE_INSUFFICIENT/.test(t) ? "insufficient_scope" : "forbidden");
    }
    if (!res.ok) {
      await res.body?.cancel();
      throw new GoogleError(`http_${res.status}`);
    }
    return res.json().catch(() => null);
  }

  // ---- pushes and the alarm ----

  /** A Pub/Sub notification. False when this object has no connection, which leaves it to the container. */
  async push(email: string | undefined, historyId: string | undefined, now: number): Promise<boolean> {
    const c = this.conn();
    if (!c?.refresh_token) return false;
    if (email && c.email && email.toLowerCase() !== c.email) {
      log("gmail.push_ignored", { acct: c.acct, reason: "other_mailbox" });
      return true;
    }
    this.set("last_push_at = ?", now);
    if (!c.history_id) {
      // No cursor yet: adopt this one rather than replaying the mailbox.
      if (historyId) this.set("history_id = ?", historyId);
      return true;
    }
    if (!needsAPerson(c.last_error)) {
      this.set("poll_due = 1");
      await this.ctx.storage.setAlarm(now);
    }
    return true;
  }

  /** The daily cron's check: a connected account always has an alarm set. */
  async ensureAlarm(now: number): Promise<void> {
    const c = this.conn();
    if (c?.refresh_token && (await this.ctx.storage.getAlarm()) === null) await this.ctx.storage.setAlarm(now);
  }

  /** One alarm. It catches everything, because the next alarm is set only at the end of this one. */
  async run(now: number): Promise<void> {
    const c = this.conn();
    if (!c?.refresh_token || needsAPerson(c.last_error)) return;
    this.calls = 0;
    this.deadline = Date.now() + ALARM_MS;
    let stopped: string | null = null;
    try {
      const token = await this.accessToken(c);
      await this.renewWatch(c, token, now);
      const every = (c.watch_expiration ?? 0) > now ? POLL_WATCHED_MS : POLL_UNWATCHED_MS;
      if (c.poll_due || !c.history_id || now >= (c.last_poll_at ?? 0) + every) await this.poll(c, token, now);
      await this.drain(token, now);
    } catch (e) {
      stopped = e instanceof GoogleError ? e.code : "storage";
      if (!(e instanceof GoogleError)) log("gmail.run_failed", { acct: c.acct, error: errText(e) });
    }
    log("gmail.run", { acct: c.acct, calls: this.calls, stopped });
    if (stopped === null) {
      this.set("last_error = NULL, stalls = 0");
    } else {
      this.set("last_error = ?, stalls = stalls + 1", stopped);
    }
    if (needsAPerson(stopped) || !this.conn()) {
      await this.ctx.storage.deleteAlarm();
      return;
    }
    await this.ctx.storage.setAlarm(this.nextRun(now));
  }

  private nextRun(now: number): number {
    const c = this.conn()!;
    if (c.stalls > 0) return now + Math.min(STALL_BASE_MS * 2 ** (c.stalls - 1), STALL_MAX_MS);
    if (c.poll_due) return now;
    const bf = this.backfill();
    const running = bf?.state === "running";
    if (running && !bf!.listed_all) return now;
    const due = this.sql
      .exec<{ at: number | null }>(
        `SELECT MIN(next_at) AS at FROM gmail_queue WHERE attempts < ? AND (source != 'import' OR ?)`,
        RETRY_ATTEMPTS,
        running ? 1 : 0,
      )
      .one().at;
    const watched = (c.watch_expiration ?? 0) > now;
    const poll = (c.last_poll_at ?? now) + (watched ? POLL_WATCHED_MS : POLL_UNWATCHED_MS);
    return due === null ? poll : Math.max(now, Math.min(due, poll));
  }

  /** Push is a shortcut, so a topic that does not work costs a slower poll and stops nothing else. */
  private async renewWatch(c: Conn, token: string, now: number): Promise<void> {
    const topic = c.topic || this.env.GOOGLE_PUBSUB_TOPIC;
    if (!topic || (c.watch_expiration ?? 0) > now + WATCH_RENEW_WITHIN_MS) return;
    if (now < (c.watch_tried_at ?? 0) + WATCH_RETRY_MS) return;
    this.set("watch_tried_at = ?", now);
    let j: { expiration?: unknown; historyId?: unknown };
    try {
      j = (await this.api(token, "/watch", {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify({ topicName: topic }),
      })) as typeof j;
    } catch (e) {
      if (!(e instanceof GoogleError) || needsAPerson(e.code)) throw e;
      log("gmail.watch_failed", { acct: c.acct, code: e.code });
      return;
    }
    const expiration = Number(strNum(j.expiration)) || null;
    // Seed the cursor only the first time: overwriting it would skip what the poll has not read.
    c.watch_expiration = expiration;
    c.history_id ??= strNum(j.historyId);
    this.set("watch_expiration = ?, history_id = ?", expiration, c.history_id);
  }

  // ---- the poll ----

  private enqueue(ids: string[], source: string): void {
    for (const id of ids) {
      this.sql.exec(
        `INSERT INTO gmail_queue (gm_id, source) SELECT ?1, ?2 WHERE NOT EXISTS (SELECT 1 FROM gmail_held WHERE gm_id = ?1)
         ON CONFLICT(gm_id) DO NOTHING`,
        id,
        source,
      );
    }
  }

  private async poll(c: Conn, token: string, now: number): Promise<void> {
    // Cleared first: a push that lands during the awaits below sets it again and is not lost.
    this.set("poll_due = 0");
    try {
      if (!c.history_id) {
        const p = (await this.api(token, "/profile")) as { historyId?: unknown };
        this.set("history_id = ?, last_poll_at = ?", strNum(p.historyId), now);
        return;
      }
      await this.readHistory(c, token);
      this.set("last_poll_at = ?", now);
    } catch (e) {
      if (e instanceof GoogleError && e.code === "http_404") {
        await this.recoverCursor(c, token, now);
        this.set("last_poll_at = ?", now);
        return;
      }
      this.set("poll_due = 1");
      throw e;
    }
  }

  private async readHistory(c: Conn, token: string): Promise<void> {
    const start = c.history_id!;
    const ids = new Set<string>();
    let pageToken: string | undefined;
    let mailboxNow: string | null = null;
    let lastRecord: string | null = null;
    for (let i = 0; i < HISTORY_PAGES; i++) {
      const q = new URLSearchParams({ startHistoryId: start, historyTypes: "messageAdded" });
      if (pageToken) q.set("pageToken", pageToken);
      const j = (await this.api(token, `/history?${q}`)) as {
        historyId?: unknown;
        nextPageToken?: string;
        history?: { id?: unknown; messagesAdded?: { message?: { id?: unknown } }[] }[];
      };
      mailboxNow = strNum(j.historyId) ?? mailboxNow;
      for (const h of j.history ?? []) {
        lastRecord = strNum(h.id) ?? lastRecord;
        for (const m of h.messagesAdded ?? []) if (typeof m.message?.id === "string") ids.add(m.message.id);
      }
      pageToken = j.nextPageToken;
      if (!pageToken) break;
    }
    // Out of pages with more to come: only as far as was read, and straight back for the rest.
    const next = pageToken ? lastRecord : mailboxNow;
    this.ctx.storage.transactionSync(() => {
      this.enqueue([...ids], "gmail");
      if (next) this.set("history_id = ?", next);
      if (pageToken && next) this.set("poll_due = 1");
    });
  }

  /** A cursor Gmail no longer has: queue what may have been missed, then take its position now. */
  private async recoverCursor(c: Conn, token: string, now: number): Promise<void> {
    const p = (await this.api(token, "/profile")) as { historyId?: unknown };
    const since = Math.floor((Math.min(now, c.last_poll_at ?? now) - GAP_MARGIN_MS) / 1000);
    const ids: string[] = [];
    let pageToken: string | undefined;
    for (let i = 0; i < GAP_PAGES; i++) {
      const q = new URLSearchParams({ q: `after:${since}`, maxResults: String(LIST_PAGE), includeSpamTrash: "true" });
      if (pageToken) q.set("pageToken", pageToken);
      const j = (await this.api(token, `/messages?${q}`)) as { messages?: { id?: unknown }[]; nextPageToken?: string };
      ids.push(...messageIds(j));
      pageToken = j.nextPageToken;
      if (!pageToken) break;
    }
    this.ctx.storage.transactionSync(() => {
      this.enqueue(ids, "gmail");
      this.set("history_id = ?", strNum(p.historyId));
    });
    log("gmail.history_gap", { acct: c.acct, queued: ids.length });
  }

  // ---- the backfill ----

  /** Start one, unless one is running: "" or the reason. */
  async startBackfill(keepForever: boolean, now: number): Promise<string> {
    const c = this.conn();
    if (!c) return "no_client";
    if (!c.refresh_token || needsAPerson(c.last_error)) return "not_authorized";
    const b = this.backfill();
    if (b && (b.state === "running" || b.state === "paused")) return "already_running";
    let total: number | null = null;
    try {
      const p = (await this.api(await this.accessToken(c), "/profile")) as { messagesTotal?: unknown };
      total = Number(strNum(p.messagesTotal)) || null;
    } catch {
      // Only the progress figure needs it.
    }
    // Retention counts from today, never from a message's own date: old mail must not expire on arrival.
    const days = Number(this.env.RETENTION_DAYS_DEFAULT) || 90;
    const expires = keepForever ? "never" : String(now + days * 86_400_000);
    this.sql.exec(
      `INSERT OR REPLACE INTO gmail_backfill (id, state, planned_total, expires_at, started_at) VALUES (1, 'running', ?, ?, ?)`,
      total,
      expires,
      now,
    );
    await this.ctx.storage.setAlarm(now);
    log("gmail.backfill_start", { acct: c.acct, total, keep_forever: keepForever });
    return "";
  }

  /** pause, resume, cancel or retry-failed: "" or the reason. */
  async controlBackfill(action: string, now: number): Promise<string> {
    if (action === "retry-failed") {
      this.sql.exec("UPDATE gmail_queue SET attempts = 0, next_at = 0, code = NULL WHERE attempts >= ?", RETRY_ATTEMPTS);
    } else {
      const b = this.backfill();
      if (!b || (b.state !== "running" && b.state !== "paused")) return "not_found";
      const state = action === "pause" ? "paused" : action === "resume" ? "running" : "cancelled";
      this.sql.exec("UPDATE gmail_backfill SET state = ? WHERE id = 1", state);
      if (state === "cancelled") {
        this.sql.exec("UPDATE gmail_backfill SET finished_at = ? WHERE id = 1", now);
        this.sql.exec("DELETE FROM gmail_queue WHERE source = 'import'");
      }
    }
    const c = this.conn();
    if (c?.refresh_token && !needsAPerson(c.last_error)) await this.ctx.storage.setAlarm(now);
    return "";
  }

  /**
   * The next page of the mailbox, once every id of the last one has been tried. A message waiting
   * on a retry does not hold up the pages after it, only the end. False when none was added.
   */
  private async listBackfill(token: string, now: number): Promise<boolean> {
    const b = this.backfill();
    if (b?.state !== "running") return false;
    const left = (where: string) => this.sql.exec<{ n: number }>(`SELECT count(*) AS n FROM gmail_queue WHERE source = 'import' AND ${where}`, RETRY_ATTEMPTS).one().n;
    if (left("attempts = 0 AND ? > 0") > 0) return false;
    if (b.listed_all) {
      if (left("attempts < ?") > 0) return false;
      this.sql.exec("UPDATE gmail_backfill SET state = 'done', finished_at = ? WHERE id = 1", now);
      log("gmail.backfill_done", { imported: b.imported });
      return false;
    }
    const q = new URLSearchParams({ maxResults: String(LIST_PAGE), includeSpamTrash: "true" });
    if (b.page_token) q.set("pageToken", b.page_token);
    const j = (await this.api(token, `/messages?${q}`)) as { messages?: { id?: unknown }[]; nextPageToken?: string };
    const ids = messageIds(j);
    const held = (id: string) => this.sql.exec("SELECT 1 FROM gmail_held WHERE gm_id = ?", id).toArray().length > 0;
    this.ctx.storage.transactionSync(() => {
      const duplicates = ids.filter(held).length;
      this.enqueue(ids, "import");
      this.sql.exec(
        "UPDATE gmail_backfill SET page_token = ?, listed_all = ?, listed = listed + ?, duplicates = duplicates + ? WHERE id = 1",
        j.nextPageToken ?? null,
        j.nextPageToken ? 0 : 1,
        ids.length,
        duplicates,
      );
    });
    return true;
  }

  // ---- the drain ----

  private spent(): boolean {
    return this.calls + 4 > ALARM_CALLS || Date.now() > this.deadline;
  }

  private async drain(token: string, now: number): Promise<void> {
    for (;;) {
      if (this.spent()) return;
      const backfilling = this.backfill()?.state === "running" ? 1 : 0;
      const rows = this.sql
        .exec<{ gm_id: string; source: string; attempts: number }>(
          `SELECT gm_id, source, attempts FROM gmail_queue
           WHERE attempts < ?1 AND next_at <= ?2 AND (source != 'import' OR ?3)
           ORDER BY source = 'import', next_at LIMIT 25`,
          RETRY_ATTEMPTS,
          now,
          backfilling,
        )
        .toArray();
      if (rows.length === 0) {
        if (!(await this.listBackfill(token, now))) return;
        continue;
      }
      for (const row of rows) {
        if (this.spent()) return;
        await this.take(token, row, now);
      }
    }
  }

  private count(column: string, source: string, by = 1): void {
    if (source === "import") this.sql.exec(`UPDATE gmail_backfill SET ${column} = ${column} + ? WHERE id = 1`, by);
  }

  private async take(token: string, row: { gm_id: string; source: string; attempts: number }, now: number): Promise<void> {
    const { gm_id: id, source } = row;
    const done = (column: string) => {
      this.sql.exec("DELETE FROM gmail_queue WHERE gm_id = ?", id);
      this.count(column, source);
    };
    try {
      const path = `/messages/${encodeURIComponent(id)}`;
      // Size first: the raw form has to fit in memory twice.
      const meta = (await this.api(token, `${path}?format=minimal`)) as { labelIds?: unknown; sizeEstimate?: unknown };
      const labels = Array.isArray(meta.labelIds) ? meta.labelIds.filter((l): l is string => typeof l === "string") : [];
      if (labels.includes("DRAFT")) return done("skipped");
      if (Number(meta.sizeEstimate) > GMAIL_RAW_MAX) {
        this.sql.exec("UPDATE gmail_queue SET attempts = ?, code = 'oversize' WHERE gm_id = ?", RETRY_ATTEMPTS, id);
        this.count("oversize", source);
        return;
      }
      const m = (await this.api(token, `${path}?format=raw`)) as { raw?: unknown; internalDate?: unknown; threadId?: unknown };
      if (typeof m.raw !== "string") throw new GoogleError("no_raw");
      const bytes = Buffer.from(m.raw, "base64url");
      const receivedAt = Number(strNum(m.internalDate)) || now;
      const thread = typeof m.threadId === "string" ? m.threadId : undefined;
      const expires = source === "import" ? this.backfill()?.expires_at : undefined;
      const ulid = await this.store(id, thread, labels, bytes, receivedAt, source, expires);
      this.ctx.storage.transactionSync(() => {
        this.appendMail({
          ulid,
          at: receivedAt,
          size: bytes.byteLength,
          source,
          gmail: { id, thread: thread ?? null, labels },
          ...(expires ? { expires_at: expires } : {}),
        });
        this.sql.exec("INSERT OR IGNORE INTO gmail_held (gm_id) VALUES (?)", id);
        done("imported");
        this.count("bytes", source, bytes.byteLength);
      });
    } catch (e) {
      if (!(e instanceof GoogleError) || transient(e.code) || needsAPerson(e.code)) throw e;
      // Gmail has no such message: a 404, or the 400 it gives for an id it cannot parse.
      if (e.code === "http_404" || e.code === "http_400") return done("skipped");
      const wait = Math.min(RETRY_BASE_MS * 2 ** Math.min(row.attempts, 5), RETRY_MAX_MS);
      this.sql.exec("UPDATE gmail_queue SET attempts = attempts + 1, next_at = ?, code = ? WHERE gm_id = ?", now + wait, e.code.slice(0, 60), id);
    }
  }

  /** The object and its marker. The metadata is what lean/Server/Spool.lean reads, plus the gm_ keys the sweep reads. */
  private async store(id: string, thread: string | undefined, labels: string[], bytes: Uint8Array, receivedAt: number, source: string, expires: string | undefined): Promise<string> {
    const acct = this.conn()!.acct;
    const ulid = await ulidFor(receivedAt, `gmail:${acct}:${id}`);
    const inbox = labels.includes("INBOX");
    const sent = labels.includes("SENT") && !inbox;
    const metadata: Record<string, string> = {
      received_at: String(receivedAt),
      source,
      size: String(bytes.byteLength),
      folder: sent ? "sent" : "inbox",
      gm_id: id,
      // System labels only: R2 keeps 2 KB of metadata, and the log entry has them all.
      gm_labels: labels.filter((l) => /^[A-Z_]+$/.test(l)).join(","),
    };
    if (thread) metadata.gm_thread = thread;
    if (!inbox && !sent) metadata.archived_at = String(receivedAt);
    if (labels.includes("SPAM")) metadata.junk_kind = "spam";
    if (expires) metadata.expires_at = expires;
    this.calls += 2;
    await this.env.RAW.put(`raw/${acct}/${ulid}`, bytes, { customMetadata: metadata });
    await this.env.RAW.put(`in/${acct}/${ulid}`, new Uint8Array(0));
    return ulid;
  }
}
