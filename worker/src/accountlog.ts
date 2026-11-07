import { DurableObject } from "cloudflare:workers";
import { GMAIL_SCHEMA, GmailSync, type GmailEnv } from "./gmailsync";
import { ULID_RE, ulidTime } from "./ulid";

/**
 * One account's ordered history, the Durable Object named `<acct>` on the LOG binding. R2 holds the
 * mail; this object holds the order. Every client (the app, the CLI) keeps its own index and asks
 * `GET /sync?after=<seq>` (src/sync.ts) for what it has not seen.
 *
 * A seq is handed out when an entry is written, so a client that has read up to seq N cannot miss an
 * entry that lands later, as it could listing R2 by ULID: a ULID is minted before the object is
 * written, and a Gmail one carries the date the mail was received.
 *
 * Besides the log it holds the two things no client may own:
 *   settings  the synced account settings, each key with its own version. Every change also adds a
 *             `settings.changed` entry, so a client that replays one it already has compares versions.
 *             Secrets never come here: the log is read back by every device, for good.
 *   tokens    the API tokens this account's `/sync` accepts, by SHA-256. Unknown means refused.
 *   codes     the live sign-in codes, keyed hashes only (src/auth.ts).
 *
 * It also runs the account's Gmail sync (src/gmailsync.ts), on this object's one alarm.
 */

export interface MailEntry {
  ulid: string;
  /** Milliseconds; the message's received time. */
  at: number;
  rcpt?: string;
  from?: string;
  size?: number;
  source?: string;
  /** Mail that came from Gmail: what a client files it by. */
  gmail?: { id: string; thread: string | null; labels: string[] };
  /** Epoch ms or `never`, when it is not the account's own retention (a backfill). */
  expires_at?: string;
}

/**
 * What an entry or a setting holds: plain JSON, as JSON.parse returns it. Not spelled out as a
 * recursive type, because TypeScript cannot expand one through the RPC stub's types.
 */
export type Json = any;

export interface LogEntry {
  seq: number;
  kind: string;
  at: number;
  data: { [key: string]: Json };
}

export interface Setting {
  value: Json;
  version: number;
  updated_at: number;
}

export interface LogPage {
  entries: LogEntry[];
  /** The newest seq in the log; 0 when it is empty. */
  head: number;
  /** Whether a sweep of R2 has ever finished, which is what fills the log from older mail. */
  swept: boolean;
}

const SCHEMA = `
CREATE TABLE IF NOT EXISTS log (
  seq  INTEGER PRIMARY KEY AUTOINCREMENT,
  kind TEXT NOT NULL,
  ulid TEXT UNIQUE,
  at   INTEGER NOT NULL,
  data TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS settings (
  key        TEXT PRIMARY KEY,
  value      TEXT NOT NULL,
  version    INTEGER NOT NULL,
  updated_at INTEGER NOT NULL
);
CREATE TABLE IF NOT EXISTS tokens (
  hash       TEXT PRIMARY KEY,
  name       TEXT NOT NULL DEFAULT '',
  scopes     TEXT NOT NULL,
  created_at INTEGER NOT NULL,
  revoked_at INTEGER
);
CREATE TABLE IF NOT EXISTS codes (
  hash       TEXT PRIMARY KEY,
  expires_at INTEGER NOT NULL,
  attempts   INTEGER NOT NULL DEFAULT 0
);
CREATE TABLE IF NOT EXISTS meta (
  key   TEXT PRIMARY KEY,
  value TEXT NOT NULL
);
`;

const LIST_PAGE = 1000;
/** Unexpired sign-in codes an account may hold; a request beyond that sends nothing. */
const MAX_LIVE_CODES = 3;
/** Wrong guesses a code survives. */
export const MAX_CODE_ATTEMPTS = 5;

export interface TokenRow {
  id: string;
  name: string;
  scopes: string[];
  created_at: number;
  revoked_at: number | null;
}

export class AccountLog extends DurableObject<GmailEnv> {
  readonly #gmail: GmailSync;

  constructor(ctx: DurableObjectState, env: GmailEnv) {
    super(ctx, env);
    ctx.storage.sql.exec(SCHEMA + GMAIL_SCHEMA);
    this.#gmail = new GmailSync(ctx, env, (entry) => this.appendMail(entry));
  }

  override alarm(): Promise<void> {
    return this.#gmail.run(Date.now());
  }

  /** JSON text: its shape is too open for TypeScript to carry through the RPC stub. */
  gmailStatus(): string {
    return JSON.stringify(this.#gmail.status());
  }

  gmailSaveClient(acct: string, clientId: string, secret: string, topic: string | null): Promise<void> {
    return this.#gmail.saveClient(acct, clientId, secret, topic);
  }

  gmailAuthorizeUrl(nonce: string, state: string, now: number): string | null {
    return this.#gmail.authorizeUrl(nonce, state, now);
  }

  gmailFinishAuthorize(nonce: string, code: string, now: number): Promise<string | null> {
    return this.#gmail.finishAuthorize(nonce, code, now);
  }

  gmailForget(): Promise<void> {
    return this.#gmail.forget();
  }

  gmailPush(email: string | undefined, historyId: string | undefined, now: number): Promise<boolean> {
    return this.#gmail.push(email, historyId, now);
  }

  gmailEnsureAlarm(now: number): Promise<void> {
    return this.#gmail.ensureAlarm(now);
  }

  gmailStartBackfill(keepForever: boolean, now: number): Promise<string> {
    return this.#gmail.startBackfill(keepForever, now);
  }

  gmailControlBackfill(action: string, now: number): Promise<string> {
    return this.#gmail.controlBackfill(action, now);
  }

  private get sql(): SqlStorage {
    return this.ctx.storage.sql;
  }

  /** Add a message once, by ULID. True when it was new. Delivery and the sweep both come here. */
  appendMail(entry: MailEntry): boolean {
    const { ulid, at, ...rest } = entry;
    const written = this.sql.exec(
      "INSERT INTO log (kind, ulid, at, data) VALUES ('mail.received', ?, ?, ?) ON CONFLICT(ulid) DO NOTHING",
      ulid,
      at,
      JSON.stringify({ ulid, ...rest }),
    ).rowsWritten;
    return written > 0;
  }

  /** Entries after `after`, oldest first, at most `limit`. */
  since(after: number, limit: number): LogPage {
    const rows = this.sql
      .exec<{ seq: number; kind: string; at: number; data: string }>(
        "SELECT seq, kind, at, data FROM log WHERE seq > ? ORDER BY seq LIMIT ?",
        after,
        limit,
      )
      .toArray();
    const head = this.sql.exec<{ head: number | null }>("SELECT MAX(seq) AS head FROM log").one().head ?? 0;
    const swept = this.sql.exec("SELECT 1 FROM meta WHERE key = 'swept_at'").toArray().length > 0;
    return { entries: rows.map((r) => ({ ...r, data: JSON.parse(r.data) as LogEntry["data"] })), head, swept };
  }

  settings(): Record<string, Setting> {
    const out: Record<string, Setting> = {};
    const rows = this.sql.exec<{ key: string; value: string; version: number; updated_at: number }>(
      "SELECT key, value, version, updated_at FROM settings",
    );
    for (const r of rows) out[r.key] = { value: JSON.parse(r.value) as Json, version: r.version, updated_at: r.updated_at };
    return out;
  }

  /**
   * Set some keys: the last write to reach this object wins, key by key. A value that is already
   * set changes nothing and adds no entry, so a client saving a whole form stays quiet.
   */
  putSettings(changes: Record<string, Json>, now: number): Record<string, Setting> {
    this.ctx.storage.transactionSync(() => {
      for (const [key, value] of Object.entries(changes)) {
        const text = JSON.stringify(value);
        const was = this.sql.exec<{ value: string }>("SELECT value FROM settings WHERE key = ?", key).toArray()[0];
        if (was?.value === text) continue;
        const { version } = this.sql
          .exec<{ version: number }>(
            `INSERT INTO settings (key, value, version, updated_at) VALUES (?1, ?2, 1, ?3)
             ON CONFLICT(key) DO UPDATE SET value = ?2, version = version + 1, updated_at = ?3
             RETURNING version`,
            key,
            text,
            now,
          )
          .one();
        this.sql.exec(
          "INSERT INTO log (kind, at, data) VALUES ('settings.changed', ?, ?)",
          now,
          JSON.stringify({ key, value, version }),
        );
      }
    });
    return this.settings();
  }

  /** Register a token. Pushed after minting, so one already revoked stays revoked. */
  putToken(hash: string, scopes: string[], now: number, name: string = ""): void {
    this.sql.exec(
      `INSERT INTO tokens (hash, name, scopes, created_at) VALUES (?, ?, ?, ?)
       ON CONFLICT(hash) DO UPDATE SET scopes = excluded.scopes`,
      hash,
      name,
      JSON.stringify(scopes),
      now,
    );
  }

  /** Newest first; a token's id is its hash, which only ever names it. */
  listTokens(): TokenRow[] {
    return this.sql
      .exec<{ hash: string; name: string; scopes: string; created_at: number; revoked_at: number | null }>(
        "SELECT hash, name, scopes, created_at, revoked_at FROM tokens ORDER BY created_at DESC, hash",
      )
      .toArray()
      .map((r) => ({ id: r.hash, name: r.name, scopes: JSON.parse(r.scopes) as string[], created_at: r.created_at, revoked_at: r.revoked_at }));
  }

  /** Keep a sign-in code's hash; false when the account already holds as many as it may. */
  addCode(hash: string, now: number, expiresAt: number): boolean {
    // A code a guesser used up does not count, or five guesses at each would stop the owner signing in.
    this.sql.exec("DELETE FROM codes WHERE expires_at <= ? OR attempts >= ?", now, MAX_CODE_ATTEMPTS);
    const { n } = this.sql.exec<{ n: number }>("SELECT count(*) AS n FROM codes").one();
    if (n >= MAX_LIVE_CODES) return false;
    this.sql.exec("INSERT OR REPLACE INTO codes (hash, expires_at) VALUES (?, ?)", hash, expiresAt);
    return true;
  }

  /** Spend a live code: true once. A wrong one costs every live code a try. */
  spendCode(hash: string, now: number): boolean {
    const spent = this.sql.exec("DELETE FROM codes WHERE hash = ? AND expires_at > ? AND attempts < ?", hash, now, MAX_CODE_ATTEMPTS).rowsWritten > 0;
    if (!spent) this.sql.exec("UPDATE codes SET attempts = attempts + 1 WHERE expires_at > ?", now);
    return spent;
  }

  /** Revoke a token, known here or not: a revocation can arrive first, or for an older token. */
  revokeToken(hash: string, now: number): void {
    this.sql.exec(
      `INSERT INTO tokens (hash, scopes, created_at, revoked_at) VALUES (?1, '[]', ?2, ?2)
       ON CONFLICT(hash) DO UPDATE SET revoked_at = COALESCE(revoked_at, ?2)`,
      hash,
      now,
    );
  }

  /** A live token's scopes; null when it is unknown or revoked. */
  tokenScopes(hash: string): string[] | null {
    const row = this.sql
      .exec<{ scopes: string; revoked_at: number | null }>("SELECT scopes, revoked_at FROM tokens WHERE hash = ?", hash)
      .toArray()[0];
    if (!row || row.revoked_at !== null) return null;
    return JSON.parse(row.scopes) as string[];
  }

  /**
   * Add every object under `raw/<acct>/` the log lacks: mail from before the log existed, a delivery
   * whose append failed, and what a container stored itself (sent mail, Gmail, imports). It lists
   * everything each time, because a Gmail ULID carries an old date and no watermark would catch it.
   */
  async sweep(acct: string, now: number): Promise<number> {
    const prefix = `raw/${acct}/`;
    let added = 0;
    let cursor: string | undefined;
    do {
      const page = await this.env.RAW.list({ prefix, cursor, limit: LIST_PAGE, include: ["customMetadata"] });
      for (const object of page.objects) {
        const ulid = object.key.slice(prefix.length);
        if (!ULID_RE.test(ulid)) continue;
        const m = object.customMetadata ?? {};
        const at = Number(m.received_at) || ulidTime(ulid);
        const entry: MailEntry = { ulid, at, size: object.size };
        if (m.rcpt) entry.rcpt = m.rcpt;
        if (m.from) entry.from = m.from;
        if (m.source) entry.source = m.source;
        if (m.gm_id) entry.gmail = { id: m.gm_id, thread: m.gm_thread ?? null, labels: (m.gm_labels ?? "").split(",").filter(Boolean) };
        if (m.expires_at) entry.expires_at = m.expires_at;
        if (this.appendMail(entry)) added++;
      }
      cursor = page.truncated ? page.cursor : undefined;
    } while (cursor);
    this.sql.exec("INSERT OR REPLACE INTO meta (key, value) VALUES ('swept_at', ?)", String(now));
    return added;
  }
}

export function logFor(env: { LOG: DurableObjectNamespace<AccountLog> }, acct: string): DurableObjectStub<AccountLog> {
  return env.LOG.get(env.LOG.idFromName(acct));
}
