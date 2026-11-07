import { logFor, type Json } from "./accountlog";
import type { Env } from "./env";
import { handleSyncGmail } from "./gmail";
import { isAddress, localDev } from "./internal";
import { lookupRoute, normAddress } from "./routes";
import { sha256Hex } from "./rate";
import { verifyMac } from "./tokens";
import { ULID_RE } from "./ulid";

/**
 * What the apps and the CLI read without a container, answered here from the account's log
 * (src/accountlog.ts) and R2:
 *
 *   GET   /sync?after=<seq>&limit=<n>  needs `read`  {entries, head, more} oldest first; after=0 (the
 *                                                    default) also carries {settings}
 *   GET   /sync/raw/<ulid>             needs `read`  the message as stored, message/rfc822
 *   GET   /sync/settings               needs `read`  {settings}
 *   PATCH /sync/settings {key: value}  needs `act`   the synced keys; {settings} after the change
 *   GET   /sync/tokens                 needs `read`  {items}, newest first, each with `current`
 *   DELETE /sync/tokens/<id>                         a token may revoke itself; revoking another
 *                                                    needs every scope
 *   /sync/gmail/*                                    src/gmail.ts
 *
 * A client reads pages until `more` is false, keeps the last seq it applied, and starts there next
 * time. A `settings.changed` entry carries {key, value, version}: apply it when its version is newer
 * than the one held. A `mail.received` entry names a ULID; a 404 for one means it has since expired.
 *
 * Only an API token gets in, never the session cookie: nothing here is for a browser, so the CSRF
 * rule has nothing to guard. The Worker verifies a token's MAC and then needs the token to be on the
 * account's list, which holds its scopes and whether it is revoked. Signing in adds a token there
 * (src/auth.ts) and a container pushes its own to /internal/tokens. A token from before that list
 * existed is refused until the client signs in again.
 */

export const ALL_SCOPES = ["read", "draft", "send", "act", "junk"];
const DEFAULT_PAGE = 500;
const MAX_PAGE = 1000;
const MAX_SETTINGS_BODY = 64 * 1024;
const MAX_PREFS = 16 * 1024;

function json(status: number, body: unknown): Response {
  return new Response(JSON.stringify(body), { status, headers: { "content-type": "application/json", "cache-control": "no-store" } });
}

interface Caller {
  acct: string;
  scopes: string[];
  /** The token's id; "" for the local development account. */
  id: string;
}

/** The API token on this request, once its MAC, its account and the account's list all accept it. */
export async function callerOf(request: Request, env: Env): Promise<Caller | null> {
  const m = /^Bearer\s+(\S+)$/i.exec(request.headers.get("authorization") ?? "");
  if (!m) return localDev(env) ? { acct: env.DEV_ACCOUNT!, scopes: ALL_SCOPES, id: "" } : null;
  const token = m[1]!;
  const verified = await verifyMac(token, env.SESSION_SECRET, ["lm1"]);
  if (!verified) return null;
  const row = await env.DB.prepare("SELECT status FROM accounts WHERE id = ?").bind(verified.acct).first<{ status: string }>();
  if (row?.status !== "active") return null;
  const id = await sha256Hex(token);
  const scopes = await logFor(env, verified.acct).tokenScopes(id);
  return scopes && { acct: verified.acct, scopes, id };
}

class BadSetting extends Error {}

/** The keys clients may write, each checked as PATCH /api/account checks it. Anything else is refused. */
const SYNCED: Record<string, (value: unknown, env: Env, acct: string) => Promise<Json> | Json> = {
  display_name(value) {
    if (typeof value !== "string") throw new BadSetting("bad_display_name");
    const name = value.trim();
    if (name.length > 200) throw new BadSetting("display_name_too_long");
    return name;
  },
  async default_from(value, env, acct) {
    if (typeof value !== "string") throw new BadSetting("bad_default_from");
    const from = value.trim();
    if (from === "") return "";
    if (!isAddress(from)) throw new BadSetting("bad_default_from");
    // The From of every send that names none: only an address this account may send as.
    const route = await lookupRoute(env.DB, from);
    if (!route || route.account_id !== acct || !route.can_send) throw new BadSetting("default_from_not_sendable");
    return normAddress(from);
  },
  // Client settings the server never reads. An object, so one client can add a key another ignores.
  prefs(value) {
    if (typeof value !== "object" || value === null || Array.isArray(value)) throw new BadSetting("bad_prefs");
    if (JSON.stringify(value).length > MAX_PREFS) throw new BadSetting("prefs_too_large");
    return value as Json;
  },
};

async function patchSettings(request: Request, env: Env, acct: string, now: number): Promise<Response> {
  const text = await request.text();
  if (text.length > MAX_SETTINGS_BODY) return json(413, { error: "too_large" });
  let body: unknown;
  try {
    body = JSON.parse(text);
  } catch {
    return json(400, { error: "bad_json" });
  }
  if (typeof body !== "object" || body === null || Array.isArray(body)) return json(400, { error: "bad_json" });
  const changes: Record<string, Json> = {};
  try {
    for (const [key, value] of Object.entries(body)) {
      const check = Object.hasOwn(SYNCED, key) ? SYNCED[key] : undefined;
      if (!check) throw new BadSetting("unknown_setting");
      changes[key] = await check(value, env, acct);
    }
  } catch (e) {
    if (e instanceof BadSetting) return json(400, { error: e.message });
    throw e;
  }
  return json(200, { settings: await logFor(env, acct).putSettings(changes, now) });
}

async function tokens(request: Request, env: Env, who: Caller, id: string | undefined, now: number): Promise<Response> {
  const account = logFor(env, who.acct);
  const m = request.method;
  if (id === undefined) {
    if (m !== "GET" && m !== "HEAD") return json(405, { error: "method_not_allowed" });
    if (!who.scopes.includes("read")) return json(403, { error: "insufficient_scope" });
    return json(200, { items: (await account.listTokens()).map((t) => ({ ...t, current: t.id === who.id })) });
  }
  if (m !== "DELETE") return json(405, { error: "method_not_allowed" });
  if (id !== who.id && !ALL_SCOPES.every((s) => who.scopes.includes(s))) return json(403, { error: "insufficient_scope" });
  if (!(await account.listTokens()).some((t) => t.id === id)) return json(404, { error: "not_found" });
  await account.revokeToken(id, now);
  return json(200, { ok: true });
}

function count(raw: string | null, fallback: number): number | null {
  if (raw === null) return fallback;
  return /^\d{1,15}$/.test(raw) ? Number(raw) : null;
}

/** Everything under /sync. `now` is here for the tests. */
export async function handleSync(request: Request, env: Env, ctx: ExecutionContext, now: number = Date.now()): Promise<Response> {
  const url = new URL(request.url);
  const path = url.pathname;
  const who = await callerOf(request, env);
  if (!who) return json(401, { error: "unauthorized" });
  const { acct, scopes } = who;
  const m = request.method;
  const reading = m === "GET" || m === "HEAD";

  if (path === "/sync/gmail" || path.startsWith("/sync/gmail/")) return handleSyncGmail(request, env, acct, scopes, now);

  const token = /^\/sync\/tokens(?:\/([^/]+))?$/.exec(path);
  if (token) return tokens(request, env, who, token[1], now);

  if (path === "/sync/settings") {
    if (!reading && m !== "PATCH") return json(405, { error: "method_not_allowed" });
    if (!scopes.includes(reading ? "read" : "act")) return json(403, { error: "insufficient_scope" });
    if (m === "PATCH") return patchSettings(request, env, acct, now);
    return json(200, { settings: await logFor(env, acct).settings() });
  }

  const raw = /^\/sync\/raw\/([^/]+)$/.exec(path);
  if (path !== "/sync" && !raw) return json(404, { error: "not_found" });
  if (!reading) return json(405, { error: "method_not_allowed" });
  if (!scopes.includes("read")) return json(403, { error: "insufficient_scope" });

  if (raw) {
    if (!ULID_RE.test(raw[1]!)) return json(404, { error: "not_found" });
    const object = await env.RAW.get(`raw/${acct}/${raw[1]!}`);
    if (!object) return json(404, { error: "not_found" });
    return new Response(object.body, {
      headers: { "content-type": "message/rfc822", "content-length": String(object.size), "cache-control": "private, no-store" },
    });
  }

  const after = count(url.searchParams.get("after"), 0);
  const limit = count(url.searchParams.get("limit"), DEFAULT_PAGE);
  if (after === null || limit === null || limit < 1) return json(400, { error: "bad_cursor" });
  const account = logFor(env, acct);
  const page = await account.since(after, Math.min(limit, MAX_PAGE));
  // Mail stored before this account's first sweep shows up on a later poll, once this one finishes.
  if (!page.swept) ctx.waitUntil(account.sweep(acct, now));
  const last = page.entries.at(-1)?.seq ?? after;
  const body: Record<string, unknown> = { entries: page.entries, head: page.head, more: last < page.head };
  if (after === 0) body.settings = await account.settings();
  return json(200, body);
}
