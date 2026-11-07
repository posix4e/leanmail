import { logFor } from "./accountlog";
import { OPERATOR, sendSigninMail, type SigninKind } from "./internal";
import type { Env } from "./env";
import { errText, log } from "./log";
import { accountByEmail, normAddress } from "./routes";
import { ALL_SCOPES, callerOf } from "./sync";
import { ACCOUNT_RE, accountFromRequest, b64uDecode, b64uEncode, bytesEqual, deriveSecret, signMac } from "./tokens";
import { ulid } from "./ulid";
import { containerFor } from "./wake";
import type { StampOptions } from "./container";
import { HOUR_MS, ipv6Net64, overLimit, sha256Hex } from "./rate";

/**
 * Sign-in, answered here with no container (apps and the CLI; there is no browser sign-in any more):
 *
 *   POST /auth/signup {email, note?}  202 for every address; 404 when SIGNUP_MODE=closed. A new
 *                                     address gets a `pending` account and a sign-up an admin
 *                                     decides, or an `active` account at once with SIGNUP_MODE=open
 *                                     or an ADMIN_EMAILS address, and then a welcome code. A sign-up
 *                                     never changes an account that already exists.
 *   POST /auth/login {email}          202 for every address; an active account gets a code by mail,
 *                                     in the background, so timing says nothing about who has one
 *   POST /auth/code {email, code, name}  an API token with every scope, shown once; else 400
 *   GET  /admin/signups               undecided sign-ups, for an ADMIN_EMAILS account's API token (`read`)
 *   POST /admin/signups/<id>/approve  needs `act`; the account goes active and gets a welcome code
 *   POST /admin/signups/<id>/reject   needs `act`; a pending account goes
 *   /admin/*                          everything else: the lm_session cookie, to the operator, as before
 *
 * An account's id lasts: signing in with the email an older account has reaches that same account,
 * and its mail in R2. There is nothing to migrate and no password to reset.
 *
 * A code is six digits, good for 15 minutes (a welcome code, 24 hours) and one use. The account's
 * log object (src/accountlog.ts) keeps HMACs of its live codes, under a key derived from the
 * master, so a copy of that storage hands out nothing. At most three live at once, each with five
 * guesses; a wrong guess costs every live code one. A guesser can spend someone's codes that way,
 * which the 10-an-hour limit below keeps to two rounds an hour, and a new code is one request away.
 * An unknown email goes through the same steps against `_operator`, which holds no codes.
 *
 * Passkey login (lean/Server/Auth/Passkey.lean). The challenge is `v1.<rand>.<exp>.<mac>` with
 * mac = base64url(HMAC-SHA256(deriveSecret(master SESSION_SECRET, "passkey:challenge"),
 * "v1.<rand>.<exp>")): a key no container holds (the label fails ACCOUNT_RE, so no account is
 * named that), and unlike magic links and cookies it names no account, since none is known before
 * the browser picks a discoverable credential. The browser signs over the token's UTF-8 bytes, so
 * `clientDataJSON.challenge` is base64url of the token. On login the Worker takes the account from
 * `response.userHandle` (base64url of the account id, which registration set as `user.id`; only an
 * active, non-`_` account in D1), checks the token's MAC and expiry, and proxies to that container
 * with `x-passkey-challenge-ok: base64url(HMAC-SHA256(K_acct, "passkey:" + token))`, K_acct being
 * the container's own derived SESSION_SECRET: the container cannot check the master's MAC, so the
 * Worker vouches for the token under the key the container has (any client value is replaced).
 * The container then checks the voucher, the origin, the signature and the sign count, spends the
 * challenge once, and starts the session.
 *
 * Rate limit: /auth/login, /auth/code and /auth/signup each, 10 an hour per cf-connecting-ip and, for an
 * IPv6 client, 10 an hour per its /64 too (one host can hold a whole /64), in D1 (`auth_rate`), so
 * every isolate shares one count (passkey logins likewise, in their own bucket); an in-memory map
 * would reset per isolate and not hold across Cloudflare's many. Without cf-connecting-ip
 * (wrangler dev) all clients share one bucket.
 */

export const AUTH_RATE_PER_HOUR = 10;
const MAX_BODY = 16 * 1024;
const SIGNIN_CODE_TTL_MS = 15 * 60_000;
const WELCOME_CODE_TTL_MS = 24 * 3_600_000;
const MAX_NOTE = 1000;
const MAX_TOKEN_NAME = 100;

function json(status: number, body: unknown, headers: Record<string, string> = {}): Response {
  return new Response(JSON.stringify(body), { status, headers: { "content-type": "application/json", ...headers } });
}

/**
 * A browser navigation, by the rule the container's pages use (lean/Server/Pages/Common.lean):
 * `Accept` has text/html, and it is neither our script's fetch (`X-Requested-With: LeanMail`) nor
 * an explicit JSON `Accept` without HTML.
 */
export function isNavigation(request: Request): boolean {
  const accept = request.headers.get("accept") ?? "";
  if (!accept.includes("text/html")) return false;
  if (request.headers.get("x-requested-with") === "LeanMail") return false;
  return true;
}

export function toSignin(): Response {
  return new Response(null, { status: 302, headers: { location: "/signin", "cache-control": "no-store" } });
}

/** Proxy to an account's container; a container that cannot be reached is a 503. */
export async function proxyTo(env: Env, acct: string, request: Request, opts: StampOptions = {}): Promise<Response> {
  try {
    return await containerFor(env, acct).proxy(request, acct, opts);
  } catch (e) {
    log("proxy.failed", { acct, error: errText(e) });
    return json(503, { error: "container unavailable" }, { "retry-after": "5" });
  }
}

/**
 * Count one attempt for this client in `bucket` (keys `<bucket>:<sha256 of the IP>` and, for
 * IPv6, `<bucket>:net:<sha256 of its /64>`); true once either window holds more than the limit.
 */
export async function rateLimited(db: D1Database, bucket: string, request: Request, now: number = Date.now()): Promise<boolean> {
  const ip = request.headers.get("cf-connecting-ip") ?? "";
  const keys = [`${bucket}:${await sha256Hex(ip)}`];
  const net = ipv6Net64(ip);
  if (net) keys.push(`${bucket}:net:${await sha256Hex(net)}`);
  return overLimit(db, keys, AUTH_RATE_PER_HOUR, now);
}

/** Forget rate windows that ended over an hour ago (daily cron). */
export async function pruneAuthRate(db: D1Database, now: number = Date.now()): Promise<void> {
  await db.prepare("DELETE FROM auth_rate WHERE window_start < ?1").bind(now - HOUR_MS).run();
}

/** A state-changing /admin/* request from our own page script: same Origin, and our header. */
export function adminWriteAllowed(request: Request, env: Env): boolean {
  if (request.method === "GET" || request.method === "HEAD") return true;
  let origin: string;
  try {
    origin = new URL(env.PUBLIC_ORIGIN).origin;
  } catch {
    return false;
  }
  return request.headers.get("origin") === origin && request.headers.get("x-requested-with") === "LeanMail";
}

function tooMany(): Response {
  return json(429, { error: "too many attempts" }, { "retry-after": "3600" });
}

interface Body {
  text: string;
  fields: Record<string, string>;
}

/** A JSON object, read once; `text` lets the caller rebuild the request. */
async function readBody(request: Request): Promise<Body | null> {
  // Bytes, decoded as UTF-8: workerd warns when .text() reads a urlencoded body.
  const bytes = await request.arrayBuffer();
  if (bytes.byteLength > MAX_BODY) return null;
  const text = new TextDecoder().decode(bytes);
  try {
    const v = JSON.parse(text || "{}");
    if (!v || typeof v !== "object" || Array.isArray(v)) return null;
    const fields: Record<string, string> = {};
    for (const [k, x] of Object.entries(v)) if (typeof x === "string") fields[k] = x;
    return { text, fields };
  } catch {
    return null;
  }
}

const EMAIL_RE = /^[^\s@<>()",;:]+@[^\s@<>()",;:]+\.[^\s@<>()",;:]+$/;

/** An address fit to be an account's, normalised; else null. */
function emailOf(raw: string | undefined): string | null {
  const e = normAddress(raw ?? "");
  return e.length <= 254 && EMAIL_RE.test(e) ? e : null;
}

/** An active account's id for this email, else null (unknown, pending and disabled alike). */
async function activeAccount(env: Env, email: string | null): Promise<string | null> {
  if (!email) return null;
  const a = await accountByEmail(env.DB, email);
  return a && a.status === "active" ? a.id : null;
}

/** What the account's log keeps of a code. `:` is not in an account id, so this is no account's key. */
function codeHash(env: Env, acct: string, code: string): Promise<string> {
  return deriveSecret(env.SESSION_SECRET!, `signin-code:${acct}:${code}`);
}

/** Six random digits. */
function newCode(): string {
  return String(crypto.getRandomValues(new Uint32Array(1))[0]! % 1_000_000).padStart(6, "0");
}

/** Mint a code and mail it. Never throws: the caller has already answered, or must not differ. */
async function mailCode(env: Env, acct: string, email: string, kind: SigninKind, now: number): Promise<void> {
  try {
    const code = newCode();
    const ttl = kind === "welcome" ? WELCOME_CODE_TTL_MS : SIGNIN_CODE_TTL_MS;
    if (await logFor(env, acct).addCode(await codeHash(env, acct, code), now, now + ttl)) {
      await sendSigninMail(env, acct, email, kind, code, now);
    } else {
      log("auth.code_capped", { acct, kind });
    }
  } catch (e) {
    log("auth.code_failed", { acct, kind, error: errText(e) });
  }
}

const accepted = () => json(202, { ok: true });

async function login(request: Request, env: Env, ctx: ExecutionContext, now: number): Promise<Response> {
  if (await rateLimited(env.DB, "login", request, now)) return tooMany();
  const body = await readBody(request);
  if (!body) return json(400, { error: "bad body" });
  const email = emailOf(body.fields.email);
  if (!email) return json(400, { error: "bad email" });
  const acct = await activeAccount(env, email);
  if (acct) ctx.waitUntil(mailCode(env, acct, email, "signin", now));
  log("auth.login", { active: acct !== null });
  return accepted();
}

/** POST /auth/code: spend the code, and the account gets a new token on its list. */
async function redeem(request: Request, env: Env, now: number): Promise<Response> {
  if (!env.SESSION_SECRET) return json(503, { error: "not configured" });
  if (await rateLimited(env.DB, "code", request, now)) return tooMany();
  const body = await readBody(request);
  if (!body) return json(400, { error: "bad body" });
  const acct = await activeAccount(env, emailOf(body.fields.email));
  const code = (body.fields.code ?? "").trim();
  // An unknown email costs what a known one does, so the answer's timing does not say which it was.
  const target = acct ?? OPERATOR;
  const spent = /^\d{6}$/.test(code) && (await logFor(env, target).spendCode(await codeHash(env, target, code), now));
  log("auth.code", { spent });
  if (!spent || !acct) return json(400, { error: "invalid code" });
  const name = (body.fields.name ?? "").trim().slice(0, MAX_TOKEN_NAME) || "app";
  const token = await signMac("lm1", acct, b64uEncode(crypto.getRandomValues(new Uint8Array(32))), env.SESSION_SECRET);
  await logFor(env, acct).putToken(await sha256Hex(token), ALL_SCOPES, now, name);
  return json(200, { token }, { "cache-control": "no-store" });
}

function isAdminEmail(env: Env, email: string): boolean {
  return (env.ADMIN_EMAILS ?? "").split(",").some((a) => a.trim() !== "" && normAddress(a) === email);
}

async function signup(request: Request, env: Env, ctx: ExecutionContext, now: number): Promise<Response> {
  const mode = env.SIGNUP_MODE;
  if (mode === "closed") return json(404, { error: "not_found" });
  if (await rateLimited(env.DB, "signup", request, now)) return json(429, { error: "too many sign-ups" }, { "retry-after": "3600" });
  const body = await readBody(request);
  if (!body) return json(400, { error: "bad body" });
  const email = emailOf(body.fields.email);
  if (!email) return json(400, { error: "bad email" });
  if (await accountByEmail(env.DB, email)) {
    log("auth.signup", { outcome: "exists" });
    return accepted();
  }
  const auto = mode === "open" || isAdminEmail(env, email);
  const id = ulid(now);
  // One transaction: the sign-up exists only if this request's account does, whoever won a race.
  const [made] = await env.DB.batch([
    env.DB.prepare("INSERT INTO accounts (id, email, status, updated_at) VALUES (?1, ?2, ?3, ?4) ON CONFLICT(email) DO NOTHING").bind(id, email, auto ? "active" : "pending", now),
    env.DB.prepare(
      `INSERT INTO signups (id, account_id, email, note, created_at, decided_at, decision)
       SELECT ?1, ?1, ?2, ?3, ?4, ?5, ?6 WHERE EXISTS (SELECT 1 FROM accounts WHERE id = ?1)`,
    ).bind(id, email, (body.fields.note ?? "").slice(0, MAX_NOTE), now, auto ? now : null, auto ? "approved" : null),
  ]);
  const created = (made?.meta.changes ?? 0) > 0;
  log("auth.signup", { outcome: created ? (auto ? "approved" : "pending") : "exists" });
  if (created && auto) ctx.waitUntil(mailCode(env, id, email, "welcome", now));
  return accepted();
}

/** The account's email when it is an admin's, else null. */
async function adminEmail(env: Env, acct: string): Promise<string | null> {
  const row = await env.DB.prepare("SELECT email FROM accounts WHERE id = ?1").bind(acct).first<{ email: string }>();
  return row && isAdminEmail(env, normAddress(row.email)) ? row.email : null;
}

/** /admin/signups: an admin's API token decides them, and only undecided ones. */
async function signups(request: Request, env: Env, ctx: ExecutionContext, path: string, now: number): Promise<Response> {
  const who = await callerOf(request, env);
  if (!who) return json(401, { error: "unauthorized" });
  const reading = request.method === "GET";
  if (!who.scopes.includes(reading ? "read" : "act")) return json(403, { error: "insufficient_scope" });
  if (!(await adminEmail(env, who.acct))) return json(403, { error: "forbidden" });
  if (path === "/admin/signups") {
    if (!reading) return json(405, { error: "method_not_allowed" });
    const { results } = await env.DB.prepare("SELECT id, email, note, created_at FROM signups WHERE decision IS NULL ORDER BY created_at DESC LIMIT 500").all();
    return json(200, { items: results });
  }
  const m = /^\/admin\/signups\/([^/]+)\/(approve|reject)$/.exec(path);
  if (!m) return json(404, { error: "not_found" });
  if (request.method !== "POST") return json(405, { error: "method_not_allowed" });
  const approve = m[2] === "approve";
  const row = await env.DB.prepare("SELECT account_id, email FROM signups WHERE id = ?1").bind(m[1]!).first<{ account_id: string; email: string }>();
  if (!row) return json(404, { error: "not_found" });
  const [decided, account] = await env.DB.batch([
    env.DB.prepare("UPDATE signups SET decision = ?2, decided_at = ?3 WHERE id = ?1 AND decision IS NULL").bind(m[1]!, approve ? "approved" : "rejected", now),
    approve
      ? env.DB.prepare(
          `UPDATE accounts SET status = 'active', updated_at = ?2 WHERE id = ?1 AND status = 'pending'
           AND EXISTS (SELECT 1 FROM signups WHERE account_id = ?1 AND decided_at = ?2 AND decision = 'approved')`,
        ).bind(row.account_id, now)
      : env.DB.prepare(
          `DELETE FROM accounts WHERE id = ?1 AND status = 'pending'
           AND EXISTS (SELECT 1 FROM signups WHERE account_id = ?1 AND decided_at = ?2 AND decision = 'rejected')`,
        ).bind(row.account_id, now),
  ]);
  if (!decided?.meta.changes) return json(409, { error: "already decided" });
  log("auth.signup_decided", { decision: m[2], by: who.acct });
  if (approve && account?.meta.changes) ctx.waitUntil(mailCode(env, row.account_id, row.email, "welcome", now));
  return json(200, { ok: true, account_id: row.account_id });
}

export const PASSKEY_CHALLENGE_TTL_MS = 5 * 60_000;
const PASSKEY_LABEL = "passkey:challenge";
const enc = new TextEncoder();

async function hmacRaw(key: string, data: string): Promise<Uint8Array> {
  const k = await crypto.subtle.importKey("raw", enc.encode(key), { name: "HMAC", hash: "SHA-256" }, false, ["sign"]);
  return new Uint8Array(await crypto.subtle.sign("HMAC", k, enc.encode(data)));
}

/** A stateless passkey login challenge `v1.<rand>.<exp>.<mac>` (see the header). */
export async function mintPasskeyChallenge(master: string, now: number = Date.now()): Promise<string> {
  const signed = `v1.${b64uEncode(crypto.getRandomValues(new Uint8Array(18)))}.${now + PASSKEY_CHALLENGE_TTL_MS}`;
  return `${signed}.${b64uEncode(await hmacRaw(await deriveSecret(master, PASSKEY_LABEL), signed))}`;
}

/** The challenge's MAC and expiry; false on any malformation, never throws. */
export async function verifyPasskeyChallenge(token: string, master: string | undefined, now: number = Date.now()): Promise<boolean> {
  if (!master || typeof token !== "string" || token.length > 512) return false;
  const parts = token.split(".");
  if (parts.length !== 4 || parts[0] !== "v1" || !parts[1] || !/^\d{1,16}$/.test(parts[2]!)) return false;
  if (Number(parts[2]) <= now) return false;
  let mac: Uint8Array;
  try {
    mac = b64uDecode(parts[3]!);
  } catch {
    return false;
  }
  const expected = await hmacRaw(await deriveSecret(master, PASSKEY_LABEL), parts.slice(0, 3).join("."));
  return bytesEqual(mac, expected);
}

/** What `acct`'s container expects in `x-passkey-challenge-ok` for this token. */
export async function passkeyVoucher(master: string, acct: string, token: string): Promise<string> {
  return b64uEncode(await hmacRaw(await deriveSecret(master, acct), `passkey:${token}`));
}

/**
 * The passkey relying party, as the container's `Server.Auth.Passkey.rpId` has it: RP_ID when it
 * is PUBLIC_ORIGIN's host or a parent domain of it, else that host.
 */
export function rpIdFor(env: { PUBLIC_ORIGIN: string; RP_ID?: string }): string {
  const host = new URL(env.PUBLIC_ORIGIN).hostname;
  const r = (env.RP_ID ?? "").trim().toLowerCase();
  return r.includes(".") && (host === r || host.endsWith(`.${r}`)) ? r : host;
}

function passkeyChallenge(env: Env): Promise<Response> | Response {
  if (!env.SESSION_SECRET) return json(503, { error: "not configured" });
  return mintPasskeyChallenge(env.SESSION_SECRET).then((challenge) =>
    json(200, { challenge, rpId: rpIdFor(env), userVerification: "preferred", timeout: PASSKEY_CHALLENGE_TTL_MS }, { "cache-control": "no-store" }),
  );
}

function b64uText(s: unknown): string | null {
  if (typeof s !== "string" || !s) return null;
  try {
    return new TextDecoder("utf-8", { fatal: true, ignoreBOM: false }).decode(b64uDecode(s));
  } catch {
    return null;
  }
}

async function passkeyLogin(request: Request, env: Env): Promise<Response> {
  if (await rateLimited(env.DB, "passkey", request)) return tooMany();
  const refuse = (reason: string) => {
    log("auth.passkey_login", { outcome: "refused", reason });
    return json(401, { error: "passkey not accepted" });
  };
  const body = await readBody(request);
  if (!body) return json(400, { error: "bad body" });
  let resp: Record<string, unknown>;
  try {
    const v = JSON.parse(body.text) as { response?: unknown };
    if (!v.response || typeof v.response !== "object") return refuse("no response");
    resp = v.response as Record<string, unknown>;
  } catch {
    return json(400, { error: "bad body" });
  }
  const acct = b64uText(resp.userHandle);
  if (!acct || !ACCOUNT_RE.test(acct) || acct.startsWith("_")) return refuse("bad user handle");
  const row = await env.DB.prepare("SELECT status FROM accounts WHERE id = ?1").bind(acct).first<{ status: string }>();
  if (row?.status !== "active") return refuse("no active account");
  const cdj = b64uText(resp.clientDataJSON);
  let token: string | null = null;
  try {
    const cd = cdj ? (JSON.parse(cdj) as { challenge?: unknown }) : null;
    token = cd ? b64uText(cd.challenge) : null;
  } catch {
    token = null;
  }
  if (!token || !(await verifyPasskeyChallenge(token, env.SESSION_SECRET))) return refuse("bad challenge");
  const h = new Headers(request.headers);
  h.set("x-passkey-challenge-ok", await passkeyVoucher(env.SESSION_SECRET!, acct, token));
  log("auth.passkey_login", { outcome: "routed", acct });
  return proxyTo(env, acct, new Request(request.url, { method: "POST", headers: h, body: body.text }));
}

/**
 * The public auth routes and `/admin/*`; null for any other request. `/admin/*` other than
 * sign-ups needs a MAC-verified `lm_session` cookie; the operator container checks that its account
 * is an admin.
 */
export async function handleAuth(request: Request, env: Env, ctx: ExecutionContext, now: number = Date.now()): Promise<Response | null> {
  const url = new URL(request.url);
  const p = url.pathname;
  const m = request.method;
  if (p === "/auth/signup" && m === "POST") return signup(request, env, ctx, now);
  if (p === "/auth/passkey/challenge" && m === "POST") return passkeyChallenge(env);
  if (p === "/auth/passkey/login" && m === "POST") return passkeyLogin(request, env);
  if (p === "/auth/login" && m === "POST") return login(request, env, ctx, now);
  if (p === "/auth/code" && m === "POST") return redeem(request, env, now);
  if (p === "/admin/signups" || p.startsWith("/admin/signups/")) return signups(request, env, ctx, p, now);
  if (p === "/admin" || p.startsWith("/admin/")) {
    const acct = await accountFromRequest(request, env.SESSION_SECRET, { bearer: false });
    const nav = isNavigation(request);
    if (!acct) return nav ? toSignin() : json(401, { error: "unauthorized" });
    if (!adminWriteAllowed(request, env)) {
      log("auth.admin_refused", { method: m });
      return json(403, { error: "forbidden" });
    }
    // A browser navigation gets the page shell from the admin's own container; the shell's script
    // (and any API client) reaches the operator, which checks that `acct` is an admin.
    if (nav) return proxyTo(env, acct, request);
    return proxyTo(env, OPERATOR, request, { adminAccount: acct });
  }
  if (p.startsWith("/auth/")) return json(404, { error: "not_found" });
  return null;
}
