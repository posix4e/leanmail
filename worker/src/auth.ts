import { OPERATOR } from "./internal";
import type { Env } from "./env";
import { errText, log } from "./log";
import { accountByEmail, normAddress } from "./routes";
import { ACCOUNT_RE, accountFromRequest, b64uDecode, b64uEncode, bytesEqual, deriveSecret, verifyMac } from "./tokens";
import { callContainer, containerFor } from "./wake";
import type { StampOptions } from "./container";
import { HOUR_MS, ipv6Net64, overLimit, sha256Hex } from "./rate";

/**
 * The public sign-in routes the Worker answers before any credential exists, per the contract at
 * the top of lean/Server/Operator.lean and lean/Server/Auth/Magic.lean:
 *
 *   POST /auth/signup        → the operator container, as is (cf-connecting-ip passes through);
 *                              the no-JS form (urlencoded, from /signup) is sent on as JSON
 *                              {email, note} and answered with a 303 to /signup?sent=1
 *                              (?error=email|rate|failed; /signup itself when sign-ups are closed)
 *   POST /auth/passkey/challenge → a stateless login challenge (below), no account involved
 *   POST /auth/passkey/login → the account `userHandle` names, once the challenge verifies
 *   POST /auth/login {email} → D1 lookup; an active account's container gets
 *                              POST /internal/auth/magic {email, purpose:"signin"} in the
 *                              background; the client always gets 202 {ok:true} (JSON) or a 303
 *                              to /signin?sent=1 (the no-JS form, urlencoded)
 *   GET  /auth/magic?t=      → the account the link's MAC names
 *   POST /auth/code          → the account the email belongs to (JSON, native apps)
 *   POST /signin/code        → the same for the web form's 6-digit code
 *   /admin/*                 → MAC-verified lm_session cookie required; a browser navigation goes
 *                              to the cookie's account (the page shell), anything else to the
 *                              operator with x-admin-account: <cookie's account>; anything but
 *                              GET/HEAD also needs Origin = PUBLIC_ORIGIN and
 *                              X-Requested-With: LeanMail (else 403)
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
 * Rate limit: /auth/login and the two code routes, 10 an hour per cf-connecting-ip and, for an
 * IPv6 client, 10 an hour per its /64 too (one host can hold a whole /64), in D1 (`auth_rate`), so
 * every isolate shares one count (passkey logins likewise, in their own bucket); an in-memory map
 * would reset per isolate and not hold across Cloudflare's many. Without cf-connecting-ip
 * (wrangler dev) all clients share one bucket.
 */

export const AUTH_RATE_PER_HOUR = 10;
const MAX_FORM = 16 * 1024;

function json(status: number, body: unknown, headers: Record<string, string> = {}): Response {
  return new Response(JSON.stringify(body), { status, headers: { "content-type": "application/json", ...headers } });
}

const BAD_LINK_HTML =
  '<!doctype html><meta charset=utf-8><title>LeanMail</title><p>This sign-in link has expired or was already used. <a href="/">Ask for a new one.</a></p>';

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
  form: boolean;
  text: string;
  fields: Record<string, string>;
}

/** A JSON or urlencoded body, read once; `text` lets the caller rebuild the request. */
async function readBody(request: Request): Promise<Body | null> {
  const ct = (request.headers.get("content-type") ?? "").toLowerCase();
  // Bytes, decoded as UTF-8: workerd warns when .text() reads a urlencoded body.
  const bytes = await request.arrayBuffer();
  if (bytes.byteLength > MAX_FORM) return null;
  const text = new TextDecoder().decode(bytes);
  if (ct.includes("application/x-www-form-urlencoded")) {
    return { form: true, text, fields: Object.fromEntries(new URLSearchParams(text)) };
  }
  try {
    const v = JSON.parse(text || "{}");
    if (!v || typeof v !== "object" || Array.isArray(v)) return null;
    const fields: Record<string, string> = {};
    for (const [k, x] of Object.entries(v)) if (typeof x === "string") fields[k] = x;
    return { form: false, text, fields };
  } catch {
    return null;
  }
}

/** The same request with another path (and the body already read). */
function rebuild(request: Request, path: string, text: string): Request {
  const url = new URL(request.url);
  url.pathname = path;
  return new Request(url, { method: request.method, headers: request.headers, body: text });
}

const EMAIL_RE = /^[^\s@<>()",;:]+@[^\s@<>()",;:]+\.[^\s@<>()",;:]+$/;

/** An active account's id for this email, else null (unknown, pending and disabled alike). */
async function activeAccount(env: Env, email: string | undefined): Promise<string | null> {
  if (!email) return null;
  const e = normAddress(email);
  if (!EMAIL_RE.test(e)) return null;
  const a = await accountByEmail(env.DB, e);
  return a && a.status === "active" ? a.id : null;
}

async function login(request: Request, env: Env, ctx: ExecutionContext): Promise<Response> {
  if (await rateLimited(env.DB, "login", request)) return tooMany();
  const body = await readBody(request);
  if (!body) return json(400, { error: "bad body" });
  const email = body.fields.email ? normAddress(body.fields.email) : "";
  if (!body.form && !EMAIL_RE.test(email)) return json(400, { error: "bad email" });
  const acct = await activeAccount(env, email);
  if (acct) {
    // In the background: waiting on a (possibly cold) container only for known emails would let
    // response time say which emails have accounts.
    ctx.waitUntil(
      callContainer(env, acct, "/internal/auth/magic", { email, purpose: "signin" })
        .then(async (r) => {
          await r.body?.cancel();
          log("auth.login", { outcome: r.ok ? "sent" : "send_failed", acct, status: r.status });
        })
        .catch((e) => log("auth.login", { outcome: "container_failed", acct, error: errText(e) })),
    );
  } else {
    log("auth.login", { outcome: "no_active_account" });
  }
  if (!body.form) return json(202, { ok: true });
  // The no-JS form: the same answer for every email.
  return new Response(null, { status: 303, headers: { location: "/signin?sent=1", "cache-control": "no-store" } });
}

/** POST /auth/code (JSON) and POST /signin/code (form): to the email's account, else the operator. */
async function code(request: Request, env: Env, path: string): Promise<Response> {
  if (await rateLimited(env.DB, "code", request)) return tooMany();
  const body = await readBody(request);
  if (!body) return json(400, { error: "bad body" });
  const acct = await activeAccount(env, body.fields.email);
  // An unknown email goes to the operator too, which has no such account and refuses the code the
  // way the account would.
  log("auth.code", { routed: acct ? "account" : "operator" });
  return proxyTo(env, acct ?? OPERATOR, rebuild(request, path, body.text));
}

/** POST /auth/signup: the JSON API is proxied as it is; the sign-up page's form is sent on as JSON. */
async function signup(request: Request, env: Env): Promise<Response> {
  const ct = (request.headers.get("content-type") ?? "").toLowerCase();
  if (!ct.includes("application/x-www-form-urlencoded")) return proxyTo(env, OPERATOR, request);
  const back = (q: string) => new Response(null, { status: 303, headers: { location: `/signup${q}`, "cache-control": "no-store" } });
  const body = await readBody(request);
  if (!body) return back("?error=failed");
  const h = new Headers(request.headers);
  h.set("content-type", "application/json");
  const json = JSON.stringify({ email: (body.fields.email ?? "").trim(), note: body.fields.note ?? "" });
  const r = await proxyTo(env, OPERATOR, new Request(request.url, { method: "POST", headers: h, body: json }));
  await r.body?.cancel();
  log("auth.signup_form", { status: r.status });
  if (r.status === 202 || r.status === 200) return back("?sent=1");
  if (r.status === 400) return back("?error=email");
  if (r.status === 429) return back("?error=rate");
  if (r.status === 404) return back("");
  return back("?error=failed");
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

function passkeyChallenge(env: Env): Promise<Response> | Response {
  if (!env.SESSION_SECRET) return json(503, { error: "not configured" });
  return mintPasskeyChallenge(env.SESSION_SECRET).then((challenge) =>
    json(200, { challenge, rpId: new URL(env.PUBLIC_ORIGIN).hostname, userVerification: "preferred", timeout: PASSKEY_CHALLENGE_TTL_MS }, { "cache-control": "no-store" }),
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
  if (!body || body.form) return json(400, { error: "bad body" });
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
 * The public auth routes and `/admin/*`; null for any other request. `/admin/*` needs a
 * MAC-verified `lm_session` cookie; the operator container checks that its account is an admin.
 */
export async function handleAuth(request: Request, env: Env, ctx: ExecutionContext): Promise<Response | null> {
  const url = new URL(request.url);
  const p = url.pathname;
  const m = request.method;
  if (p === "/auth/signup" && m === "POST") return signup(request, env);
  if (p === "/auth/passkey/challenge" && m === "POST") return passkeyChallenge(env);
  if (p === "/auth/passkey/login" && m === "POST") return passkeyLogin(request, env);
  if (p === "/auth/login" && m === "POST") return login(request, env, ctx);
  if (p === "/auth/code" && m === "POST") return code(request, env, "/auth/code");
  if (p === "/signin/code" && m === "POST") return code(request, env, "/signin/code");
  if (p === "/auth/magic" && m === "GET") {
    const v = await verifyMac(url.searchParams.get("t") ?? "", env.SESSION_SECRET, ["v1"]);
    if (!v) return new Response(BAD_LINK_HTML, { status: 400, headers: { "content-type": "text/html; charset=utf-8", "cache-control": "no-store" } });
    return proxyTo(env, v.acct, request);
  }
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
