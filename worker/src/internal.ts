import { EmailMessage } from "cloudflare:email";
import type { Env } from "./env";
import { errText, log } from "./log";
import {
  MAILS,
  domainOf,
  domainsNotOwnedBy,
  domainsOf,
  lookupRoute,
  normAddress,
  upsertAccounts,
  upsertRoutes,
  type AccountInput,
  type DomainInput,
  type Mail,
  type RouteInput,
} from "./routes";
import { pushPayload, sendApns, type ApnsConfig, type Device } from "./apns";
import { safeDecode } from "./paths";
import { overLimit } from "./rate";
import { ACCOUNT_RE, deriveInternalSecret, secretEqual, verifyMac } from "./tokens";
import { ulid as mintUlid } from "./ulid";
import { callContainer, containerFor, wake as defaultWake, type WakeFn } from "./wake";

/**
 * `/internal/*`: what containers ask of the Worker-only bindings. Containers call the public
 * origin, so these arrive from the internet. Each container holds only its own derived secret
 * (HMAC of its account id under INTERNAL_SECRET), so `x-account-id` is proven by
 * `X-Internal-Secret`: a container can speak only as itself, and `_operator` claims need the
 * operator's derived secret.
 */

export const OPERATOR = "_operator";
export const LOCAL_HOSTS = new Set(["localhost", "127.0.0.1", "[::1]"]);

/**
 * Local development: DEV_ACCOUNT is set (worker/.dev.vars) AND PUBLIC_ORIGIN is a localhost origin.
 * Keyed on the configured origin, not the request's host: under `wrangler dev` a custom-domain
 * route rewrites every request's host to the production domain, while production never has a
 * localhost PUBLIC_ORIGIN (deploy.yml refuses one). The container applies the same double guard.
 */
export function localDev(env: { DEV_ACCOUNT?: string; PUBLIC_ORIGIN?: string }): boolean {
  if (!env.DEV_ACCOUNT) return false;
  try {
    return LOCAL_HOSTS.has(new URL(env.PUBLIC_ORIGIN ?? "").hostname);
  } catch {
    return false;
  }
}
const MAX_SEND_BYTES = 25 * 1024 * 1024;
const MAX_SEND_RCPTS = 50;
const MAX_ROUTES = 1000;
const ADDRESS_RE = /^[^\s@<>()",;:]+@[a-z0-9](?:[a-z0-9-]*[a-z0-9])?(?:\.[a-z0-9](?:[a-z0-9-]*[a-z0-9])?)+$/;
const DOMAIN_RE = /^[a-z0-9](?:[a-z0-9-]*[a-z0-9])?(?:\.[a-z0-9](?:[a-z0-9-]*[a-z0-9])?)+$/;
const STATUSES = ["pending", "active", "disabled"];

export interface InternalDeps {
  wake: WakeFn;
  /** POST JSON to an account's container through its DO (callContainer); injectable for tests. */
  call: typeof callContainer;
  /** The APNs transport (apns.ts `sendApns`); injectable for tests. */
  sendApns: typeof sendApns;
  now: () => number;
}

/** The address the install's own mail comes from: MAIL_FROM, else `no-reply@<host of PUBLIC_ORIGIN>`. */
export function mailFrom(env: Env): string {
  if (env.MAIL_FROM) return normAddress(env.MAIL_FROM);
  let host = "localhost";
  try {
    host = new URL(env.PUBLIC_ORIGIN).hostname;
  } catch {}
  return `no-reply@${host.toLowerCase()}`;
}

class HttpError extends Error {
  constructor(
    readonly status: number,
    readonly code: string,
  ) {
    super(code);
  }
}

function json(status: number, body: unknown): Response {
  return new Response(JSON.stringify(body), { status, headers: { "content-type": "application/json" } });
}

async function readJson(request: Request, max = 36 * 1024 * 1024): Promise<Record<string, unknown>> {
  const len = Number(request.headers.get("content-length") ?? "0");
  if (len > max) throw new HttpError(413, "too_large");
  let v: unknown;
  try {
    v = await request.json();
  } catch {
    throw new HttpError(400, "bad_json");
  }
  if (!v || typeof v !== "object" || Array.isArray(v)) throw new HttpError(400, "bad_json");
  return v as Record<string, unknown>;
}

function isAddress(s: unknown): s is string {
  return typeof s === "string" && s.length <= 320 && ADDRESS_RE.test(normAddress(s));
}

function decodeB64(s: string): Uint8Array {
  try {
    const bin = atob(s.replace(/\s+/g, ""));
    return Uint8Array.from(bin, (c) => c.charCodeAt(0));
  } catch {
    throw new HttpError(400, "bad_raw_b64");
  }
}

/**
 * The account a call is from: `x-account-id`, accepted only when `X-Internal-Secret` equals that
 * account's derived secret (constant-time). Null when INTERNAL_SECRET is unset (fail closed), the
 * header is malformed, or the secret is another account's.
 */
export async function authenticatedCaller(request: Request, env: Env, acct: string | null | undefined): Promise<string | null> {
  const presented = request.headers.get("x-internal-secret");
  if (!env.INTERNAL_SECRET || !presented || !acct || !ACCOUNT_RE.test(acct)) return null;
  const expected = await deriveInternalSecret(env.INTERNAL_SECRET, acct);
  return (await secretEqual(presented, expected)) ? acct : null;
}

function requireOperator(caller: string): void {
  if (caller !== OPERATOR) throw new HttpError(403, "operator_only");
}

// ---- POST /internal/email/send ----

/**
 * The address of the raw message's one `From:` header (the addr-spec in its last `<...>`, else the
 * whole value), normalised; null when there is none, more than one, or it names several mailboxes.
 */
export function headerFrom(raw: Uint8Array): string | null {
  const text = new TextDecoder().decode(raw.subarray(0, 256 * 1024));
  const end = text.search(/\r?\n\r?\n/);
  const head = (end < 0 ? text : text.slice(0, end)).replace(/\r?\n[ \t]+/g, " ");
  const values: string[] = [];
  for (const line of head.split(/\r?\n/)) {
    const m = /^([!-9;-~]+):(.*)$/.exec(line);
    if (m && m[1]!.toLowerCase() === "from") values.push(m[2]!);
  }
  if (values.length !== 1) return null;
  const v = values[0]!.trim();
  const angle = /<([^<>]*)>\s*$/.exec(v);
  const addr = normAddress(angle ? angle[1]! : v);
  return isAddress(addr) ? addr : null;
}

async function emailSend(request: Request, env: Env, caller: string): Promise<Response> {
  const body = await readJson(request);
  const { from, to, raw_b64 } = body;
  if (!isAddress(from)) throw new HttpError(400, "bad_from");
  if (!Array.isArray(to) || to.length === 0 || to.length > MAX_SEND_RCPTS || !to.every(isAddress)) throw new HttpError(400, "bad_to");
  if (typeof raw_b64 !== "string" || raw_b64.length === 0) throw new HttpError(400, "bad_raw_b64");
  const raw = decodeB64(raw_b64);
  if (raw.byteLength > MAX_SEND_BYTES) throw new HttpError(413, "too_large");
  // The message says it is from the envelope's sender, for every caller.
  const shown = headerFrom(raw);
  if (shown === null) throw new HttpError(400, "bad_header_from");
  if (shown !== normAddress(from)) throw new HttpError(403, "header_from_mismatch");
  // An account sends only as an address it owns and may send from; the install's MAIL_FROM is not
  // one (its sign-in and welcome mail go through /internal/email/signin, which the Worker writes).
  // The operator may send as anything.
  if (caller !== OPERATOR) {
    const route = await lookupRoute(env.DB, from);
    if (!route || route.account_id !== caller || !route.can_send) throw new HttpError(403, "from_not_allowed");
  }
  let sent = 0;
  const failed: number[] = [];
  for (const [i, rcpt] of to.entries()) {
    try {
      // A stream reads once: each recipient gets its own.
      await env.EMAIL.send(new EmailMessage(normAddress(from), normAddress(rcpt), new Blob([raw]).stream()));
      sent++;
    } catch (e) {
      failed.push(i);
      log("internal.email_send_failed", { acct: caller, error: errText(e) });
    }
  }
  log("internal.email_send", { acct: caller, sent, failed: failed.length, size: raw.byteLength });
  if (failed.length) return json(502, { ok: false, sent, failed });
  return json(200, { ok: true, sent });
}

// ---- POST /internal/email/signin ----

export const SIGNIN_MAIL_PER_HOUR = 20;
const CODE_RE = /^\d{6}$/;
const LINK_MAX = 900;
const LINK_TOKEN_RE = /^[A-Za-z0-9._~%-]+$/;

export type SigninKind = "signin" | "welcome";

/** The sign-in (or welcome) message the Worker writes itself: plain text, 7-bit, CRLF. */
export function signinMessage(m: { from: string; to: string; kind: SigninKind; link: string; code: string; now: number; id: string }): string {
  const subject = m.kind === "welcome" ? "Welcome to LeanMail" : "Sign in to LeanMail";
  const date = new Date(m.now).toUTCString().replace(/GMT$/, "+0000");
  const intro = m.kind === "welcome" ? ["Welcome to LeanMail: your account is ready.", "", "Open this link to sign in:"] : ["Open this link to sign in to LeanMail:"];
  const outro = m.kind === "welcome" ? [] : ["", "If you did not ask to sign in, ignore this message."];
  const head = [
    `From: LeanMail <${m.from}>`,
    `To: ${m.to}`,
    `Subject: ${subject}`,
    `Date: ${date}`,
    `Message-ID: <${m.id}@${domainOf(m.from) || "localhost"}>`,
    "MIME-Version: 1.0",
    "Content-Type: text/plain; charset=utf-8",
    "Content-Transfer-Encoding: 7bit",
    "Auto-Submitted: auto-generated",
  ];
  const text = [...intro, m.link, "", `Or enter this code: ${m.code}`, "", "The link and the code work once, and expire soon.", ...outro];
  return `${head.join("\r\n")}\r\n\r\n${text.join("\r\n")}\r\n`;
}

/** `<PUBLIC_ORIGIN>/auth/magic?t=<token>` with a token the caller's own key MACs (as GET /auth/magic checks). */
async function validSigninLink(env: Env, link: unknown, caller: string): Promise<boolean> {
  if (typeof link !== "string" || link.length > LINK_MAX) return false;
  const prefix = `${env.PUBLIC_ORIGIN.replace(/\/+$/, "")}/auth/magic?t=`;
  if (!link.startsWith(prefix)) return false;
  const rest = link.slice(prefix.length);
  if (!LINK_TOKEN_RE.test(rest)) return false;
  const token = safeDecode(rest);
  if (token === null) return false;
  const v = await verifyMac(token, env.SESSION_SECRET, ["v1"]);
  return v !== null && v.acct === caller;
}

/**
 * `{to, kind, link, code}` from any container: the Worker writes the sign-in or welcome mail
 * (From MAIL_FROM) and sends it, but only to the caller's own account email, with a link to this
 * install's /auth/magic for the caller's account, 20 an hour per account.
 */
async function emailSignin(request: Request, env: Env, caller: string, now: number): Promise<Response> {
  const body = await readJson(request, 16 * 1024);
  const { to, kind, link, code } = body;
  if (kind !== "signin" && kind !== "welcome") throw new HttpError(400, "bad_kind");
  if (!isAddress(to)) throw new HttpError(400, "bad_to");
  if (typeof code !== "string" || !CODE_RE.test(code)) throw new HttpError(400, "bad_code");
  if (!(await validSigninLink(env, link, caller))) throw new HttpError(400, "bad_link");
  const row = await env.DB.prepare("SELECT email FROM accounts WHERE id = ?1").bind(caller).first<{ email: string }>();
  if (!row || normAddress(row.email) !== normAddress(to)) throw new HttpError(403, "not_your_email");
  const from = mailFrom(env);
  if (!isAddress(from)) {
    log("internal.email_signin_unconfigured", { acct: caller });
    return json(503, { error: "mail_from_invalid" });
  }
  if (await overLimit(env.DB, [`signin_mail:${caller}`], SIGNIN_MAIL_PER_HOUR, now)) {
    log("internal.email_signin_limited", { acct: caller });
    return new Response(JSON.stringify({ error: "rate_limited" }), { status: 429, headers: { "content-type": "application/json", "retry-after": "3600" } });
  }
  const rcpt = normAddress(to);
  const raw = signinMessage({ from, to: rcpt, kind, link: link as string, code, now, id: mintUlid(now) });
  try {
    await env.EMAIL.send(new EmailMessage(from, rcpt, raw));
  } catch (e) {
    log("internal.email_signin_failed", { acct: caller, kind, error: errText(e) });
    return json(502, { ok: false });
  }
  log("internal.email_signin", { acct: caller, kind });
  return json(200, { ok: true });
}

// ---- GET /internal/grants, POST /internal/routing/{plan,apply} ----

/**
 * The operator's /internal/routing/<op>, with the caller's JSON body and `x-routing-account:
 * <caller>`: the operator holds the Cloudflare token and checks the caller's grants.
 */
async function routingForward(request: Request, env: Env, caller: string, op: string, call: typeof callContainer): Promise<Response> {
  const body = await readJson(request, 1024 * 1024);
  const res = await call(env, OPERATOR, `/internal/routing/${op}`, body, { routingAccount: caller });
  log("internal.routing", { acct: caller, op, status: res.status });
  return new Response(res.body, { status: res.status, headers: { "content-type": res.headers.get("content-type") ?? "application/json" } });
}

// ---- POST /internal/push ----

const MAX_PUSH_TOKENS = 20;
const APNS_TOKEN_RE = /^[0-9a-fA-F]{32,200}$/;

/** The APNs settings, from the Worker's secrets (`APNS_TEAM_ID`, `APNS_KEY_ID`, `APNS_KEY_P8`, `APNS_TOPIC`); null when any is missing. */
export function apnsConfig(env: Env): ApnsConfig | null {
  const e = env as unknown as Partial<Record<"APNS_TEAM_ID" | "APNS_KEY_ID" | "APNS_KEY_P8" | "APNS_TOPIC", string>>;
  if (!e.APNS_TEAM_ID || !e.APNS_KEY_ID || !e.APNS_KEY_P8 || !e.APNS_TOPIC) return null;
  return { teamId: e.APNS_TEAM_ID, keyId: e.APNS_KEY_ID, keyP8: e.APNS_KEY_P8, topic: e.APNS_TOPIC };
}

function clip(v: unknown, n: number): string {
  return typeof v === "string" ? v.slice(0, n) : "";
}

/**
 * `{tokens:[{token, env}], payload:{id, from, subject}}` from a container: one alert to each
 * device. Answers `{sent, dead, failed}`; the container forgets the dead tokens. No tokens is
 * not an error (`{sent: 0, dead: [], failed: 0}`); tokens without APNs configured is 503.
 */
async function push(request: Request, env: Env, caller: string, send: typeof sendApns): Promise<Response> {
  const body = await readJson(request, 64 * 1024);
  const raw = body.tokens ?? [];
  if (!Array.isArray(raw) || raw.length > MAX_PUSH_TOKENS) throw new HttpError(400, "bad_tokens");
  const devices: Device[] = raw.map((v) => {
    const t = (v ?? {}) as Record<string, unknown>;
    if (typeof t.token !== "string" || !APNS_TOKEN_RE.test(t.token)) throw new HttpError(400, "bad_token");
    const e = t.env === undefined ? "prod" : t.env;
    if (e !== "prod" && e !== "sandbox") throw new HttpError(400, "bad_env");
    return { token: t.token.toLowerCase(), env: e };
  });
  if (devices.length === 0) return json(200, { sent: 0, dead: [], failed: 0 });
  const p = (body.payload ?? {}) as Record<string, unknown>;
  if (!p || typeof p !== "object" || typeof p.id !== "string" || !p.id) throw new HttpError(400, "bad_payload");
  const cfg = apnsConfig(env);
  if (!cfg) {
    log("internal.push_unconfigured", { acct: caller, devices: devices.length });
    return json(503, { error: "apns_not_configured" });
  }
  const payload = pushPayload({ id: clip(p.id, 64), from: clip(p.from, 120), subject: clip(p.subject, 200) });
  const out = await send(cfg, devices, payload);
  log("internal.push", { acct: caller, devices: devices.length, sent: out.sent, dead: out.dead.length, failed: out.failed });
  return json(200, out);
}

// ---- PUT /internal/routes ----

function parseRoute(v: unknown): RouteInput {
  if (!v || typeof v !== "object") throw new HttpError(400, "bad_route");
  const r = v as Record<string, unknown>;
  const address = typeof r.address === "string" ? normAddress(r.address) : "";
  const wildcard = address.startsWith("*@") && DOMAIN_RE.test(address.slice(2));
  if (!wildcard && !isAddress(address)) throw new HttpError(400, "bad_route_address");
  if (typeof r.mail !== "string" || !(MAILS as readonly string[]).includes(r.mail)) throw new HttpError(400, "bad_route_mail");
  const forward_to = r.forward_to ?? [];
  if (!Array.isArray(forward_to) || forward_to.length > 10 || !forward_to.every(isAddress)) throw new HttpError(400, "bad_route_forward_to");
  if ((r.mail === "forward" || r.mail === "vault+forward") && forward_to.length === 0) throw new HttpError(400, "bad_route_forward_to");
  return { address, mail: r.mail as Mail, forward_to: forward_to.map(normAddress), can_send: r.can_send === true || r.can_send === 1 };
}

async function putRoutes(request: Request, env: Env, caller: string): Promise<Response> {
  const body = await readJson(request);
  const accountId = body.account_id;
  if (typeof accountId !== "string" || !ACCOUNT_RE.test(accountId)) throw new HttpError(400, "bad_account_id");
  if (caller !== OPERATOR && caller !== accountId) throw new HttpError(403, "not_your_account");
  if (!Array.isArray(body.routes) || body.routes.length > MAX_ROUTES) throw new HttpError(400, "bad_routes");
  const routes = body.routes.map(parseRoute);
  if (caller !== OPERATOR) {
    const foreign = await domainsNotOwnedBy(env.DB, accountId, routes.map((r) => domainOf(r.address)));
    if (foreign.length) return json(403, { error: "domain_not_granted", domains: foreign });
  }
  await upsertRoutes(env.DB, accountId, routes, { replace: body.replace === true });
  log("internal.routes", { acct: accountId, by: caller === OPERATOR ? "operator" : "account", n: routes.length, replace: body.replace === true });
  return json(200, { ok: true, n: routes.length });
}

// ---- PUT /internal/accounts (operator) ----

async function putAccounts(request: Request, env: Env): Promise<Response> {
  const body = await readJson(request);
  const accounts = body.accounts ?? [];
  const domains = body.domains ?? [];
  if (!Array.isArray(accounts) || !Array.isArray(domains)) throw new HttpError(400, "bad_body");
  const acc: AccountInput[] = accounts.map((v) => {
    const a = (v ?? {}) as Record<string, unknown>;
    if (typeof a.id !== "string" || !ACCOUNT_RE.test(a.id)) throw new HttpError(400, "bad_account_id");
    if (!isAddress(a.email)) throw new HttpError(400, "bad_account_email");
    if (typeof a.status !== "string" || !STATUSES.includes(a.status)) throw new HttpError(400, "bad_account_status");
    const has_gmail = a.has_gmail === undefined || a.has_gmail === null ? undefined : a.has_gmail === true || a.has_gmail === 1;
    return { id: a.id, email: a.email, status: a.status, has_gmail };
  });
  const dom: DomainInput[] = domains.map((v) => {
    const d = (v ?? {}) as Record<string, unknown>;
    const name = typeof d.name === "string" ? d.name.trim().toLowerCase() : "";
    if (!DOMAIN_RE.test(name)) throw new HttpError(400, "bad_domain");
    if (typeof d.account_id !== "string" || !ACCOUNT_RE.test(d.account_id)) throw new HttpError(400, "bad_account_id");
    return { name, account_id: d.account_id };
  });
  const replace = body.replace_domains_for ?? [];
  if (!Array.isArray(replace) || !replace.every((x) => typeof x === "string" && ACCOUNT_RE.test(x))) throw new HttpError(400, "bad_replace_domains_for");
  await upsertAccounts(env.DB, acc, dom, { replaceDomainsFor: replace as string[] });
  log("internal.accounts", { accounts: acc.length, domains: dom.length, replaced: replace.length });
  return json(200, { ok: true, accounts: acc.length, domains: dom.length });
}

/** Route one `/internal/*` request. */
export async function handleInternal(request: Request, env: Env, deps: Partial<InternalDeps> = {}): Promise<Response> {
  const wake = deps.wake ?? defaultWake;
  const call = deps.call ?? callContainer;
  const now = (deps.now ?? Date.now)();
  const url = new URL(request.url);
  const { pathname } = url;
  const m = request.method;
  // The container's own health (its DO pingEndpoint), reachable from outside for operations and
  // for `curl localhost:8787/internal/health` under wrangler dev: the account named by
  // x-account-id, else DEV_ACCOUNT on a localhost host; the secret must be that account's.
  const healthCheck = m === "GET" && pathname === "/internal/health";
  const named = request.headers.get("x-account-id")?.trim();
  const claimed = named || (healthCheck && localDev(env) ? env.DEV_ACCOUNT : undefined);
  const caller = await authenticatedCaller(request, env, claimed);
  if (!caller) return json(401, { error: "unauthorized" });
  try {
    if (healthCheck) return await containerFor(env, caller).proxy(new Request("http://container/internal/health"), caller, { internal: true });
    if (m === "POST" && pathname === "/internal/email/send") return await emailSend(request, env, caller);
    if (m === "POST" && pathname === "/internal/email/signin") return await emailSignin(request, env, caller, now);
    if (m === "GET" && pathname === "/internal/grants") return json(200, { domains: await domainsOf(env.DB, caller) });
    const rt = /^\/internal\/routing\/(plan|apply)$/.exec(pathname);
    if (m === "POST" && rt) return await routingForward(request, env, caller, rt[1]!, call);
    if (m === "PUT" && pathname === "/internal/routes") return await putRoutes(request, env, caller);
    if (m === "PUT" && pathname === "/internal/accounts") {
      requireOperator(caller);
      return await putAccounts(request, env);
    }
    const w = /^\/internal\/wake\/([^/]+)$/.exec(pathname);
    if (m === "POST" && w) {
      requireOperator(caller);
      const acct = safeDecode(w[1]!);
      if (acct === null || !ACCOUNT_RE.test(acct)) throw new HttpError(400, "bad_account_id");
      // `{provision:{email, is_admin, display_name?}}` (the operator's approve) becomes the new
      // account's POST /internal/provision, and its reply is the answer; no body is a plain wake.
      const text = await request.text();
      if (text.trim()) {
        let body: unknown;
        try {
          body = JSON.parse(text);
        } catch {
          throw new HttpError(400, "bad_json");
        }
        const provision = (body as Record<string, unknown> | null)?.provision;
        if (provision !== undefined) {
          const p = provision as Record<string, unknown> | null;
          if (!p || typeof p !== "object" || !isAddress(p.email)) throw new HttpError(400, "bad_provision");
          const res = await call(env, acct, "/internal/provision", provision);
          log("internal.provision", { acct, status: res.status });
          return new Response(res.body, { status: res.status, headers: { "content-type": res.headers.get("content-type") ?? "application/json" } });
        }
      }
      await wake(env, acct);
      return json(202, { ok: true });
    }
    if (m === "POST" && pathname === "/internal/push") return await push(request, env, caller, deps.sendApns ?? sendApns);
    return json(404, { error: "not_found" });
  } catch (e) {
    if (e instanceof HttpError) return json(e.status, { error: e.code });
    log("internal.error", { path: pathname, error: errText(e) });
    return json(500, { error: "internal" });
  }
}
