import { EmailMessage } from "cloudflare:email";
import type { Env } from "./env";
import { errText, log } from "./log";
import {
  MAILS,
  domainOf,
  domainsNotOwnedBy,
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
import { ACCOUNT_RE, deriveInternalSecret, secretEqual } from "./tokens";
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

async function emailSend(request: Request, env: Env, caller: string): Promise<Response> {
  const body = await readJson(request);
  const { from, to, raw_b64 } = body;
  if (!isAddress(from)) throw new HttpError(400, "bad_from");
  if (!Array.isArray(to) || to.length === 0 || to.length > MAX_SEND_RCPTS || !to.every(isAddress)) throw new HttpError(400, "bad_to");
  if (typeof raw_b64 !== "string" || raw_b64.length === 0) throw new HttpError(400, "bad_raw_b64");
  const raw = decodeB64(raw_b64);
  if (raw.byteLength > MAX_SEND_BYTES) throw new HttpError(413, "too_large");
  // An account sends only as an address it owns and may send from, or as the install's MAIL_FROM
  // (its sign-in and welcome mail). The operator may send as anything.
  if (caller !== OPERATOR && normAddress(from) !== mailFrom(env)) {
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
  const url = new URL(request.url);
  const { pathname } = url;
  const m = request.method;
  // The container's own health (its DO pingEndpoint), reachable from outside for operations and
  // for `curl localhost:8787/internal/health` under wrangler dev: the account named by
  // x-account-id, else DEV_ACCOUNT on a localhost host; the secret must be that account's.
  const healthCheck = m === "GET" && pathname === "/internal/health";
  const named = request.headers.get("x-account-id")?.trim();
  const claimed = named || (healthCheck && LOCAL_HOSTS.has(url.hostname) ? env.DEV_ACCOUNT : undefined);
  const caller = await authenticatedCaller(request, env, claimed);
  if (!caller) return json(401, { error: "unauthorized" });
  try {
    if (healthCheck) return await containerFor(env, caller).proxy(new Request("http://container/internal/health"), caller);
    if (m === "POST" && pathname === "/internal/email/send") return await emailSend(request, env, caller);
    if (m === "PUT" && pathname === "/internal/routes") return await putRoutes(request, env, caller);
    if (m === "PUT" && pathname === "/internal/accounts") {
      requireOperator(caller);
      return await putAccounts(request, env);
    }
    const w = /^\/internal\/wake\/([^/]+)$/.exec(pathname);
    if (m === "POST" && w) {
      requireOperator(caller);
      const acct = decodeURIComponent(w[1]!);
      if (!ACCOUNT_RE.test(acct)) throw new HttpError(400, "bad_account_id");
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
