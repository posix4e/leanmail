import { logFor } from "./accountlog";
import { proxyTo } from "./auth";
import type { Env } from "./env";
import { errText, log } from "./log";
import { safeDecode } from "./paths";
import { ACCOUNT_RE, b64uEncode, deriveInternalSecret, secretEqual, signMac, verifyMac } from "./tokens";
import { callContainer } from "./wake";

/**
 * Gmail's two public entry points (lean/Server/Gmail/{Watch,OAuth}.lean):
 *
 *   POST /hooks/gmail/:acct/:sig   Pub/Sub push. `sig` = base64url(HMAC-SHA256(K, "gmail-push:" + acct))
 *                                  with K = deriveInternalSecret(INTERNAL_SECRET, acct), the secret
 *                                  that account's container holds (so the container can show the URL
 *                                  and no account can mint another's). Checked in constant time; the
 *                                  notification's {emailAddress, historyId} goes to the container's
 *                                  POST /internal/gmail/push in the background, and Pub/Sub gets
 *                                  204 at once (it retries a slow answer).
 *   GET  /oauth/google/callback    Google's redirect after consent. `state` is
 *                                  v1.<acct>.goauth.<nonce>.<exp>.<mac>, MAC'd like every credential;
 *                                  the Worker routes it to the account it names, which re-verifies
 *                                  the MAC, the expiry and its single-use nonce.
 *
 * Both go to the account's log object when that holds the connection (src/gmailsync.ts), and to the
 * container otherwise, so an account connected the old way keeps working until containers go.
 *
 * A notification carries no mail, so a forged one could at most cause a poll; the signature keeps
 * even that to Pub/Sub.
 *
 * The rest is for the apps and the CLI, under /sync/gmail (src/sync.ts sends them here):
 *
 *   GET    /sync/gmail                        `read`   {connection, queue and backfill, hook_url}
 *   PUT    /sync/gmail/client                 every    {client_id, client_secret, pubsub_topic?}
 *   POST   /sync/gmail/authorize              every    {url}: open it in a browser
 *   DELETE /sync/gmail                        every    forget the connection; stored mail stays
 *   POST   /sync/gmail/backfill               `act`    {keep_forever?}
 *   POST   /sync/gmail/backfill/<action>      `act`    pause, resume, cancel or retry-failed
 *
 * "every" means a token with all the scopes, which is what signing in gives. The container asked
 * for a session here, and these change whose mail comes in, so an agent's narrower token cannot.
 */

export const OAUTH_PURPOSE = "goauth";
const MAX_PUSH_BYTES = 64 * 1024;
const enc = new TextEncoder();

async function hmac(secret: string, data: string): Promise<Uint8Array> {
  const key = await crypto.subtle.importKey("raw", enc.encode(secret), { name: "HMAC", hash: "SHA-256" }, false, ["sign"]);
  return new Uint8Array(await crypto.subtle.sign("HMAC", key, enc.encode(data)));
}

/** The push URL's signature for `acct`, from the master INTERNAL_SECRET. */
export async function gmailHookSig(master: string, acct: string): Promise<string> {
  return b64uEncode(await hmac(await deriveInternalSecret(master, acct), `gmail-push:${acct}`));
}

export interface GmailDeps {
  call: typeof callContainer;
  proxy: typeof proxyTo;
}

function text(status: number, body = ""): Response {
  return new Response(body || null, { status, headers: { "content-type": "text/plain; charset=utf-8", "cache-control": "no-store" } });
}

/** What Gmail put in a Pub/Sub message: base64 JSON `{emailAddress, historyId}`. Null when absent or malformed. */
export function pushPayload(body: string): { emailAddress?: string; historyId?: string } | null {
  try {
    const data = (JSON.parse(body) as { message?: { data?: unknown } } | null)?.message?.data;
    if (typeof data !== "string" || !data) return null;
    const inner = JSON.parse(atob(data.replace(/-/g, "+").replace(/_/g, "/"))) as { emailAddress?: unknown; historyId?: unknown };
    const out: { emailAddress?: string; historyId?: string } = {};
    if (typeof inner.emailAddress === "string") out.emailAddress = inner.emailAddress.slice(0, 320);
    if (typeof inner.historyId === "string" || typeof inner.historyId === "number") out.historyId = String(inner.historyId).slice(0, 40);
    return out;
  } catch {
    return null;
  }
}

/** POST /hooks/gmail/:acct/:sig. */
export async function handleGmailHook(request: Request, env: Env, ctx: ExecutionContext, deps: Partial<GmailDeps> = {}): Promise<Response> {
  const call = deps.call ?? callContainer;
  const m = /^\/hooks\/gmail\/([^/]+)\/([^/]+)$/.exec(new URL(request.url).pathname);
  if (!m) return text(404);
  if (request.method !== "POST") return text(405);
  const acct = safeDecode(m[1]!);
  const sig = safeDecode(m[2]!);
  if (acct === null || sig === null) {
    await request.body?.cancel();
    return text(400);
  }
  if (!env.INTERNAL_SECRET || !ACCOUNT_RE.test(acct) || !(await secretEqual(sig, await gmailHookSig(env.INTERNAL_SECRET, acct)))) {
    log("gmail.hook_refused", { configured: !!env.INTERNAL_SECRET });
    await request.body?.cancel();
    return text(403);
  }
  // Read the body before answering: after the response it is no longer guaranteed readable.
  const bytes = await request.arrayBuffer();
  if (bytes.byteLength > MAX_PUSH_BYTES) return text(413);
  const payload = pushPayload(new TextDecoder().decode(bytes));
  if (!payload) {
    // Acknowledged all the same: Pub/Sub would only redeliver the same malformed message.
    log("gmail.hook_ignored", { acct, reason: "no_data" });
    return text(204);
  }
  const handOn = async () => {
    if (await logFor(env, acct).gmailPush(payload.emailAddress, payload.historyId, Date.now())) {
      log("gmail.hook", { acct, to: "log" });
      return;
    }
    const r = await call(env, acct, "/internal/gmail/push", payload);
    await r.body?.cancel();
    log("gmail.hook", { acct, status: r.status });
  };
  ctx.waitUntil(handOn().catch((e) => log("gmail.hook_failed", { acct, error: errText(e) })));
  return text(204);
}

const BAD_STATE_HTML =
  '<!doctype html><meta charset=utf-8><title>LeanMail</title><p>This Google authorization link is not valid. <a href="/settings/gmail">Start again from Settings.</a></p>';

/** GET /oauth/google/callback?code&state: to the account the state's MAC names. */
export async function handleOAuthCallback(request: Request, env: Env, deps: Partial<GmailDeps> = {}): Promise<Response> {
  const proxy = deps.proxy ?? proxyTo;
  if (request.method !== "GET") return text(405);
  const state = new URL(request.url).searchParams.get("state") ?? "";
  const v = await verifyMac(state, env.SESSION_SECRET, ["v1"]);
  if (!v || !v.body.startsWith(`${OAUTH_PURPOSE}.`)) {
    log("gmail.callback_refused", {});
    return new Response(BAD_STATE_HTML, { status: 400, headers: { "content-type": "text/html; charset=utf-8", "cache-control": "no-store" } });
  }
  const url = new URL(request.url);
  const [, nonce = ""] = v.body.split(".");
  const code = url.searchParams.get("error") ? "" : (url.searchParams.get("code") ?? "");
  const outcome = await logFor(env, v.acct).gmailFinishAuthorize(nonce, code, Date.now());
  if (outcome === null) return proxy(env, v.acct, request);
  log("gmail.callback", { acct: v.acct, outcome: outcome || "connected" });
  const line = outcome === "" ? "Gmail is connected. You can go back to LeanMail." : "Gmail did not connect. Go back to LeanMail and try again.";
  const page = `<!doctype html><meta charset=utf-8><title>LeanMail</title><p>${line}`;
  return new Response(page, { status: outcome === "" ? 200 : 400, headers: { "content-type": "text/html; charset=utf-8", "cache-control": "no-store" } });
}

const CLIENT_ID_RE = /\.apps\.googleusercontent\.com$/;
const TOPIC_RE = /^projects\/[^/\s]+\/topics\/[^/\s]+$/;
const BACKFILL_ACTIONS = new Set(["pause", "resume", "cancel", "retry-failed"]);
const ALL_SCOPES = ["read", "draft", "send", "act", "junk"];

function json(status: number, body: unknown): Response {
  return new Response(JSON.stringify(body), { status, headers: { "content-type": "application/json", "cache-control": "no-store" } });
}

/** What the apps and the CLI see of a refusal: 400 for the owner to set up, 409 when it is running. */
function refused(code: string): Response {
  return json(code === "not_found" ? 404 : code === "already_running" ? 409 : 400, { error: code });
}

async function readBody(request: Request): Promise<Record<string, unknown> | null> {
  const body = await request.json().catch(() => null);
  return typeof body === "object" && body !== null && !Array.isArray(body) ? (body as Record<string, unknown>) : null;
}

async function saveClient(request: Request, env: Env, acct: string): Promise<Response> {
  const body = await readBody(request);
  if (!body) return json(400, { error: "bad_json" });
  const id = typeof body.client_id === "string" ? body.client_id.trim() : "";
  const secret = typeof body.client_secret === "string" ? body.client_secret.trim() : "";
  const topic = typeof body.pubsub_topic === "string" ? body.pubsub_topic.trim() : "";
  if (!CLIENT_ID_RE.test(id) || id.length > 200) return json(400, { error: "bad_client_id" });
  if (secret.length < 10 || secret.length > 200) return json(400, { error: "bad_client_secret" });
  if (topic && !TOPIC_RE.test(topic)) return json(400, { error: "bad_pubsub_topic" });
  await logFor(env, acct).gmailSaveClient(acct, id, secret, topic || null);
  return json(200, { ok: true });
}

async function authorize(env: Env, acct: string, now: number): Promise<Response> {
  if (!env.SESSION_SECRET) return json(500, { error: "not_configured" });
  const nonce = b64uEncode(crypto.getRandomValues(new Uint8Array(18)));
  const state = await signMac("v1", acct, `${OAUTH_PURPOSE}.${nonce}.${now + 10 * 60_000}`, env.SESSION_SECRET);
  const url = await logFor(env, acct).gmailAuthorizeUrl(nonce, state, now);
  return url ? json(200, { url }) : refused("no_client");
}

/** Everything under /sync/gmail, for a caller src/sync.ts has already authenticated. */
export async function handleSyncGmail(request: Request, env: Env, acct: string, scopes: string[], now: number): Promise<Response> {
  const path = new URL(request.url).pathname;
  const m = request.method;
  const account = logFor(env, acct);
  const every = ALL_SCOPES.every((s) => scopes.includes(s));
  const forbidden = () => json(403, { error: "insufficient_scope" });

  if (path === "/sync/gmail") {
    if (m === "GET" || m === "HEAD") {
      if (!scopes.includes("read")) return forbidden();
      const hook = env.INTERNAL_SECRET ? `${env.PUBLIC_ORIGIN.replace(/\/+$/, "")}/hooks/gmail/${acct}/${await gmailHookSig(env.INTERNAL_SECRET, acct)}` : null;
      return json(200, { ...(JSON.parse(await account.gmailStatus()) as object), hook_url: hook });
    }
    if (m !== "DELETE") return json(405, { error: "method_not_allowed" });
    if (!every) return forbidden();
    await account.gmailForget();
    return json(200, { ok: true });
  }
  if (path === "/sync/gmail/client" || path === "/sync/gmail/authorize") {
    if (m !== (path.endsWith("client") ? "PUT" : "POST")) return json(405, { error: "method_not_allowed" });
    if (!every) return forbidden();
    return path.endsWith("client") ? saveClient(request, env, acct) : authorize(env, acct, now);
  }
  const action = /^\/sync\/gmail\/backfill(?:\/([a-z-]+))?$/.exec(path);
  if (!action) return json(404, { error: "not_found" });
  if (m !== "POST") return json(405, { error: "method_not_allowed" });
  if (!scopes.includes("act")) return forbidden();
  if (action[1] !== undefined && !BACKFILL_ACTIONS.has(action[1])) return json(404, { error: "not_found" });
  const body = action[1] === undefined ? await readBody(request) : null;
  const reason = action[1] === undefined ? await account.gmailStartBackfill(body?.keep_forever === true, now) : await account.gmailControlBackfill(action[1], now);
  return reason ? refused(reason) : json(200, JSON.parse(await account.gmailStatus()));
}
