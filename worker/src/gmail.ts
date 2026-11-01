import { proxyTo } from "./auth";
import type { Env } from "./env";
import { errText, log } from "./log";
import { ACCOUNT_RE, b64uEncode, deriveInternalSecret, secretEqual, verifyMac } from "./tokens";
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
 * A notification carries no mail, so a forged one could at most cause a poll; the signature keeps
 * even that to Pub/Sub.
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
  const acct = decodeURIComponent(m[1]!);
  const sig = decodeURIComponent(m[2]!);
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
  ctx.waitUntil(
    call(env, acct, "/internal/gmail/push", payload)
      .then(async (r) => {
        await r.body?.cancel();
        log("gmail.hook", { acct, status: r.status });
      })
      .catch((e) => log("gmail.hook_failed", { acct, error: errText(e) })),
  );
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
  return proxy(env, v.acct, request);
}
