import { MailContainer } from "./container";
import { handleEmail } from "./email";
import type { Env } from "./env";
import { handleAuth, isNavigation, proxyTo, pruneAuthRate, toSignin } from "./auth";
import { LOCAL_HOSTS, OPERATOR, handleInternal } from "./internal";
import { handleGmailHook, handleOAuthCallback } from "./gmail";
import { errText, log } from "./log";
import { canonicalPath, firstSegment } from "./paths";
import { listActiveAccounts } from "./routes";
import { accountFromRequest } from "./tokens";
import { callContainer } from "./wake";

export { MailContainer };
export type { Env };

const WORKER_VERSION = "0.1.0";
const DAILY_CONCURRENCY = 5;

function json(status: number, body: unknown, headers: Record<string, string> = {}): Response {
  return new Response(JSON.stringify(body), { status, headers: { "content-type": "application/json", ...headers } });
}

/**
 * The account a public request goes to: the MAC-verified cookie or bearer token, else DEV_ACCOUNT
 * (worker/.dev.vars), honoured only for a localhost host so a stray production var opens nothing.
 */
export async function accountFor(request: Request, env: Env): Promise<string | null> {
  const acct = await accountFromRequest(request, env.SESSION_SECRET);
  if (acct) return acct;
  if (env.DEV_ACCOUNT && LOCAL_HOSTS.has(new URL(request.url).hostname)) return env.DEV_ACCOUNT;
  return null;
}

/** Run `fn` over `items`, at most `limit` at a time. */
export async function mapLimit<T>(items: T[], limit: number, fn: (item: T) => Promise<void>): Promise<void> {
  let next = 0;
  const lane = async () => {
    while (next < items.length) await fn(items[next++]!);
  };
  await Promise.all(Array.from({ length: Math.min(limit, items.length) }, lane));
}

export type TickFn = (env: Env, acct: string) => Promise<void>;

const tickContainer: TickFn = async (env, acct) => {
  const res = await callContainer(env, acct, "/internal/tick?daily=1");
  await res.body?.cancel();
  if (!res.ok) throw new Error(`tick status ${res.status}`);
};

/** The daily cron: wake every active account for its daily jobs. Errors are logged per account. */
export async function dailyTick(env: Env, tick: TickFn = tickContainer): Promise<{ ok: number; failed: number }> {
  await pruneAuthRate(env.DB).catch((e) => log("cron.prune_failed", { error: errText(e) }));
  const accounts = await listActiveAccounts(env.DB);
  const out = { ok: 0, failed: 0 };
  await mapLimit(accounts, DAILY_CONCURRENCY, async (acct) => {
    try {
      await tick(env, acct);
      out.ok++;
    } catch (e) {
      out.failed++;
      log("cron.tick_failed", { acct, error: errText(e) });
    }
  });
  log("cron.daily", { accounts: accounts.length, ...out });
  return out;
}

/** Pages every container serves alike, which a signed-out browser must reach: the operator's. */
function isPublicPage(method: string, path: string): boolean {
  if (path === "/signin" || path === "/signup") return method === "GET" || method === "POST";
  return method === "GET" && path.startsWith("/static/");
}

/**
 * The public front door. First the path is canonicalised (src/paths.ts): one that cannot be is a
 * 404, and so is any path that only canonicalises to `/internal/*` (`/%69nternal/x`,
 * `//internal/x`, `/INTERNAL/x`): the Worker's own /internal/* answers only its plain spelling,
 * and a client never reaches a container's. (`/internal/../internal/x` is `/internal/x` once the
 * URL is parsed, so it is that handler's, behind a container secret.) Every later check, and the
 * container, sees the canonical path. Order: /health; /internal/* (containers); the Gmail hook and OAuth
 * callback (src/gmail.ts); the sign-in routes and
 * /admin/* (src/auth.ts); a MAC-verified cookie or bearer → that account; DEV_ACCOUNT on
 * localhost; signed-out sign-in and sign-up pages and static files → the operator; a signed-out browser
 * navigation → 302 /signin; anything else 401.
 */
export async function handleFetch(request: Request, env: Env, ctx: ExecutionContext): Promise<Response> {
  const url = new URL(request.url);
  const canon = canonicalPath(url.pathname);
  if (canon === null) return json(404, { error: "not_found" });
  if (firstSegment(canon) === "internal") {
    if (canon !== url.pathname || !(canon === "/internal" || canon.startsWith("/internal/"))) return json(404, { error: "not_found" });
    return handleInternal(request, env);
  }
  if (canon !== url.pathname) {
    url.pathname = canon;
    request = new Request(url, request);
  }
  if (url.pathname === "/health") {
    return json(200, { ok: true, worker: "leanmail", version: WORKER_VERSION });
  }
  // Gmail: Pub/Sub push (signed URL) and Google's OAuth redirect (MAC'd state), src/gmail.ts
  if (url.pathname.startsWith("/hooks/gmail/")) return handleGmailHook(request, env, ctx);
  if (url.pathname === "/oauth/google/callback") return handleOAuthCallback(request, env);
  const auth = await handleAuth(request, env, ctx);
  if (auth) return auth;
  const accountId = await accountFor(request, env);
  if (accountId) return proxyTo(env, accountId, request);
  if (isPublicPage(request.method, url.pathname)) return proxyTo(env, OPERATOR, request);
  if (isNavigation(request)) return toSignin();
  return json(401, { error: "unauthorized" });
}

export default {
  fetch(request: Request, env: Env, ctx: ExecutionContext): Promise<Response> {
    return handleFetch(request, env, ctx);
  },

  async email(message: ForwardableEmailMessage, env: Env, ctx: ExecutionContext): Promise<void> {
    await handleEmail(message, env, ctx);
  },

  async scheduled(controller: ScheduledController, env: Env, ctx: ExecutionContext): Promise<void> {
    if (controller.cron !== "0 4 * * *") {
      log("cron.unknown", { cron: controller.cron });
      return;
    }
    ctx.waitUntil(dailyTick(env).then(() => undefined));
  },
} satisfies ExportedHandler<Env>;
