import type { Env } from "./env";
import { errText, log } from "./log";
import { domainOf, lookupRoute, normAddress, ownDomains } from "./routes";
import { ulid as mintUlid } from "./ulid";
import { wake as defaultWake, type WakeFn } from "./wake";

/** Email Routing hands a Worker at most 25 MiB; anything larger is refused rather than half-kept. */
export const RAW_MAX = 25 * 1024 * 1024;
export const FORWARD_BREAKER_PER_HOUR = 300;
export const FORWARD_HEADER = "X-LeanMail-Forwarded";
const RECEIVED_MAX = 20;
const HOUR_MS = 3_600_000;

export interface EmailDeps {
  wake: WakeFn;
  now: () => number;
}

/**
 * Store-only signals (port of MailVault's `loopGuard`): mail carrying any of these is kept but never
 * forwarded. Returns the reason, or null when forwarding is safe.
 */
export function loopGuard(headers: Headers): string | null {
  if (headers.get(FORWARD_HEADER)) return "leanmail_forwarded";
  if (headers.get("x-forwarded-for") || headers.get("x-forwarded-to")) return "gmail_forwarded";
  if (headers.get("x-google-group-id")) return "google_group";
  const auto = (headers.get("auto-submitted") ?? "").trim().toLowerCase();
  if (auto && auto !== "no") return "auto_submitted";
  const precedence = (headers.get("precedence") ?? "").trim().toLowerCase();
  if (["bulk", "list", "junk", "auto_reply"].includes(precedence)) return "precedence";
  // Headers.get() joins repeated Received headers with ", "; count the hops heuristically.
  const receivedValue = headers.get("received") ?? "";
  const received = receivedValue ? receivedValue.split(/,\s*(?=(?:from|by)\s)/i).length : 0;
  if (received > RECEIVED_MAX) return "received_count";
  return null;
}

/**
 * Count one forward against the account's hourly window, atomically; true once the window holds
 * more than FORWARD_BREAKER_PER_HOUR. A tripped breaker keeps mail, it only stops copies.
 */
export async function breakerTripped(db: D1Database, acct: string, now: number): Promise<boolean> {
  const row = await db
    .prepare(
      `INSERT INTO forward_window (account_id, window_start, count) VALUES (?1, ?2, 1)
       ON CONFLICT(account_id) DO UPDATE SET
         count = CASE WHEN ?2 - window_start >= ${HOUR_MS} THEN 1 ELSE count + 1 END,
         window_start = CASE WHEN ?2 - window_start >= ${HOUR_MS} THEN ?2 ELSE window_start END
       RETURNING count`,
    )
    .bind(acct, now)
    .first<{ count: number }>();
  return (row?.count ?? 0) > FORWARD_BREAKER_PER_HOUR;
}

/** Targets that are real addresses and not on a domain some LeanMail account receives (a loop). */
async function deliverable(db: D1Database, targets: string[]): Promise<string[]> {
  const clean = [...new Set(targets.map(normAddress).filter((t) => domainOf(t) !== ""))];
  const own = await ownDomains(db, [...new Set(clean.map(domainOf))]);
  return clean.filter((t) => !own.has(domainOf(t)));
}

async function forwardEach(message: ForwardableEmailMessage, targets: string[], marker: string): Promise<number> {
  let ok = 0;
  for (const to of targets) {
    try {
      const h = new Headers();
      h.set(FORWARD_HEADER, marker);
      await message.forward(to, h);
      ok++;
    } catch (e) {
      log("email.forward_failed", { error: errText(e) });
    }
  }
  return ok;
}

/**
 * Email Routing delivery. Decides vault / vault+forward / forward / drop from the D1 `routes`
 * projection; vaulted mail goes to the R2 spool (`raw/<acct>/<ulid>` then the `in/` marker) and the
 * account's container gets a wake hint it is never waited on for.
 */
export async function handleEmail(message: ForwardableEmailMessage, env: Env, ctx: ExecutionContext, deps: Partial<EmailDeps> = {}): Promise<void> {
  const wake = deps.wake ?? defaultWake;
  const now = (deps.now ?? Date.now)();
  const rcpt = normAddress(message.to);
  const zone = domainOf(rcpt);
  const size = message.rawSize;
  const route = await lookupRoute(env.DB, rcpt);
  const decision = (acct: string | null, mail: string, forwarded: number, extra: Record<string, unknown> = {}) =>
    log("email.decision", { acct, mail, forwarded, size, ...extra });

  if (!route || route.mail === "drop") {
    decision(route?.account_id ?? null, "drop", 0, { match: route?.match ?? "none" });
    return;
  }
  const acct = route.account_id;

  if (route.mail === "forward") {
    const targets = await deliverable(env.DB, route.forward_to);
    if (targets.length === 0) {
      // A forward-only address with nowhere to go is a misconfiguration; bounce rather than lose it.
      decision(acct, "forward", 0, { match: route.match, error: "no_target" });
      message.setReject("address not deliverable");
      return;
    }
    const ok = await forwardEach(message, targets, `fo@${zone}`);
    decision(acct, "forward", ok, { match: route.match, targets: targets.length });
    // Nothing stored and nothing delivered: a temporary failure, so the sender retries.
    if (ok === 0) throw new Error("forward-only delivery failed");
    return;
  }

  // vault or vault+forward
  if (size > RAW_MAX) {
    decision(acct, route.mail, 0, { match: route.match, error: "oversize" });
    message.setReject("message too large");
    return;
  }
  const bytes = new Uint8Array(await new Response(message.raw).arrayBuffer());
  const id = mintUlid(now);
  // R2 failures throw: Email Routing answers with a temporary failure and the sender retries.
  await env.RAW.put(`raw/${acct}/${id}`, bytes, {
    customMetadata: { rcpt, from: message.from, received_at: String(now), source: "route", size: String(bytes.byteLength) },
  });
  await env.RAW.put(`in/${acct}/${id}`, new Uint8Array(0));
  ctx.waitUntil(wake(env, acct, id));

  let forwarded = 0;
  const extra: Record<string, unknown> = { match: route.match };
  if (route.mail === "vault+forward") {
    const targets = await deliverable(env.DB, route.forward_to);
    const guard = targets.length ? loopGuard(message.headers) : null;
    if (targets.length === 0) extra.store_only = "no_target";
    else if (guard) extra.store_only = guard;
    else if (await breakerTripped(env.DB, acct, now)) extra.store_only = "breaker";
    // The raw is already durable: a failed forward is logged, never thrown (a retry would store again).
    else forwarded = await forwardEach(message, targets, `${id}@${zone}`);
  }
  decision(acct, route.mail, forwarded, extra);
}
