import type { MailContainer } from "./container";
import type { Env } from "./env";
import { errText, log } from "./log";

/**
 * The account's container stub: what `getContainer` from @cloudflare/containers does, written out
 * because its `T extends Container<Cloudflare.Env>` bound rejects a container typed on ContainerEnv
 * once tests augment Cloudflare.Env.
 */
export function containerFor(env: Env, acct: string): DurableObjectStub<MailContainer> {
  return env.MAIL.get(env.MAIL.idFromName(acct));
}

/** POST a path to an account's container through its Durable Object (which stamps the secret). */
export async function callContainer(env: Env, acct: string, path: string, body?: unknown): Promise<Response> {
  const init: RequestInit = { method: "POST" };
  if (body !== undefined) {
    init.body = JSON.stringify(body);
    init.headers = { "content-type": "application/json" };
  }
  return containerFor(env, acct).proxy(new Request(`http://container${path}`, init), acct);
}

export type WakeFn = (env: Env, acct: string, ulid?: string) => Promise<void>;

/**
 * The wake hint after a spool write. A hint only: the container drains `in/<acct>/` on boot and on
 * every wake, so a failure here costs latency, never mail. Never throws.
 */
export const wake: WakeFn = async (env, acct, ulid) => {
  try {
    const res = await callContainer(env, acct, "/internal/wake", ulid ? { ulid } : {});
    if (!res.ok) log("wake.status", { acct, status: res.status });
    await res.body?.cancel();
  } catch (e) {
    log("wake.failed", { acct, error: errText(e) });
  }
};
