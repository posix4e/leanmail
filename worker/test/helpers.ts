import { env as rawEnv } from "cloudflare:workers";
import type { Env } from "../src/env";

export const env = rawEnv as unknown as Env;
export const SECRET = "test-internal-secret";

export async function resetDb(): Promise<void> {
  await env.DB.batch(["accounts", "domains", "routes", "forward_window", "auth_rate"].map((t) => env.DB.prepare(`DELETE FROM ${t}`)));
}

export async function addRoute(address: string, account_id: string, mail: string, forward_to: string[] = [], can_send = 0): Promise<void> {
  await env.DB.prepare("INSERT INTO routes (address, account_id, mail, forward_to, can_send, updated_at) VALUES (?1, ?2, ?3, ?4, ?5, 0)")
    .bind(address, account_id, mail, JSON.stringify(forward_to), can_send)
    .run();
}

export async function addDomain(name: string, account_id: string): Promise<void> {
  await env.DB.prepare("INSERT INTO domains (name, account_id) VALUES (?1, ?2)").bind(name, account_id).run();
}

export interface FakeMessage extends ForwardableEmailMessage {
  forwards: { to: string; headers: Record<string, string> }[];
  rejected: string | null;
  rawReads: number;
}

/** A ForwardableEmailMessage with spies on forward() and setReject(). */
export function fakeMessage(opts: { from?: string; to: string; raw?: string; rawSize?: number; headers?: Record<string, string> | [string, string][]; forwardFails?: boolean }): FakeMessage {
  const raw = new TextEncoder().encode(opts.raw ?? "From: a@b.example\r\nSubject: hi\r\n\r\nbody\r\n");
  const m = {
    from: opts.from ?? "sender@outside.example",
    to: opts.to,
    headers: new Headers(opts.headers ?? {}),
    rawSize: opts.rawSize ?? raw.byteLength,
    forwards: [] as FakeMessage["forwards"],
    rejected: null as string | null,
    rawReads: 0,
    get raw() {
      m.rawReads++;
      return new Response(raw).body!;
    },
    setReject(reason: string) {
      m.rejected = reason;
    },
    async forward(to: string, headers?: Headers) {
      if (opts.forwardFails) throw new Error("forward refused");
      m.forwards.push({ to, headers: Object.fromEntries(headers ?? new Headers()) });
      return { messageId: "x" };
    },
    async reply() {
      return { messageId: "x" };
    },
  };
  return m as unknown as FakeMessage;
}

/** Collect the JSON lines console.log writes while `fn` runs. */
export async function captureLogs<T>(fn: () => Promise<T>): Promise<{ result: T; lines: Record<string, unknown>[]; text: string[] }> {
  const orig = console.log;
  const text: string[] = [];
  console.log = (...a: unknown[]) => {
    text.push(a.map(String).join(" "));
  };
  try {
    const result = await fn();
    const lines = text.flatMap((t) => {
      try {
        return [JSON.parse(t) as Record<string, unknown>];
      } catch {
        return [];
      }
    });
    return { result, lines, text };
  } finally {
    console.log = orig;
  }
}
