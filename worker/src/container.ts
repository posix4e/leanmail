import { Container } from "@cloudflare/containers";
import { deriveSecret } from "./tokens";

/** Everything the container reads from its environment. Set from Worker vars and secrets. */
export interface ContainerEnv {
  PUBLIC_ORIGIN: string;
  SIGNUP_MODE: string;
  RETENTION_DAYS_DEFAULT: string;
  SESSION_SECRET?: string;
  INTERNAL_SECRET?: string;
  R2_BUCKET?: string;
  R2_ENDPOINT?: string;
  R2_ACCESS_KEY_ID?: string;
  R2_SECRET_ACCESS_KEY?: string;
  AI_BASE_URL?: string;
  AI_API_KEY?: string;
  AI_MODEL?: string;
  /** The Cloudflare API token the routing settings page applies plans with (optional). */
  CF_API_TOKEN?: string;
  /** The Cloudflare account that token belongs to (destination addresses). */
  CF_ACCOUNT_ID?: string;
  ADMIN_EMAILS?: string;
  /** The From of sign-in mail; default `no-reply@<host of PUBLIC_ORIGIN>` on both sides. */
  MAIL_FROM?: string;
  /** `projects/<p>/topics/<t>`: the Pub/Sub topic Gmail's watch publishes to (optional). */
  GOOGLE_PUBSUB_TOPIC?: string;
  /** The container's log level (`debug`, `info`, `warn`, `error`); passed through when set. */
  LOG_LEVEL?: string;
  /** Local development only: set, the container gets `DEV_AUTH_BYPASS=1`, which it honours only on a
   * localhost `PUBLIC_ORIGIN` (so it is guarded twice). */
  DEV_ACCOUNT?: string;
}

/** A container's `X-Next-Wake: <seconds>`, clamped to a minute .. a day; null when absent or malformed. */
export function nextWakeSeconds(header: string | null): number | null {
  if (!header || !/^\d{1,9}$/.test(header)) return null;
  return Math.max(60, Math.min(86400, Number(header)));
}

export interface StampOptions {
  /** Set by the Worker's `/admin/*` path only: the MAC-verified account of the admin's session. */
  adminAccount?: string;
}

/**
 * The headers a request reaches a container with: every client-sent `x-account-id`,
 * `x-internal-secret` and `x-admin-*` removed, then this object's own values set.
 */
export function stampHeaders(source: Headers, accountId: string, internalSecret: string | undefined, opts: StampOptions = {}): Headers {
  const headers = new Headers(source);
  for (const k of [...headers.keys()]) {
    if (k === "x-account-id" || k === "x-internal-secret" || k.startsWith("x-admin-")) headers.delete(k);
  }
  headers.set("x-account-id", accountId);
  if (internalSecret) headers.set("x-internal-secret", internalSecret);
  if (opts.adminAccount) headers.set("x-admin-account", opts.adminAccount);
  return headers;
}

/**
 * One container per account, keyed by the Durable Object's name (the account id, or `_operator`).
 * The container never sees a public port: every request reaches it through this object, which
 * stamps the account id and that account's derived internal secret on the way in.
 */
export class MailContainer extends Container<ContainerEnv> {
  defaultPort = 8080;
  sleepAfter = "5m";

  /** This account's derived internal secret (see `deriveSecret` in tokens.ts); never the master. */
  private internalSecret: string | undefined;

  constructor(ctx: DurableObjectState<{}>, env: ContainerEnv) {
    super(ctx, env);
    const name = ctx.id.name ?? "unknown";
    this.envVars = MailContainer.varsFor(name, env);
    // WebCrypto is async; blockConcurrencyWhile holds every fetch, RPC call and alarm (so every
    // start path, all of which read envVars) until the derived secrets are in place.
    const { INTERNAL_SECRET: internal, SESSION_SECRET: session } = env;
    if (internal || session) {
      void ctx.blockConcurrencyWhile(async () => {
        const derived: Record<string, string> = {};
        if (internal) derived.INTERNAL_SECRET = this.internalSecret = await deriveSecret(internal, name);
        if (session) derived.SESSION_SECRET = await deriveSecret(session, name);
        this.envVars = { ...this.envVars, ...derived };
      });
    }
  }

  /** Everything but the two secrets, which the constructor sets to their per-account derivations. */
  static varsFor(name: string, e: ContainerEnv): Record<string, string> {
    const vars: Record<string, string> = {
      ACCOUNT_ID: name,
      ROLE: name === "_operator" ? "operator" : "account",
      PORT: "8080",
      DATA_DIR: "/data",
      WORKER_ORIGIN: e.PUBLIC_ORIGIN,
      PUBLIC_ORIGIN: e.PUBLIC_ORIGIN,
      SIGNUP_MODE: e.SIGNUP_MODE,
      RETENTION_DAYS_DEFAULT: e.RETENTION_DAYS_DEFAULT,
    };
    for (const k of ["MAIL_FROM", "R2_BUCKET", "R2_ENDPOINT", "R2_ACCESS_KEY_ID",
      "R2_SECRET_ACCESS_KEY", "AI_BASE_URL", "AI_API_KEY", "AI_MODEL", "CF_API_TOKEN", "CF_ACCOUNT_ID", "ADMIN_EMAILS",
      "GOOGLE_PUBSUB_TOPIC", "LOG_LEVEL"] as const) {
      const v = e[k];
      if (v) vars[k] = v;
    }
    if (e.DEV_ACCOUNT) vars.DEV_AUTH_BYPASS = "1";
    return vars;
  }

  override onStart() {
    console.log(JSON.stringify({ ev: "container.start", account: this.ctx.id.name }));
  }

  override onStop() {
    console.log(JSON.stringify({ ev: "container.stop", account: this.ctx.id.name }));
  }

  override onError(error: unknown) {
    console.log(JSON.stringify({ ev: "container.error", account: this.ctx.id.name, error: String(error) }));
  }

  /** Proxy a request in, stamping the headers only the Durable Object (and the admin path) may set. */
  async proxy(request: Request, accountId: string, opts: StampOptions = {}): Promise<Response> {
    const headers = stampHeaders(request.headers, accountId, this.internalSecret, opts);
    const forwarded = new Request(request, { headers });
    const response = await this.containerFetch(forwarded);
    // Every response may name the next wake; keep exactly one pending, the latest one asked for
    // (each request would otherwise add another schedule row).
    const seconds = nextWakeSeconds(response.headers.get("x-next-wake"));
    if (seconds !== null) {
      this.deleteSchedules("wake");
      await this.schedule(seconds, "wake");
    }
    return response;
  }

  /** A scheduled wake: ask the container to run its due jobs. */
  async wake() {
    try {
      await this.proxy(new Request("http://container/internal/tick", { method: "POST" }), this.ctx.id.name ?? "");
    } catch (e) {
      console.log(JSON.stringify({ ev: "container.wake.failed", error: String(e) }));
    }
  }
}
