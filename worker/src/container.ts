import { Container } from "@cloudflare/containers";
import { canonicalPath, firstSegment } from "./paths";
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
  /** The Cloudflare API token routing plans are applied with (optional). Only the `_operator`
   * container receives it; accounts reach it through the Worker's /internal/routing/{plan,apply}. */
  CF_API_TOKEN?: string;
  /** The Cloudflare account that token belongs to (destination addresses); `_operator` only. */
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
  /**
   * The Worker itself made this request (callContainer, the scheduled wake, provisioning, the
   * health check), not a client: it goes in without `x-lm-public`, so the container's own
   * `/internal/*` routes accept it. Everything else is marked public.
   */
  internal?: boolean;
  /** With `internal`, to the operator's /internal/routing/*: the account the plan is for. */
  routingAccount?: string;
}

/** Headers only the Durable Object sets; whatever a client sent under these names is dropped. */
const STAMPED = new Set(["x-account-id", "x-internal-secret", "x-lm-public", "x-routing-account"]);

/**
 * The headers a request reaches a container with: every client-sent `x-account-id`,
 * `x-internal-secret`, `x-lm-public`, `x-routing-account` and `x-admin-*` removed, then this
 * object's own values set. A request a client made carries `x-lm-public: 1`, and the container
 * refuses its `/internal/*` routes to such a request whatever secret it holds.
 */
export function stampHeaders(source: Headers, accountId: string, internalSecret: string | undefined, opts: StampOptions = {}): Headers {
  const headers = new Headers(source);
  for (const k of [...headers.keys()]) {
    if (STAMPED.has(k) || k.startsWith("x-admin-")) headers.delete(k);
  }
  headers.set("x-account-id", accountId);
  if (internalSecret) headers.set("x-internal-secret", internalSecret);
  if (!opts.internal) headers.set("x-lm-public", "1");
  if (opts.internal && opts.routingAccount) headers.set("x-routing-account", opts.routingAccount);
  if (opts.adminAccount) headers.set("x-admin-account", opts.adminAccount);
  return headers;
}

/**
 * What `containerFetch` answers when the container's port drops the connection, typically right
 * after a first start: it returns a 500 with this text rather than throwing.
 */
export const TRANSIENT_PORT_ERROR = /connection closed unexpectedly|suddenly disconnected/i;

/** The largest client body proxy() keeps a copy of for the retry (its own calls are always kept). */
export const RETRY_BODY_MAX = 1024 * 1024;

/** Whether a request may be retried: no body, or a declared length the copy can afford. */
export function retryable(request: Request, internal: boolean): boolean {
  if (internal || request.body === null) return true;
  const len = request.headers.get("content-length");
  return len !== null && /^\d{1,12}$/.test(len) && Number(len) <= RETRY_BODY_MAX;
}

/**
 * `send(request)`, once more (after `delayMs`) when the answer is a 500 whose text is a
 * TRANSIENT_PORT_ERROR, or `send` throws one. The retry goes out on a clone made before the first
 * attempt, so a body is sent whole both times. Any other 500 is returned with its body intact.
 * With `retry` false (a large body), it is a plain `send`.
 */
export async function sendRetryingOnce(send: (r: Request) => Promise<Response>, request: Request, retry = true, delayMs = 250): Promise<Response> {
  if (!retry) return send(request);
  const spare = request.clone();
  const again = async () => {
    await new Promise((r) => setTimeout(r, delayMs));
    return send(spare);
  };
  let res: Response;
  try {
    res = await send(request);
  } catch (e) {
    if (TRANSIENT_PORT_ERROR.test(String(e))) return again();
    await spare.body?.cancel();
    throw e;
  }
  if (res.status !== 500) {
    await spare.body?.cancel();
    return res;
  }
  const text = await res.text();
  if (TRANSIENT_PORT_ERROR.test(text)) {
    console.log(JSON.stringify({ ev: "container.retry", error: text.slice(0, 200) }));
    return again();
  }
  await spare.body?.cancel();
  return new Response(text, res);
}

/** Docker's name for the host: inside a container, localhost is the container itself. */
export const DOCKER_HOST = "host.docker.internal";
const LOCAL_ORIGIN_HOSTS = new Set(["localhost", "127.0.0.1", "[::1]"]);

/**
 * The origin a container calls the Worker on: PUBLIC_ORIGIN, except under `wrangler dev` (a
 * localhost PUBLIC_ORIGIN), where it is `http://host.docker.internal:<port>`.
 */
export function workerOriginFor(publicOrigin: string): string {
  let u: URL;
  try {
    u = new URL(publicOrigin);
  } catch {
    return publicOrigin;
  }
  if (!LOCAL_ORIGIN_HOSTS.has(u.hostname)) return publicOrigin;
  return `http://${DOCKER_HOST}:${u.port || "8787"}`;
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
      WORKER_ORIGIN: workerOriginFor(e.PUBLIC_ORIGIN),
      PUBLIC_ORIGIN: e.PUBLIC_ORIGIN,
      SIGNUP_MODE: e.SIGNUP_MODE,
      RETENTION_DAYS_DEFAULT: e.RETENTION_DAYS_DEFAULT,
    };
    for (const k of ["MAIL_FROM", "R2_BUCKET", "R2_ENDPOINT", "R2_ACCESS_KEY_ID",
      "R2_SECRET_ACCESS_KEY", "AI_BASE_URL", "AI_API_KEY", "AI_MODEL", "ADMIN_EMAILS",
      "GOOGLE_PUBSUB_TOPIC", "LOG_LEVEL"] as const) {
      const v = e[k];
      if (v) vars[k] = v;
    }
    // The Cloudflare token can change any zone's routing: the operator alone holds it, and checks
    // an account's domain grants before planning or applying for it (/internal/routing/*).
    if (name === "_operator") {
      for (const k of ["CF_API_TOKEN", "CF_ACCOUNT_ID"] as const) {
        const v = e[k];
        if (v) vars[k] = v;
      }
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

  /**
   * Proxy a request in, stamping the headers only the Durable Object (and the admin path) may set.
   * A public request whose canonical path is `/internal/*` never reaches the container (the
   * Worker's fetch() already refuses it; this is the second lock). A container that is not yet
   * running is started and its port waited for first, and a dropped first connection is retried
   * once (sendRetryingOnce).
   */
  async proxy(request: Request, accountId: string, opts: StampOptions = {}): Promise<Response> {
    if (!opts.internal) {
      const canon = canonicalPath(new URL(request.url).pathname);
      if (canon === null || firstSegment(canon) === "internal") {
        return new Response(JSON.stringify({ error: "not_found" }), { status: 404, headers: { "content-type": "application/json" } });
      }
    }
    const headers = stampHeaders(request.headers, accountId, this.internalSecret, opts);
    const forwarded = new Request(request, { headers });
    await this.ensureStarted();
    const response = await sendRetryingOnce((r) => this.containerFetch(r), forwarded, retryable(forwarded, !!opts.internal));
    // Every response may name the next wake; keep exactly one pending, the latest one asked for
    // (each request would otherwise add another schedule row).
    const seconds = nextWakeSeconds(response.headers.get("x-next-wake"));
    if (seconds !== null) {
      this.deleteSchedules("wake");
      await this.schedule(seconds, "wake");
    }
    return response;
  }

  /** Start the container and wait for its port when it is not already up and healthy. */
  private async ensureStarted(): Promise<void> {
    try {
      const state = await this.getState();
      if (state.status === "healthy") return;
      await this.startAndWaitForPorts(this.defaultPort);
    } catch (e) {
      // containerFetch tries again and turns a failure into a 5xx answer.
      console.log(JSON.stringify({ ev: "container.start_wait_failed", account: this.ctx.id.name, error: String(e) }));
    }
  }

  /** A scheduled wake: ask the container to run its due jobs. */
  async wake() {
    try {
      const res = await this.proxy(new Request("http://container/internal/tick", { method: "POST" }), this.ctx.id.name ?? "", { internal: true });
      await res.body?.cancel();
    } catch (e) {
      console.log(JSON.stringify({ ev: "container.wake.failed", error: String(e) }));
    }
  }
}
