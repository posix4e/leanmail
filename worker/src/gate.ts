import type { Env } from "./env";
import { rateLimited } from "./auth";
import { sha256Hex } from "./rate";
import { accountFromRequest, b64uDecode, b64uEncode, bytesEqual, deriveSecret, secretEqual } from "./tokens";

/**
 * The passphrase gate: with GATE_PASSPHRASE set, a request that is not signed in (no verifying
 * `lm_session` cookie or bearer token) gets a bare 401 with `WWW-Authenticate: Basic` until it
 * shows the passphrase, so a stranger sees neither the sign-in nor the sign-up page. The browser
 * asks once (any user name, the passphrase as password); the Worker then sets `lm_gate`
 * (`v1.<exp>.<mac>`, mac = base64url(HMAC-SHA256(deriveSecret(master SESSION_SECRET, "gate:cookie"),
 * "v1.<exp>." + sha256 hex of the passphrase))), so changing the passphrase voids every gate cookie,
 * and the label fails ACCOUNT_RE, so no container holds the key. Wrong passphrases are rate-limited
 * like sign-in (bucket `gate`). Unset, the gate is off and nothing changes.
 *
 * A Basic `Authorization` header is dropped before the request goes on: the browser keeps sending
 * it, and a container reads any `Authorization` as a bearer token.
 */

export const GATE_COOKIE = "lm_gate";
const GATE_TTL_S = 90 * 24 * 3600;
const REALM = 'Basic realm="LeanMail", charset="UTF-8"';

function unauthorized(): Response {
  return new Response("401 Unauthorized\n", {
    status: 401,
    headers: { "content-type": "text/plain; charset=utf-8", "www-authenticate": REALM, "cache-control": "no-store" },
  });
}

async function gateMac(env: Env, signed: string): Promise<Uint8Array> {
  const key = await crypto.subtle.importKey(
    "raw",
    new TextEncoder().encode(await deriveSecret(env.SESSION_SECRET!, "gate:cookie")),
    { name: "HMAC", hash: "SHA-256" },
    false,
    ["sign"],
  );
  const data = `${signed}.${await sha256Hex(env.GATE_PASSPHRASE!)}`;
  return new Uint8Array(await crypto.subtle.sign("HMAC", key, new TextEncoder().encode(data)));
}

/** A fresh `lm_gate` value, good for 90 days. */
export async function gateToken(env: Env, now: number = Date.now()): Promise<string> {
  const signed = `v1.${Math.floor(now / 1000) + GATE_TTL_S}`;
  return `${signed}.${b64uEncode(await gateMac(env, signed))}`;
}

export async function gateTokenOk(env: Env, token: string, now: number = Date.now()): Promise<boolean> {
  const m = /^(v1\.(\d{1,12}))\.([A-Za-z0-9_-]+)$/.exec(token);
  if (!m || Number(m[2]) * 1000 <= now) return false;
  let mac: Uint8Array;
  try {
    mac = b64uDecode(m[3]!);
  } catch {
    return false;
  }
  return bytesEqual(mac, await gateMac(env, m[1]!));
}

function cookie(request: Request, name: string): string | null {
  for (const part of (request.headers.get("cookie") ?? "").split(";")) {
    const eq = part.indexOf("=");
    if (eq >= 0 && part.slice(0, eq).trim() === name) return part.slice(eq + 1).trim();
  }
  return null;
}

/** The password of a Basic `Authorization` header (the user name is ignored), else null. */
function basicPassword(request: Request): string | null {
  const m = /^Basic\s+([A-Za-z0-9+/=]+)$/i.exec(request.headers.get("authorization") ?? "");
  if (!m) return null;
  try {
    const bytes = Uint8Array.from(atob(m[1]!), (c) => c.charCodeAt(0));
    const text = new TextDecoder().decode(bytes);
    const colon = text.indexOf(":");
    return colon < 0 ? null : text.slice(colon + 1);
  } catch {
    return null;
  }
}

export type GateResult =
  | { pass: true; request: Request; setCookie?: string }
  | { pass: false; response: Response };

/** Let `request` on, with any Basic header removed, or answer it: 401, or 429 once over the limit. */
export async function checkGate(request: Request, env: Env, now: number = Date.now()): Promise<GateResult> {
  const stripped = /^Basic\s/i.test(request.headers.get("authorization") ?? "")
    ? (() => {
        const r = new Request(request);
        r.headers.delete("authorization");
        return r;
      })()
    : request;
  if (!env.GATE_PASSPHRASE) return { pass: true, request: stripped };
  // no key to sign the gate cookie with: shut, not open
  if (!env.SESSION_SECRET) return { pass: false, response: unauthorized() };
  if (await accountFromRequest(request, env.SESSION_SECRET)) return { pass: true, request: stripped };
  const held = cookie(request, GATE_COOKIE);
  if (held && (await gateTokenOk(env, held, now))) return { pass: true, request: stripped };
  const given = basicPassword(request);
  if (given === null) return { pass: false, response: unauthorized() };
  if (await rateLimited(env.DB, "gate", request, now)) {
    return { pass: false, response: new Response("429 Too Many Requests\n", { status: 429, headers: { "retry-after": "3600" } }) };
  }
  if (!(await secretEqual(given, env.GATE_PASSPHRASE))) return { pass: false, response: unauthorized() };
  const setCookie = `${GATE_COOKIE}=${await gateToken(env, now)}; Max-Age=${GATE_TTL_S}; Path=/; HttpOnly; Secure; SameSite=Lax`;
  return { pass: true, request: stripped, setCookie };
}

/** `res` with the gate cookie added (a fresh Response: a proxied one's headers may be immutable). */
export function withCookie(res: Response, setCookie: string | undefined): Response {
  if (!setCookie || res.status === 101) return res;
  const out = new Response(res.body, res);
  out.headers.append("set-cookie", setCookie);
  return out;
}
