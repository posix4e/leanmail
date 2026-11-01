/**
 * The MAC every credential the Worker routes on carries (plan, "Auth"):
 *   session cookie `lm_session`  v1.<acct>.<sid>.<exp>.<epoch>.<mac>
 *   magic link                   v1.<acct>.<rand>.<exp>.<mac>
 *   API token (Bearer)           lm1.<acct>.<rand>.<mac>
 * where mac = base64url(HMAC-SHA256(K_acct, everything before the last dot)), unpadded.
 *
 * Per-account keys. The Worker holds the master secrets; no container ever sees one. Each
 * account's container (the Durable Object named `<acct>`, `_operator` included) is started with
 *   SESSION_SECRET  = K_acct = deriveSecret(master SESSION_SECRET,  acct)
 *   INTERNAL_SECRET =          deriveSecret(master INTERNAL_SECRET, acct)
 * with deriveSecret(master, acct) = base64url(HMAC-SHA256(master, acct)), unpadded. The derived
 * value is a string and the container keys HMAC with its UTF-8 bytes, so the Worker does too.
 * The Worker verifies a token under the key of the account the token names, so a container can
 * mint only its own account's credentials, and the master itself never verifies anything.
 * The container re-verifies (account, expiry, revocation); the Worker only checks the MAC to pick
 * the container.
 */

export const SESSION_COOKIE = "lm_session";
export const ACCOUNT_RE = /^[A-Za-z0-9_-]{1,64}$/;

const enc = new TextEncoder();

export function b64uEncode(bytes: Uint8Array): string {
  let s = "";
  for (const b of bytes) s += String.fromCharCode(b);
  return btoa(s).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
}

export function b64uDecode(s: string): Uint8Array {
  if (!/^[A-Za-z0-9_-]*$/.test(s)) throw new Error("not base64url");
  const pad = s.length % 4 === 0 ? "" : "=".repeat(4 - (s.length % 4));
  const bin = atob(s.replace(/-/g, "+").replace(/_/g, "/") + pad);
  return Uint8Array.from(bin, (c) => c.charCodeAt(0));
}

/** Constant-time equality for byte strings (length is not secret). */
export function bytesEqual(a: Uint8Array, b: Uint8Array): boolean {
  if (a.byteLength !== b.byteLength) return false;
  let d = 0;
  for (let i = 0; i < a.byteLength; i++) d |= a[i]! ^ b[i]!;
  return d === 0;
}

/** Constant-time string equality: compares SHA-256 digests, so neither length nor content leaks. */
export async function secretEqual(a: string, b: string): Promise<boolean> {
  const [x, y] = await Promise.all([crypto.subtle.digest("SHA-256", enc.encode(a)), crypto.subtle.digest("SHA-256", enc.encode(b))]);
  return bytesEqual(new Uint8Array(x), new Uint8Array(y));
}

async function hmac(secret: string, data: string): Promise<Uint8Array> {
  const key = await crypto.subtle.importKey("raw", enc.encode(secret), { name: "HMAC", hash: "SHA-256" }, false, ["sign"]);
  return new Uint8Array(await crypto.subtle.sign("HMAC", key, enc.encode(data)));
}

/** base64url(HMAC-SHA256(master, acct)): one account's share of a master secret (see the header). */
export async function deriveSecret(master: string, acct: string): Promise<string> {
  return b64uEncode(await hmac(master, acct));
}

/** The internal secret one account's container holds; it authenticates as that account only. */
export const deriveInternalSecret = deriveSecret;

/** `<prefix>.<acct>.<body>.<mac>` under an explicit key: what a container holding `key` mints. */
export async function signMacWithKey(prefix: string, acct: string, body: string, key: string): Promise<string> {
  const signed = `${prefix}.${acct}.${body}`;
  return `${signed}.${b64uEncode(await hmac(key, signed))}`;
}

/** Sign as `acct`'s container would, deriving its key from the master SESSION_SECRET. */
export async function signMac(prefix: string, acct: string, body: string, master: string): Promise<string> {
  return signMacWithKey(prefix, acct, body, await deriveSecret(master, acct));
}

export interface Verified {
  prefix: string;
  acct: string;
  /** Everything between the account and the MAC, dots included. */
  body: string;
}

/**
 * Verify a `<prefix>.<acct>.<body...>.<mac>` token under the key derived from `master` for the
 * account it names. `prefixes` limits which kinds are accepted (default `v1` and `lm1`). Null on
 * any malformation or a wrong MAC; never throws.
 */
export async function verifyMac(token: string, master: string | undefined, prefixes: readonly string[] = ["v1", "lm1"]): Promise<Verified | null> {
  if (!master || typeof token !== "string" || token.length > 4096) return null;
  const parts = token.split(".");
  if (parts.length < 4) return null;
  const [prefix, acct] = parts as [string, string];
  if (!prefixes.includes(prefix) || !ACCOUNT_RE.test(acct)) return null;
  const last = token.lastIndexOf(".");
  const body = parts.slice(2, -1).join(".");
  if (!body) return null;
  let mac: Uint8Array;
  try {
    mac = b64uDecode(token.slice(last + 1));
  } catch {
    return null;
  }
  const expected = await hmac(await deriveSecret(master, acct), token.slice(0, last));
  if (!bytesEqual(mac, expected)) return null;
  return { prefix, acct, body };
}

function cookieValue(header: string | null, name: string): string | null {
  if (!header) return null;
  for (const part of header.split(";")) {
    const eq = part.indexOf("=");
    if (eq < 0) continue;
    if (part.slice(0, eq).trim() === name) return part.slice(eq + 1).trim();
  }
  return null;
}

/**
 * The account a request's credential names: the `lm_session` cookie (`v1.`), else an
 * `Authorization: Bearer lm1.` token. Null when neither verifies.
 */
export async function accountFromRequest(request: Request, secret: string | undefined, opts: { bearer?: boolean } = {}): Promise<string | null> {
  const cookie = cookieValue(request.headers.get("cookie"), SESSION_COOKIE);
  if (cookie) {
    const v = await verifyMac(cookie, secret, ["v1"]);
    if (v) return v.acct;
  }
  if (opts.bearer === false) return null;
  const m = /^Bearer\s+(\S+)$/i.exec(request.headers.get("authorization") ?? "");
  if (m) {
    const v = await verifyMac(m[1]!, secret, ["lm1"]);
    if (v) return v.acct;
  }
  return null;
}
