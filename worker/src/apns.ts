import { b64uEncode } from "./tokens";

/**
 * APNs, ported from MailVault's src/push.ts. NOT WIRED YET: `/internal/push` is a stub until M7.
 *
 * Authentication is a provider token: an ES256 JWT over the team and key ids, signed with the .p8
 * Apple issued, good for an hour and reused for fifty minutes. The payload is `{id, from, subject}`
 * now that the server reads plaintext; no Notification Service Extension is needed.
 */

export const PROVIDER_TOKEN_TTL_MS = 50 * 60 * 1000;
export const APNS_HOSTS = { sandbox: "https://api.sandbox.push.apple.com", prod: "https://api.push.apple.com" } as const;
export type ApnsEnv = keyof typeof APNS_HOSTS;

export interface ApnsConfig {
  teamId: string;
  keyId: string;
  /** PEM-armoured PKCS#8 P-256 key (the .p8 file). */
  keyP8: string;
  topic: string;
}

export interface Device {
  token: string;
  env: ApnsEnv;
}

const enc = new TextEncoder();
let providerCache: { at: number; forKey: string; jwt: string } | null = null;

/** The .p8 Apple hands out is a PEM-armoured PKCS#8 P-256 key; WebCrypto wants the DER inside. */
export async function importP8(pem: string): Promise<CryptoKey> {
  const der = Uint8Array.from(atob(pem.replace(/-----[^-]+-----/g, "").replace(/\s+/g, "")), (c) => c.charCodeAt(0));
  return crypto.subtle.importKey("pkcs8", der, { name: "ECDSA", namedCurve: "P-256" }, false, ["sign"]);
}

/** A signed provider token, cached per key for PROVIDER_TOKEN_TTL_MS. */
export async function apnsProviderToken(cfg: ApnsConfig, now: number = Date.now()): Promise<string> {
  const digest = new Uint8Array(await crypto.subtle.digest("SHA-256", enc.encode(cfg.keyP8)));
  const forKey = `${cfg.teamId}/${cfg.keyId}/${b64uEncode(digest)}`;
  if (providerCache && providerCache.forKey === forKey && now - providerCache.at < PROVIDER_TOKEN_TTL_MS) return providerCache.jwt;
  const key = await importP8(cfg.keyP8);
  const head = b64uEncode(enc.encode(JSON.stringify({ alg: "ES256", kid: cfg.keyId })));
  const body = b64uEncode(enc.encode(JSON.stringify({ iss: cfg.teamId, iat: Math.floor(now / 1000) })));
  // WebCrypto's ECDSA signature is r||s, which is exactly what JWS ES256 wants.
  const sig = new Uint8Array(await crypto.subtle.sign({ name: "ECDSA", hash: "SHA-256" }, key, enc.encode(`${head}.${body}`)));
  const jwt = `${head}.${body}.${b64uEncode(sig)}`;
  providerCache = { at: now, forKey, jwt };
  return jwt;
}

export function pushPayload(msg: { id: string; from: string; subject: string }): Record<string, unknown> {
  return { aps: { alert: { title: msg.from, body: msg.subject }, "content-available": 1, sound: "default" }, id: msg.id };
}

/**
 * One payload to each device. A token Apple says is dead is returned in `dead` for the container to
 * forget; a refused provider token clears the cache so the next push signs afresh.
 */
export async function sendApns(cfg: ApnsConfig, devices: Device[], payload: Record<string, unknown>, now: number = Date.now()): Promise<{ sent: number; dead: string[]; failed: number }> {
  const out = { sent: 0, dead: [] as string[], failed: 0 };
  const body = JSON.stringify(payload);
  for (const d of devices) {
    try {
      const res = await fetch(`${APNS_HOSTS[d.env]}/3/device/${d.token}`, {
        method: "POST",
        headers: {
          authorization: `bearer ${await apnsProviderToken(cfg, now)}`,
          "apns-topic": cfg.topic,
          "apns-push-type": "alert",
          "apns-priority": "10",
          "content-type": "application/json",
        },
        body,
      });
      if (res.ok) {
        out.sent++;
        continue;
      }
      const reason = ((await res.json().catch(() => ({}))) as { reason?: string }).reason ?? "";
      if (res.status === 410 || reason === "BadDeviceToken" || reason === "Unregistered" || reason === "DeviceTokenNotForTopic") {
        out.dead.push(d.token);
      } else {
        if (reason === "InvalidProviderToken" || reason === "ExpiredProviderToken") providerCache = null;
        out.failed++;
      }
    } catch {
      out.failed++;
    }
  }
  return out;
}
