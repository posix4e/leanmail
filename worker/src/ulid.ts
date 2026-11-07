/** Crockford base32, as ULID spells it. */
const ALPHABET = "0123456789ABCDEFGHJKMNPQRSTVWXYZ";

function encode(now: number, random: Uint8Array): string {
  let t = Math.floor(now);
  if (!Number.isFinite(t) || t < 0 || t > 0xffff_ffff_ffff) throw new RangeError("ulid: time out of range");
  let time = "";
  for (let i = 0; i < 10; i++) {
    time = ALPHABET[t % 32]! + time;
    t = Math.floor(t / 32);
  }
  // 80 random bits = 16 characters of 5 bits; use the low 5 bits of each of 16 bytes.
  let rand = "";
  for (const b of random.subarray(0, 16)) rand += ALPHABET[b & 31]!;
  return time + rand;
}

/**
 * A ULID: 48 bits of milliseconds then 80 random bits, 26 characters, lexically time-ordered.
 * Not monotonic within a millisecond; the spool does not need it to be.
 */
export function ulid(now: number = Date.now()): string {
  return encode(now, crypto.getRandomValues(new Uint8Array(16)));
}

/** A ULID whose random part comes from `name`, so naming the same thing twice gives the same id. */
export async function ulidFor(now: number, name: string): Promise<string> {
  return encode(now, new Uint8Array(await crypto.subtle.digest("SHA-256", new TextEncoder().encode(name))));
}

/** The millisecond timestamp a ULID carries. */
export function ulidTime(id: string): number {
  let t = 0;
  for (const c of id.slice(0, 10).toUpperCase()) {
    const v = ALPHABET.indexOf(c);
    if (v < 0) throw new RangeError("ulid: bad character");
    t = t * 32 + v;
  }
  return t;
}

export const ULID_RE = /^[0-9A-HJKMNP-TV-Z]{26}$/;
