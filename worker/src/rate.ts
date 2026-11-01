/**
 * One-hour counting windows in D1 `auth_rate` (pruned by the daily cron): every isolate shares the
 * count, which an in-memory map would not.
 */

export const HOUR_MS = 3_600_000;

/** The statement that counts one hit on `key` and returns the window's count. */
export function hitStatement(db: D1Database, key: string, now: number): D1PreparedStatement {
  return db
    .prepare(
      `INSERT INTO auth_rate (key, window_start, count) VALUES (?1, ?2, 1)
       ON CONFLICT(key) DO UPDATE SET
         count = CASE WHEN ?2 - window_start >= ${HOUR_MS} THEN 1 ELSE count + 1 END,
         window_start = CASE WHEN ?2 - window_start >= ${HOUR_MS} THEN ?2 ELSE window_start END
       RETURNING count`,
    )
    .bind(key, now);
}

/** Count one hit on each key, in one batch; true when any window now holds more than `limit`. */
export async function overLimit(db: D1Database, keys: string[], limit: number, now: number = Date.now()): Promise<boolean> {
  if (keys.length === 0) return false;
  const results = await db.batch<{ count: number }>(keys.map((k) => hitStatement(db, k, now)));
  return results.some((r) => (r.results[0]?.count ?? 0) > limit);
}

export async function sha256Hex(s: string): Promise<string> {
  const d = new Uint8Array(await crypto.subtle.digest("SHA-256", new TextEncoder().encode(s)));
  return [...d].map((b) => b.toString(16).padStart(2, "0")).join("");
}

/**
 * The /64 network of an IPv6 address, as `xxxx:xxxx:xxxx:xxxx::/64` (lower-case, zero-padded);
 * null for IPv4, IPv4-mapped IPv6 and anything unparseable.
 */
export function ipv6Net64(ip: string): string | null {
  let s = ip.trim().toLowerCase();
  if (s.startsWith("[") && s.endsWith("]")) s = s.slice(1, -1);
  const zone = s.indexOf("%");
  if (zone >= 0) s = s.slice(0, zone);
  if (!s.includes(":") || !/^[0-9a-f:.]+$/.test(s)) return null;
  // An embedded dotted quad stands for the last two hextets.
  let tail: string[] = [];
  const lastColon = s.lastIndexOf(":");
  if (s.slice(lastColon + 1).includes(".")) {
    const quad = s.slice(lastColon + 1).split(".");
    if (quad.length !== 4 || !quad.every((q) => /^\d{1,3}$/.test(q) && Number(q) <= 255)) return null;
    const n = quad.map(Number);
    tail = [((n[0]! << 8) | n[1]!).toString(16), ((n[2]! << 8) | n[3]!).toString(16)];
    s = s.slice(0, lastColon + 1) + "0";
  }
  const halves = s.split("::");
  if (halves.length > 2) return null;
  const parse = (h: string) => (h === "" ? [] : h.split(":"));
  const head = parse(halves[0]!);
  const rest = halves.length === 2 ? parse(halves[1]!) : [];
  if (tail.length) {
    // The "0" placeholder stands for the dotted quad: swap it for its two hextets.
    const last = halves.length === 2 ? rest : head;
    last.pop();
    last.push(...tail);
  }
  let groups = head;
  if (halves.length === 2) {
    const fill = 8 - head.length - rest.length;
    if (fill < 1) return null;
    groups = [...head, ...Array<string>(fill).fill("0"), ...rest];
  }
  if (groups.length !== 8 || !groups.every((g) => /^[0-9a-f]{1,4}$/.test(g))) return null;
  const padded = groups.map((g) => g.padStart(4, "0"));
  // IPv4-mapped (::ffff:a.b.c.d): an IPv4 client, limited by its exact address only.
  if (padded.slice(0, 5).every((g) => g === "0000") && padded[5] === "ffff") return null;
  return `${padded.slice(0, 4).join(":")}::/64`;
}
