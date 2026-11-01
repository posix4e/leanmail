/**
 * Path canonicalisation for the public front door. The container's router percent-decodes each
 * segment and drops empty ones (lean/Server/Main.lean `segments`), so `/%69nternal/tick` and
 * `//internal/tick` would reach its `/internal/*` routes. The Worker therefore routes on the same
 * canonical form the container will see, refuses whatever cannot be put in that form, and sends
 * the container the canonical path.
 */

/** The first segment decides routing (internal, auth, admin, hooks, static...): unreserved only. */
const FIRST_SEGMENT_RE = /^[A-Za-z0-9._~-]+$/;
/**
 * Any later segment, after one decode: RFC 3986 `pchar` without `%`, so the addresses and
 * `*@domain` wildcards in `/settings/routing/addresses/:address` still pass. A decoded `/`, `\`,
 * `%`, `?`, `#`, space, control or non-ASCII character is refused.
 */
const SEGMENT_RE = /^[A-Za-z0-9._~!$&'()*+,;=:@-]+$/;

/** `decodeURIComponent` that never throws: null on a malformed escape. */
export function safeDecode(s: string): string | null {
  try {
    return decodeURIComponent(s);
  } catch {
    return null;
  }
}

/**
 * The canonical form of a URL path: each segment percent-decoded once, empty and `.` segments
 * dropped, `..` resolved. Null when a segment cannot be decoded or holds a character outside the
 * sets above (the caller answers 404).
 */
export function canonicalPath(raw: string): string | null {
  const out: string[] = [];
  for (const part of raw.split("/")) {
    const seg = safeDecode(part);
    if (seg === null) return null;
    if (seg === "" || seg === ".") continue;
    if (seg === "..") {
      out.pop();
      continue;
    }
    if (!SEGMENT_RE.test(seg)) return null;
    out.push(seg);
  }
  if (out.length && !FIRST_SEGMENT_RE.test(out[0]!)) return null;
  return "/" + out.join("/");
}

/** The canonical path's first segment, lower-cased ("" for `/`). */
export function firstSegment(canonical: string): string {
  return (canonical.split("/")[1] ?? "").toLowerCase();
}
