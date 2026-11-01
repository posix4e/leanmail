/**
 * The D1 projection tables the Worker routes on: which account a domain belongs to, what each
 * address does with its mail, and which accounts are active. Containers write them through
 * /internal/{routes,accounts}; `email()` and the cron read them without waking anything.
 */

export const MAILS = ["vault", "vault+forward", "forward", "drop"] as const;
export type Mail = (typeof MAILS)[number];

export interface Route {
  address: string;
  account_id: string;
  mail: Mail;
  forward_to: string[];
  can_send: boolean;
}

export interface RouteInput {
  address: string;
  mail: Mail;
  forward_to: string[];
  can_send: boolean;
}

export interface AccountInput {
  id: string;
  email: string;
  status: string;
  /** Absent: keep what D1 has. */
  has_gmail?: boolean;
}

export interface DomainInput {
  name: string;
  account_id: string;
}

export type Match = "exact" | "stripped" | "domain";

interface RouteRow {
  address: string;
  account_id: string;
  mail: Mail;
  forward_to: string;
  can_send: number;
}

/** An address as the tables key it: trimmed, angle brackets gone, lower-cased. */
export function normAddress(addr: string): string {
  return addr.trim().replace(/^<|>$/g, "").trim().toLowerCase();
}

/** The domain of an address, lower-cased; "" when there is none. */
export function domainOf(addr: string): string {
  const a = normAddress(addr);
  const at = a.lastIndexOf("@");
  return at > 0 ? a.slice(at + 1) : "";
}

function parseList(json: string): string[] {
  try {
    const v = JSON.parse(json);
    return Array.isArray(v) ? v.filter((x) => typeof x === "string") : [];
  } catch {
    return [];
  }
}

function toRoute(r: RouteRow): Route {
  return { address: r.address, account_id: r.account_id, mail: r.mail, forward_to: parseList(r.forward_to), can_send: r.can_send === 1 };
}

/**
 * What to do with mail for `rcpt`: its own row, then the row without its `+tag` (as MailVault's
 * decide.ts did), then the domain's `*@domain` default, else null (the caller drops it). One query.
 */
export async function lookupRoute(db: D1Database, rcpt: string): Promise<(Route & { match: Match }) | null> {
  const addr = normAddress(rcpt);
  const at = addr.lastIndexOf("@");
  if (at <= 0 || at === addr.length - 1) return null;
  const local = addr.slice(0, at);
  const domain = addr.slice(at + 1);
  const plus = local.indexOf("+");
  const stripped = plus > 0 ? `${local.slice(0, plus)}@${domain}` : addr;
  const wildcard = `*@${domain}`;
  const { results } = await db
    .prepare("SELECT address, account_id, mail, forward_to, can_send FROM routes WHERE address IN (?1, ?2, ?3)")
    .bind(addr, stripped, wildcard)
    .all<RouteRow>();
  const find = (a: string) => results.find((r) => r.address === a);
  const exact = find(addr);
  if (exact) return { ...toRoute(exact), match: "exact" };
  const s = stripped !== addr ? find(stripped) : undefined;
  if (s) return { ...toRoute(s), match: "stripped" };
  const d = find(wildcard);
  if (d) return { ...toRoute(d), match: "domain" };
  return null;
}

/**
 * Upsert an account's routes in one batch. With `replace`, the account's other rows go too, so the
 * table is exactly what the container projected.
 */
export async function upsertRoutes(db: D1Database, accountId: string, routes: RouteInput[], opts: { replace?: boolean; now?: number } = {}): Promise<void> {
  const now = opts.now ?? Date.now();
  const stmts: D1PreparedStatement[] = [];
  if (opts.replace) {
    const keep = routes.map((r) => normAddress(r.address));
    stmts.push(
      db.prepare("DELETE FROM routes WHERE account_id = ?1 AND address NOT IN (SELECT value FROM json_each(?2))").bind(accountId, JSON.stringify(keep)),
    );
  }
  for (const r of routes) {
    stmts.push(
      db
        .prepare(
          `INSERT INTO routes (address, account_id, mail, forward_to, can_send, updated_at) VALUES (?1, ?2, ?3, ?4, ?5, ?6)
           ON CONFLICT(address) DO UPDATE SET account_id = ?2, mail = ?3, forward_to = ?4, can_send = ?5, updated_at = ?6`,
        )
        .bind(normAddress(r.address), accountId, r.mail, JSON.stringify(r.forward_to.map(normAddress)), r.can_send ? 1 : 0, now),
    );
  }
  if (stmts.length) await db.batch(stmts);
}

/**
 * Upsert accounts and domain grants from the operator, in one batch. An account whose
 * `has_gmail` is absent keeps its stored value. For each id in `replaceDomainsFor`, the domains
 * listed for it are all of its grants: its other `domains` rows go (none listed: all go).
 */
export async function upsertAccounts(
  db: D1Database,
  accounts: AccountInput[],
  domains: DomainInput[] = [],
  opts: { replaceDomainsFor?: string[]; now?: number } = {},
): Promise<void> {
  const now = opts.now ?? Date.now();
  const stmts: D1PreparedStatement[] = [];
  for (const a of accounts) {
    stmts.push(
      db
        .prepare(
          `INSERT INTO accounts (id, email, status, has_gmail, updated_at) VALUES (?1, ?2, ?3, COALESCE(?4, 0), ?5)
           ON CONFLICT(id) DO UPDATE SET email = ?2, status = ?3, has_gmail = COALESCE(?4, has_gmail), updated_at = ?5`,
        )
        .bind(a.id, normAddress(a.email), a.status, a.has_gmail === undefined ? null : a.has_gmail ? 1 : 0, now),
    );
  }
  const names = domains.map((d) => ({ name: d.name.trim().toLowerCase(), account_id: d.account_id }));
  for (const acct of new Set(opts.replaceDomainsFor ?? [])) {
    const keep = names.filter((d) => d.account_id === acct).map((d) => d.name);
    stmts.push(
      db.prepare("DELETE FROM domains WHERE account_id = ?1 AND name NOT IN (SELECT value FROM json_each(?2))").bind(acct, JSON.stringify(keep)),
    );
  }
  for (const d of names) {
    stmts.push(db.prepare("INSERT INTO domains (name, account_id) VALUES (?1, ?2) ON CONFLICT(name) DO UPDATE SET account_id = ?2").bind(d.name, d.account_id));
  }
  if (stmts.length) await db.batch(stmts);
}

/** The account an email belongs to, by its normalised address: id and status, or null. */
export async function accountByEmail(db: D1Database, email: string): Promise<{ id: string; status: string } | null> {
  return db.prepare("SELECT id, status FROM accounts WHERE email = ?1").bind(normAddress(email)).first<{ id: string; status: string }>();
}

/** Ids of every account whose status is `active`, in id order. */
export async function listActiveAccounts(db: D1Database): Promise<string[]> {
  const { results } = await db.prepare("SELECT id FROM accounts WHERE status = 'active' ORDER BY id").all<{ id: string }>();
  return results.map((r) => r.id);
}

/** The subset of `names` that are domains some account receives on (lower-case). */
export async function ownDomains(db: D1Database, names: string[]): Promise<Set<string>> {
  if (names.length === 0) return new Set();
  const { results } = await db
    .prepare("SELECT name FROM domains WHERE name IN (SELECT value FROM json_each(?1))")
    .bind(JSON.stringify(names.map((n) => n.toLowerCase())))
    .all<{ name: string }>();
  return new Set(results.map((r) => r.name));
}

/** Of `names`, the domains that are not granted to `accountId`. */
export async function domainsNotOwnedBy(db: D1Database, accountId: string, names: string[]): Promise<string[]> {
  const uniq = [...new Set(names.map((n) => n.toLowerCase()))];
  if (uniq.length === 0) return [];
  const { results } = await db
    .prepare("SELECT name FROM domains WHERE account_id = ?1 AND name IN (SELECT value FROM json_each(?2))")
    .bind(accountId, JSON.stringify(uniq))
    .all<{ name: string }>();
  const owned = new Set(results.map((r) => r.name));
  return uniq.filter((n) => !owned.has(n));
}

/** The domains granted to `accountId`, in name order. */
export async function domainsOf(db: D1Database, accountId: string): Promise<string[]> {
  const { results } = await db.prepare("SELECT name FROM domains WHERE account_id = ?1 ORDER BY name").bind(accountId).all<{ name: string }>();
  return results.map((r) => r.name);
}
