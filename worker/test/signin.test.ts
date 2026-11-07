import { createExecutionContext, runInDurableObject, waitOnExecutionContext } from "cloudflare:test";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { MAX_CODE_ATTEMPTS, logFor, type AccountLog } from "../src/accountlog";
import { handleAuth } from "../src/auth";
import type { Env } from "../src/env";
import { handleFetch } from "../src/index";
import { signinMessage } from "../src/internal";
import { ulid } from "../src/ulid";
import { captureLogs, env, resetDb } from "./helpers";

/** Signing up, getting a code by mail, and using the token it buys, with no container at all. */

const ORIGIN = "https://leanmail.example";
const ADMIN = "boss@x.example";

interface Mail {
  to: string;
  code: string;
}

let ip = 0;
/** The runtime's EmailMessage hides its body, so the code is read as it is drawn. */
let drawn = "";

beforeEach(() => {
  const real = crypto.getRandomValues.bind(crypto);
  vi.spyOn(crypto, "getRandomValues").mockImplementation(((a: Uint8Array | Uint32Array) => {
    const out = real(a);
    if (a instanceof Uint32Array) drawn = String(a[0]! % 1_000_000).padStart(6, "0");
    return out;
  }) as typeof crypto.getRandomValues);
});
afterEach(() => void vi.restoreAllMocks());

function world(vars: Partial<Env> = {}) {
  const mail: Mail[] = [];
  // A container that is asked for anything fails the test: the point is that none is needed.
  const MAIL = {
    idFromName: (n: string) => n,
    get: (name: string) => {
      throw new Error(`container ${name} was asked for`);
    },
  };
  const EMAIL = { send: async (m: { to: string }) => void mail.push({ to: m.to, code: drawn }) };
  const e = { ...env, PUBLIC_ORIGIN: ORIGIN, SIGNUP_MODE: "approve", ADMIN_EMAILS: ADMIN, MAIL_FROM: "no-reply@leanmail.example", DEV_ACCOUNT: undefined, MAIL, EMAIL, ...vars } as unknown as Env;
  const send = async (method: string, path: string, body?: unknown, token?: string) => {
    const ctx = createExecutionContext();
    const headers: Record<string, string> = { "content-type": "application/json", "cf-connecting-ip": `10.0.${ip >> 8}.${ip++ & 255}` };
    if (token) headers.authorization = `Bearer ${token}`;
    const r = await handleFetch(new Request(ORIGIN + path, { method, headers, body: body === undefined ? undefined : JSON.stringify(body) }), e, ctx);
    await waitOnExecutionContext(ctx);
    return r;
  };
  const codeFor = (to: string) => {
    const last = mail.filter((m) => m.to === to).at(-1);
    if (!last) throw new Error(`no mail for ${to}`);
    return last.code;
  };
  /** The whole sign-in: ask, read the mail, redeem. */
  const signIn = async (email: string, name = "phone") => {
    expect((await send("POST", "/auth/login", { email })).status).toBe(202);
    const r = await send("POST", "/auth/code", { email, code: codeFor(email), name });
    expect(r.status).toBe(200);
    return ((await r.json()) as { token: string }).token;
  };
  return { e, mail, send, codeFor, signIn };
}

async function account(id: string, email: string, status = "active") {
  await env.DB.prepare("INSERT INTO accounts (id, email, status, updated_at) VALUES (?1, ?2, ?3, 0)").bind(id, email, status).run();
}

const status = async (email: string) => (await env.DB.prepare("SELECT status FROM accounts WHERE email = ?1").bind(email).first<{ status: string }>())?.status ?? null;
const acctOf = async (email: string) => (await env.DB.prepare("SELECT id FROM accounts WHERE email = ?1").bind(email).first<{ id: string }>())!.id;

beforeEach(resetDb);

describe("a new person, with no container running", () => {
  it("SIGNUP_MODE=open: signs up, gets a welcome code, and reads /sync with the token it buys", async () => {
    const w = world({ SIGNUP_MODE: "open" });
    const { result, lines } = await captureLogs(() => w.send("POST", "/auth/signup", { email: " New@X.example ", note: "hi" }));
    expect(result.status).toBe(202);
    expect(await status("new@x.example")).toBe("active");
    expect(w.mail.map((m) => m.to)).toEqual(["new@x.example"]);
    expect(lines.find((l) => l.ev === "internal.email_signin")?.kind).toBe("welcome");
    expect(JSON.stringify(lines)).not.toMatch(/@|\b\d{6}\b/);
    const r = await w.send("POST", "/auth/code", { email: "new@x.example", code: w.codeFor("new@x.example"), name: "cli" });
    expect(r.status).toBe(200);
    expect(r.headers.get("cache-control")).toBe("no-store");
    const { token } = (await r.json()) as { token: string };
    expect(token).toMatch(new RegExp(`^lm1\\.${await acctOf("new@x.example")}\\.[A-Za-z0-9_-]{43}\\.`));
    const page = await w.send("GET", "/sync", undefined, token);
    expect(page.status).toBe(200);
    expect(await page.json()).toMatchObject({ entries: [], head: 0, more: false });
  });

  it("SIGNUP_MODE=approve: nothing until an admin approves; then a welcome code", async () => {
    const w = world();
    expect((await w.send("POST", "/auth/signup", { email: "wait@x.example", note: "please" })).status).toBe(202);
    expect(await status("wait@x.example")).toBe("pending");
    expect(w.mail).toEqual([]);
    // A pending account can neither ask for a code nor spend one.
    expect((await w.send("POST", "/auth/login", { email: "wait@x.example" })).status).toBe(202);
    expect(w.mail).toEqual([]);

    // The first admin is an ADMIN_EMAILS address, approved by signing up.
    expect((await w.send("POST", "/auth/signup", { email: "BOSS@x.example" })).status).toBe(202);
    expect(await status(ADMIN)).toBe("active");
    const boss = await w.signIn(ADMIN);
    const list = (await (await w.send("GET", "/admin/signups", undefined, boss)).json()) as { items: { id: string; email: string; note: string; created_at: number }[] };
    expect(list.items.map(({ email, note }) => ({ email, note }))).toEqual([{ email: "wait@x.example", note: "please" }]);

    const ok = await w.send("POST", `/admin/signups/${list.items[0]!.id}/approve`, {}, boss);
    expect(ok.status).toBe(200);
    expect(await status("wait@x.example")).toBe("active");
    expect(w.mail.at(-1)!.to).toBe("wait@x.example");
    expect((await w.send("POST", "/auth/code", { email: "wait@x.example", code: w.codeFor("wait@x.example") })).status).toBe(200);
    expect((await (await w.send("GET", "/admin/signups", undefined, boss)).json()) as object).toEqual({ items: [] });
  });

  it("a rejected sign-up leaves no account, and the address may ask again", async () => {
    const w = world();
    await w.send("POST", "/auth/signup", { email: ADMIN });
    const boss = await w.signIn(ADMIN);
    await w.send("POST", "/auth/signup", { email: "no@x.example" });
    const { items } = (await (await w.send("GET", "/admin/signups", undefined, boss)).json()) as { items: { id: string }[] };
    expect((await w.send("POST", `/admin/signups/${items[0]!.id}/reject`, {}, boss)).status).toBe(200);
    expect(await status("no@x.example")).toBeNull();
    // It is decided: neither way again, and nothing comes back.
    expect((await w.send("POST", `/admin/signups/${items[0]!.id}/approve`, {}, boss)).status).toBe(409);
    expect(await status("no@x.example")).toBeNull();
    await w.send("POST", "/auth/signup", { email: "no@x.example" });
    expect(await status("no@x.example")).toBe("pending");
  });

  it("approving does not bring back an account disabled meanwhile", async () => {
    const w = world();
    await w.send("POST", "/auth/signup", { email: ADMIN });
    const boss = await w.signIn(ADMIN);
    await w.send("POST", "/auth/signup", { email: "wait@x.example" });
    const { items } = (await (await w.send("GET", "/admin/signups", undefined, boss)).json()) as { items: { id: string }[] };
    await env.DB.prepare("UPDATE accounts SET status = 'disabled' WHERE email = 'wait@x.example'").run();
    const sent = w.mail.length;
    expect((await w.send("POST", `/admin/signups/${items[0]!.id}/approve`, {}, boss)).status).toBe(200);
    expect(await status("wait@x.example")).toBe("disabled");
    expect(w.mail).toHaveLength(sent);
  });

  it("only an admin's token decides: a plain account is refused, signed out is 401, an unknown id 404", async () => {
    const w = world();
    await w.send("POST", "/auth/signup", { email: ADMIN });
    const boss = await w.signIn(ADMIN);
    await account("plain1", "plain@x.example");
    const plain = await w.signIn("plain@x.example");
    await w.send("POST", "/auth/signup", { email: "wait@x.example" });
    const { items } = (await (await w.send("GET", "/admin/signups", undefined, boss)).json()) as { items: { id: string }[] };
    const id = items[0]!.id;
    expect((await w.send("GET", "/admin/signups", undefined, plain)).status).toBe(403);
    expect((await w.send("POST", `/admin/signups/${id}/approve`, {}, plain)).status).toBe(403);
    expect((await w.send("POST", `/admin/signups/${id}/approve`, {})).status).toBe(401);
    expect(await status("wait@x.example")).toBe("pending");
    expect((await w.send("POST", "/admin/signups/nope/approve", {}, boss)).status).toBe(404);
  });

  it("a sign-up never touches an account that exists; closed means 404 for everyone; a bad address is 400", async () => {
    const w = world({ SIGNUP_MODE: "open" });
    await account("gone1", "gone@x.example", "disabled");
    expect((await w.send("POST", "/auth/signup", { email: "gone@x.example" })).status).toBe(202);
    expect(await status("gone@x.example")).toBe("disabled");
    await account("here1", "here@x.example");
    expect((await w.send("POST", "/auth/signup", { email: "here@x.example" })).status).toBe(202);
    expect(await acctOf("here@x.example")).toBe("here1");
    expect(w.mail).toEqual([]);
    expect((await env.DB.prepare("SELECT count(*) AS n FROM signups").first<{ n: number }>())!.n).toBe(0);
    expect((await w.send("POST", "/auth/signup", { email: "not-an-email" })).status).toBe(400);
    expect((await world({ SIGNUP_MODE: "closed" }).send("POST", "/auth/signup", { email: ADMIN })).status).toBe(404);
    expect(await status(ADMIN)).toBeNull();
  });

  it("two sign-ups racing for one address make one account and one sign-up", async () => {
    const w = world();
    await Promise.all([1, 2, 3].map(() => w.send("POST", "/auth/signup", { email: "race@x.example" })));
    expect((await env.DB.prepare("SELECT count(*) AS n FROM accounts WHERE email = 'race@x.example'").first<{ n: number }>())!.n).toBe(1);
    expect((await env.DB.prepare("SELECT count(*) AS n FROM signups").first<{ n: number }>())!.n).toBe(1);
  });
});

describe("someone who already had an account", () => {
  it("signs in with their email and finds the mail already in R2 under the same account", async () => {
    const w = world();
    await account("old1", "old@x.example");
    const id = ulid(Date.now() - 86_400_000);
    await env.RAW.put(`raw/old1/${id}`, "From: a@b.example\r\n\r\nkept\r\n", { customMetadata: { received_at: String(Date.now() - 86_400_000) } });
    const token = await w.signIn("old@x.example");
    expect(token).toMatch(/^lm1\.old1\./);
    await w.send("GET", "/sync", undefined, token); // starts the first sweep
    const page = (await (await w.send("GET", "/sync", undefined, token)).json()) as { entries: { data: { ulid: string } }[] };
    expect(page.entries.map((x) => x.data.ulid)).toEqual([id]);
    const raw = await w.send("GET", `/sync/raw/${id}`, undefined, token);
    expect(await raw.text()).toContain("kept");
  });
});

describe("a code", () => {
  const email = "code@x.example";
  const inObject = <T>(acct: string, fn: (i: AccountLog, s: DurableObjectState) => T | Promise<T>) => runInDurableObject(logFor(env, acct) as unknown as DurableObjectStub<AccountLog>, fn);

  it("works once, and the store holds no code", async () => {
    const w = world();
    await account("c1", email);
    await w.send("POST", "/auth/login", { email });
    const code = w.codeFor(email);
    const held = await inObject("c1", (_, s) => s.storage.sql.exec<{ hash: string }>("SELECT hash FROM codes").toArray());
    expect(held).toHaveLength(1);
    expect(held[0]!.hash).not.toContain(code);
    expect((await w.send("POST", "/auth/code", { email, code })).status).toBe(200);
    const again = await w.send("POST", "/auth/code", { email, code });
    expect(again.status).toBe(400);
    expect(await again.json()).toEqual({ error: "invalid code" });
  });

  it("names the token, default `app`", async () => {
    const w = world();
    await account("c2", email);
    const token = await w.signIn(email, "  ");
    const { items } = (await (await w.send("GET", "/sync/tokens", undefined, token)).json()) as { items: { name: string; scopes: string[] }[] };
    expect(items.map((t) => t.name)).toEqual(["app"]);
    expect(items[0]!.scopes).toEqual(["read", "draft", "send", "act", "junk"]);
  });

  it("from one account does nothing for another", async () => {
    const w = world();
    await account("c3", email);
    await account("c4", "other@x.example");
    await w.send("POST", "/auth/login", { email });
    await w.send("POST", "/auth/login", { email: "other@x.example" });
    const mine = w.codeFor(email);
    const theirs = w.codeFor("other@x.example");
    if (mine === theirs) return; // one in a million
    expect((await w.send("POST", "/auth/code", { email, code: theirs })).status).toBe(400);
  });

  it("expires after 15 minutes; a welcome code lasts a day", async () => {
    const w = world({ SIGNUP_MODE: "open" });
    await account("c5", email);
    const at = async (now: number, path: string, body: unknown) => {
      const ctx = createExecutionContext();
      const r = (await handleAuth(new Request(ORIGIN + path, { method: "POST", headers: { "cf-connecting-ip": `10.9.0.${ip++ & 255}` }, body: JSON.stringify(body) }), w.e, ctx, now))!;
      await waitOnExecutionContext(ctx);
      return r;
    };
    const t0 = Date.now();
    await at(t0, "/auth/login", { email });
    expect((await at(t0 + 15 * 60_000, "/auth/code", { email, code: w.codeFor(email) })).status).toBe(400);
    await at(t0, "/auth/login", { email });
    expect((await at(t0 + 15 * 60_000 - 1, "/auth/code", { email, code: w.codeFor(email) })).status).toBe(200);

    await at(t0, "/auth/signup", { email: "w@x.example" });
    expect((await at(t0 + 23 * 3_600_000, "/auth/code", { email: "w@x.example", code: w.codeFor("w@x.example") })).status).toBe(200);
  });

  it("dies after five wrong guesses, and used-up codes do not stop a new one being asked for", async () => {
    const w = world();
    await account("c6", email);
    for (let i = 0; i < 3; i++) await w.send("POST", "/auth/login", { email });
    const code = w.codeFor(email);
    const wrong = code === "000000" ? "000001" : "000000";
    for (let i = 0; i < MAX_CODE_ATTEMPTS; i++) expect((await w.send("POST", "/auth/code", { email, code: wrong })).status).toBe(400);
    expect((await w.send("POST", "/auth/code", { email, code })).status).toBe(400);
    expect(await w.signIn(email)).toMatch(/^lm1\./);
  });

  it("is capped at three live: a fourth request mails nothing, and spending one frees a slot", async () => {
    const w = world();
    await account("c7", email);
    for (let i = 0; i < 4; i++) expect((await w.send("POST", "/auth/login", { email })).status).toBe(202);
    expect(w.mail).toHaveLength(3);
    expect((await w.send("POST", "/auth/code", { email, code: w.codeFor(email) })).status).toBe(200);
    await w.send("POST", "/auth/login", { email });
    expect(w.mail).toHaveLength(4);
  });

  it("is answered alike for an address with no account, a pending one and a disabled one, and none gets mail", async () => {
    const w = world();
    await account("c8", "wait@x.example", "pending");
    await account("c9", "off@x.example", "disabled");
    for (const who of ["nobody@x.example", "wait@x.example", "off@x.example"]) {
      expect((await w.send("POST", "/auth/login", { email: who })).status).toBe(202);
      const r = await w.send("POST", "/auth/code", { email: who, code: "123456" });
      expect(r.status).toBe(400);
      expect(await r.json()).toEqual({ error: "invalid code" });
    }
    expect(w.mail).toEqual([]);
  });

  it("stops working when the account is disabled, as does the token it bought", async () => {
    const w = world();
    await account("c10", email);
    const token = await w.signIn(email);
    await w.send("POST", "/auth/login", { email });
    await env.DB.prepare("UPDATE accounts SET status = 'disabled' WHERE id = 'c10'").run();
    expect((await w.send("POST", "/auth/code", { email, code: w.codeFor(email) })).status).toBe(400);
    expect((await w.send("GET", "/sync", undefined, token)).status).toBe(401);
  });

  it("a mail that fails to send is still a 202: the answer says nothing about the account", async () => {
    const w = world({ EMAIL: { send: async () => { throw new Error("refused"); } } } as unknown as Partial<Env>);
    await account("c11", email);
    expect((await w.send("POST", "/auth/login", { email })).status).toBe(202);
  });

  it("reads as a code alone: no link to a page that no longer exists", () => {
    const msg = signinMessage({ from: "no-reply@leanmail.io", to: "a@x.example", kind: "signin", code: "123456", now: 0, id: "01J" });
    expect(msg).toContain("Your LeanMail sign-in code: 123456\r\n");
    expect(msg).not.toContain("http");
  });
});

describe("a device's tokens", () => {
  const email = "tok@x.example";
  const items = async (w: ReturnType<typeof world>, token: string) =>
    ((await (await w.send("GET", "/sync/tokens", undefined, token)).json()) as { items: { id: string; name: string; current: boolean; revoked_at: number | null }[] }).items;

  it("lists each sign-in, and a lost phone can be revoked from the laptop", async () => {
    const w = world();
    await account("t1", email);
    const phone = await w.signIn(email, "phone");
    const laptop = await w.signIn(email, "laptop");
    const listed = await items(w, laptop);
    expect(listed.map((t) => [t.name, t.current]).sort()).toEqual([["laptop", true], ["phone", false]]);
    const lost = listed.find((t) => t.name === "phone")!;
    expect((await w.send("DELETE", `/sync/tokens/${lost.id}`, undefined, laptop)).status).toBe(200);
    expect((await w.send("GET", "/sync", undefined, phone)).status).toBe(401);
    expect((await w.send("GET", "/sync", undefined, laptop)).status).toBe(200);
    expect((await items(w, laptop)).find((t) => t.name === "phone")!.revoked_at).not.toBeNull();
  });

  it("a read-only token may sign itself out but not another; an unknown id is 404", async () => {
    const w = world();
    await account("t2", email);
    const full = await w.signIn(email, "full");
    const narrow = await w.signIn(email, "narrow");
    const listed = await items(w, full);
    const [fullId, narrowId] = [listed.find((t) => t.name === "full")!.id, listed.find((t) => t.name === "narrow")!.id];
    await logFor(env, "t2").putToken(narrowId, ["read"], Date.now());
    expect((await w.send("DELETE", `/sync/tokens/${fullId}`, undefined, narrow)).status).toBe(403);
    expect((await w.send("DELETE", `/sync/tokens/${"0".repeat(64)}`, undefined, full)).status).toBe(404);
    expect((await w.send("DELETE", `/sync/tokens/${narrowId}`, undefined, narrow)).status).toBe(200);
    expect((await w.send("GET", "/sync", undefined, narrow)).status).toBe(401);
    expect((await w.send("GET", "/sync", undefined, full)).status).toBe(200);
  });

  it("one account's token cannot see or revoke another's", async () => {
    const w = world();
    await account("t3", email);
    await account("t4", "other@x.example");
    const mine = await w.signIn(email);
    const theirs = await w.signIn("other@x.example");
    const id = (await items(w, theirs))[0]!.id;
    expect((await items(w, mine)).map((t) => t.id)).not.toContain(id);
    expect((await w.send("DELETE", `/sync/tokens/${id}`, undefined, mine)).status).toBe(404);
    expect((await w.send("GET", "/sync", undefined, theirs)).status).toBe(200);
  });
});
