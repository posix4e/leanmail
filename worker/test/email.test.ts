import { createExecutionContext, waitOnExecutionContext } from "cloudflare:test";
import { beforeEach, describe, expect, it } from "vitest";
import { FORWARD_BREAKER_PER_HOUR, RAW_MAX, breakerTripped, handleEmail, loopGuard } from "../src/email";
import type { Env } from "../src/env";
import { ULID_RE } from "../src/ulid";
import { addDomain, addRoute, captureLogs, env, fakeMessage, resetDb } from "./helpers";

async function clearBucket() {
  const listed = await env.RAW.list();
  if (listed.objects.length) await env.RAW.delete(listed.objects.map((o) => o.key));
}

beforeEach(async () => {
  await resetDb();
  await clearBucket();
  await addDomain("ex.example", "acct1");
});

async function deliver(msg: ReturnType<typeof fakeMessage>, now = 1_761_900_000_000) {
  const wakes: { acct: string; ulid?: string }[] = [];
  const ctx = createExecutionContext();
  const out = await captureLogs(async () => {
    await handleEmail(msg, env, ctx, { now: () => now, wake: async (_e, acct, ulid) => void wakes.push({ acct, ulid }) });
    await waitOnExecutionContext(ctx);
  });
  const decisions = out.lines.filter((l) => l.ev === "email.decision");
  return { wakes, decisions, text: out.text };
}

describe("vault", () => {
  it("spools raw with metadata, writes the in/ marker, wakes, and logs one content-free decision", async () => {
    await addRoute("*@ex.example", "acct1", "vault");
    const raw = "From: a@b.example\r\nSubject: Secret plans\r\n\r\nhello\r\n";
    const msg = fakeMessage({ to: "Alice@Ex.example", from: "a@b.example", raw });
    const { wakes, decisions, text } = await deliver(msg);

    const rawList = await env.RAW.list({ prefix: "raw/acct1/", include: ["customMetadata"] });
    expect(rawList.objects).toHaveLength(1);
    const key = rawList.objects[0]!.key;
    const id = key.slice("raw/acct1/".length);
    expect(id).toMatch(ULID_RE);
    const obj = (await env.RAW.get(key))!;
    expect(await obj.text()).toBe(raw);
    expect(obj.customMetadata).toEqual({
      rcpt: "alice@ex.example",
      from: "a@b.example",
      received_at: "1761900000000",
      source: "route",
      size: String(raw.length),
    });
    const marker = await env.RAW.get(`in/acct1/${id}`);
    expect(marker).not.toBeNull();
    expect(marker!.size).toBe(0);

    expect(wakes).toEqual([{ acct: "acct1", ulid: id }]);
    expect(msg.forwards).toEqual([]);
    expect(msg.rejected).toBeNull();
    expect(decisions).toEqual([{ ev: "email.decision", acct: "acct1", mail: "vault", forwarded: 0, size: raw.length, match: "domain" }]);
    expect(text.join("\n")).not.toMatch(/@|Secret/);
  });

  it("rejects mail over 25 MiB without reading it", async () => {
    await addRoute("*@ex.example", "acct1", "vault");
    const msg = fakeMessage({ to: "a@ex.example", rawSize: RAW_MAX + 1 });
    const { wakes, decisions } = await deliver(msg);
    expect(msg.rejected).toBe("message too large");
    expect(msg.rawReads).toBe(0);
    expect(wakes).toEqual([]);
    expect((await env.RAW.list()).objects).toEqual([]);
    expect(decisions[0]).toMatchObject({ mail: "vault", error: "oversize" });
  });

  it("the real wake swallows a container failure: the delivery still succeeds", async () => {
    // Containers are not emulated under the pool, so MAIL is a stub whose proxy() throws.
    await addRoute("*@ex.example", "acct1", "vault");
    const calls: string[] = [];
    const MAIL = {
      idFromName: (n: string) => n,
      get: (n: string) => ({
        proxy: async (r: Request, acct: string) => {
          calls.push(`${n} ${acct} ${new URL(r.url).pathname} ${await r.text()}`);
          throw new Error("container down");
        },
      }),
    };
    const e = { ...env, MAIL } as unknown as Env;
    const ctx = createExecutionContext();
    const { lines } = await captureLogs(async () => {
      await handleEmail(fakeMessage({ to: "a@ex.example" }), e, ctx);
      await waitOnExecutionContext(ctx);
    });
    const [marker] = (await env.RAW.list({ prefix: "in/acct1/" })).objects;
    const id = marker!.key.slice("in/acct1/".length);
    expect(calls).toEqual([`acct1 acct1 /internal/wake {"ulid":"${id}"}`]);
    expect(lines.find((l) => l.ev === "wake.failed")).toMatchObject({ acct: "acct1" });
  });
});

describe("drop and forward-only store nothing", () => {
  it("drops an explicit drop route and an unknown address", async () => {
    await addRoute("spam@ex.example", "acct1", "drop");
    for (const to of ["spam@ex.example", "who@unrouted.example"]) {
      const msg = fakeMessage({ to });
      const { wakes, decisions } = await deliver(msg);
      expect(wakes).toEqual([]);
      expect(msg.forwards).toEqual([]);
      expect(msg.rejected).toBeNull();
      expect(decisions[0]).toMatchObject({ mail: "drop", forwarded: 0 });
    }
    expect((await env.RAW.list()).objects).toEqual([]);
  });

  it("forwards each target with a marker header and stores nothing", async () => {
    await addRoute("fwd@ex.example", "acct1", "forward", ["one@else.example", "two@else.example", "loop@ex.example"]);
    const msg = fakeMessage({ to: "fwd@ex.example" });
    const { wakes, decisions } = await deliver(msg);
    expect(msg.forwards.map((f) => f.to)).toEqual(["one@else.example", "two@else.example"]);
    expect(msg.forwards[0]!.headers["x-leanmail-forwarded"]).toBe("fo@ex.example");
    expect(msg.rawReads).toBe(0);
    expect(wakes).toEqual([]);
    expect((await env.RAW.list()).objects).toEqual([]);
    expect(decisions[0]).toMatchObject({ acct: "acct1", mail: "forward", forwarded: 2 });
  });

  it("bounces a forward-only address with no deliverable target, and throws when every forward fails", async () => {
    await addRoute("nowhere@ex.example", "acct1", "forward", ["self@ex.example"]);
    const a = fakeMessage({ to: "nowhere@ex.example" });
    await deliver(a);
    expect(a.rejected).toBe("address not deliverable");

    await addRoute("fail@ex.example", "acct1", "forward", ["one@else.example"]);
    const b = fakeMessage({ to: "fail@ex.example", forwardFails: true });
    await expect(deliver(b)).rejects.toThrow(/forward-only/);
    expect((await env.RAW.list()).objects).toEqual([]);
  });
});

describe("vault+forward", () => {
  it("stores, then forwards with X-LeanMail-Forwarded: <ulid>@<domain>", async () => {
    await addRoute("both@ex.example", "acct1", "vault+forward", ["out@else.example"]);
    const msg = fakeMessage({ to: "both@ex.example" });
    const { wakes, decisions } = await deliver(msg);
    const id = wakes[0]!.ulid!;
    expect(await env.RAW.head(`raw/acct1/${id}`)).not.toBeNull();
    expect(msg.forwards).toEqual([{ to: "out@else.example", headers: { "x-leanmail-forwarded": `${id}@ex.example` } }]);
    expect(decisions[0]).toMatchObject({ mail: "vault+forward", forwarded: 1 });
  });

  it("keeps but does not forward looped or bulk mail", async () => {
    await addRoute("both@ex.example", "acct1", "vault+forward", ["out@else.example"]);
    const msg = fakeMessage({ to: "both@ex.example", headers: { Precedence: "bulk" } });
    const { decisions } = await deliver(msg);
    expect(msg.forwards).toEqual([]);
    expect((await env.RAW.list({ prefix: "raw/acct1/" })).objects).toHaveLength(1);
    expect(decisions[0]).toMatchObject({ forwarded: 0, store_only: "precedence" });
  });

  it("a failed forward is logged, not thrown, once the raw is stored", async () => {
    await addRoute("both@ex.example", "acct1", "vault+forward", ["out@else.example"]);
    const msg = fakeMessage({ to: "both@ex.example", forwardFails: true });
    const { decisions } = await deliver(msg);
    expect(decisions[0]).toMatchObject({ forwarded: 0 });
    expect((await env.RAW.list({ prefix: "in/acct1/" })).objects).toHaveLength(1);
  });

  it("stops forwarding (but keeps storing) once the breaker trips", async () => {
    await addRoute("both@ex.example", "acct1", "vault+forward", ["out@else.example"]);
    const now = 1_761_900_000_000;
    await env.DB.prepare("INSERT INTO forward_window (account_id, window_start, count) VALUES ('acct1', ?1, ?2)").bind(now - 1000, FORWARD_BREAKER_PER_HOUR).run();
    const msg = fakeMessage({ to: "both@ex.example" });
    const { decisions } = await deliver(msg, now);
    expect(msg.forwards).toEqual([]);
    expect(decisions[0]).toMatchObject({ forwarded: 0, store_only: "breaker" });
    expect((await env.RAW.list({ prefix: "raw/acct1/" })).objects).toHaveLength(1);
  });
});

describe("loopGuard", () => {
  const g = (h: Record<string, string> | [string, string][]) => loopGuard(new Headers(h));
  it("flags each store-only signal", () => {
    expect(g({})).toBeNull();
    expect(g({ "X-LeanMail-Forwarded": "01J@ex.example" })).toBe("leanmail_forwarded");
    expect(g({ "X-Forwarded-For": "x" })).toBe("gmail_forwarded");
    expect(g({ "X-Forwarded-To": "x" })).toBe("gmail_forwarded");
    expect(g({ "X-Google-Group-Id": "123" })).toBe("google_group");
    expect(g({ "Auto-Submitted": "auto-replied" })).toBe("auto_submitted");
    expect(g({ "Auto-Submitted": "no" })).toBeNull();
    for (const p of ["bulk", "list", "junk", "Bulk"]) expect(g({ Precedence: p })).toBe("precedence");
    expect(g({ Precedence: "first-class" })).toBeNull();
  });
  it("counts Received hops", () => {
    const hops = (n: number) => Array.from({ length: n }, (_, i): [string, string] => ["Received", `from h${i}.example by mx.example; Mon, 1 Jan 2026 00:00:00 +0000`]);
    expect(g(hops(20))).toBeNull();
    expect(g(hops(21))).toBe("received_count");
  });
});

describe("forward breaker", () => {
  it("trips after 300 in an hour and resets after the window", async () => {
    const t0 = 1_761_900_000_000;
    for (let i = 0; i < FORWARD_BREAKER_PER_HOUR; i++) expect(await breakerTripped(env.DB, "b1", t0 + i)).toBe(false);
    expect(await breakerTripped(env.DB, "b1", t0 + 400)).toBe(true);
    expect(await breakerTripped(env.DB, "b2", t0 + 400)).toBe(false);
    expect(await breakerTripped(env.DB, "b1", t0 + 3_600_000)).toBe(false);
    expect(await env.DB.prepare("SELECT window_start, count FROM forward_window WHERE account_id = 'b1'").first()).toEqual({ window_start: t0 + 3_600_000, count: 1 });
  });
});
