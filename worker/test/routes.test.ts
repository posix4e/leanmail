import { beforeEach, describe, expect, it } from "vitest";
import { domainsNotOwnedBy, listActiveAccounts, lookupRoute, upsertAccounts, upsertRoutes } from "../src/routes";
import { ulid, ulidTime, ULID_RE } from "../src/ulid";
import { addRoute, env, resetDb } from "./helpers";

beforeEach(resetDb);

describe("lookupRoute", () => {
  it("prefers the exact address, then the +tag-stripped address, then *@domain", async () => {
    await addRoute("*@ex.example", "a1", "vault");
    await addRoute("alice@ex.example", "a1", "vault+forward", ["out@else.example"]);
    await addRoute("bob@ex.example", "a1", "drop");
    await addRoute("bob+keep@ex.example", "a1", "forward", ["x@else.example"]);

    const exact = await lookupRoute(env.DB, "<Alice@EX.example>");
    expect(exact).toMatchObject({ match: "exact", mail: "vault+forward", account_id: "a1", forward_to: ["out@else.example"] });
    expect(await lookupRoute(env.DB, "alice+news@ex.example")).toMatchObject({ match: "stripped", mail: "vault+forward" });
    expect(await lookupRoute(env.DB, "bob+keep@ex.example")).toMatchObject({ match: "exact", mail: "forward" });
    expect(await lookupRoute(env.DB, "bob+other@ex.example")).toMatchObject({ match: "stripped", mail: "drop" });
    expect(await lookupRoute(env.DB, "nobody@ex.example")).toMatchObject({ match: "domain", mail: "vault", forward_to: [] });
    expect(await lookupRoute(env.DB, "nobody@unknown.example")).toBeNull();
    expect(await lookupRoute(env.DB, "not-an-address")).toBeNull();
  });
});

describe("upserts", () => {
  it("upserts routes, and replace removes the account's other rows only", async () => {
    await upsertRoutes(env.DB, "a1", [
      { address: "A@ex.example", mail: "vault", forward_to: [], can_send: true },
      { address: "b@ex.example", mail: "drop", forward_to: [], can_send: false },
    ]);
    await addRoute("c@other.example", "a2", "vault");
    await upsertRoutes(env.DB, "a1", [{ address: "a@ex.example", mail: "forward", forward_to: ["Z@else.example"], can_send: false }], { replace: true });
    const { results } = await env.DB.prepare("SELECT address, account_id, mail, forward_to, can_send FROM routes ORDER BY address").all();
    expect(results).toEqual([
      { address: "a@ex.example", account_id: "a1", mail: "forward", forward_to: '["z@else.example"]', can_send: 0 },
      { address: "c@other.example", account_id: "a2", mail: "vault", forward_to: "[]", can_send: 0 },
    ]);
  });

  it("upserts accounts and domains; lists active accounts", async () => {
    await upsertAccounts(
      env.DB,
      [
        { id: "a1", email: "one@x.example", status: "active", has_gmail: false },
        { id: "a2", email: "two@x.example", status: "pending", has_gmail: true },
      ],
      [{ name: "ex.example", account_id: "a1" }],
    );
    await upsertAccounts(env.DB, [{ id: "a2", email: "two@x.example", status: "active", has_gmail: true }]);
    expect(await listActiveAccounts(env.DB)).toEqual(["a1", "a2"]);
    expect(await domainsNotOwnedBy(env.DB, "a1", ["ex.example", "EX.example", "nope.example"])).toEqual(["nope.example"]);
  });
});

describe("ulid", () => {
  it("is 26 Crockford characters carrying the time", () => {
    const id = ulid(1_761_900_000_000);
    expect(id).toMatch(ULID_RE);
    expect(ulidTime(id)).toBe(1_761_900_000_000);
    expect(ulid(1) < ulid(2)).toBe(true);
    expect(ulid()).not.toBe(ulid());
  });
});
