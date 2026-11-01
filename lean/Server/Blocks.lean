import Lean.Data.Json
import Core.Junk
import Core.Text.Str
import Server.Db

/-! # Sender blocks and the junk verdict, the parts ingest needs

A port of MailVault's `src/blocks.ts` without the HMAC: with no end-to-end encryption the server
reads the sender anyway, so `sender_blocks.value` is the address or domain itself. A domain block
covers its subdomains down to two labels (`domainAndParents`). The vault's own domains are never
blockable as domains (that would file the person's own mail in Junk); an address on them is.

`ownDomainsUnlocked` is the junk check's `ownDomains`: the `domains` table plus the domain of
`account.default_from`. -/

namespace Server.Blocks

open Lean Server.Db Core.Text

/-- A domain block covers its subdomains, this far down. -/
def parentsMax : Nat := 5

private def labelOk (l : String) : Bool :=
  !l.isEmpty && l.length ≤ 63 && l.toList.all (fun c => c.isAlphanum || c == '-') &&
    !l.startsWith "-" && !l.endsWith "-"

def isDomain (d : String) : Bool :=
  let ls := d.splitOn "."
  d.length ≤ 253 && ls.length ≥ 2 && ls.all labelOk && d.toList.all (fun c => c.toNat < 128)

def isAddress (a : String) : Bool :=
  match a.splitOn "@" with
  | [l, d] => !l.isEmpty && l.length ≤ 64 && isDomain d &&
      l.toList.all (fun c => c.toNat > 0x20 && c.toNat < 0x7F && !"<>()\",;:@[]\\".toList.contains c)
  | _ => false

/-- The one spelling of what is blocked, or `none` for something that is neither an address nor a domain. -/
def normalize (scope value : String) : Option String :=
  let v := (Str.trim value).toLower
  if scope == "address" then
    let v := if v.startsWith "<" && v.endsWith ">" then String.ofList (v.toList.drop 1 |>.dropLast) else v
    if isAddress v then some v else none
  else if scope == "domain" then
    let v := if v.startsWith "@" then String.ofList (v.toList.drop 1) else v
    let v := String.ofList (v.toList.reverse.dropWhile (· == '.') |>.reverse)
    if isDomain v then some v else none
  else none

/-- The domain and the parents of it a block on any of them covers, down to two labels. -/
def domainAndParents (domain : String) : List String := Id.run do
  let labels := domain.splitOn "."
  let mut out : List String := []
  for i in [0:labels.length] do
    if i + 2 > labels.length || out.length ≥ parentsMax then break
    out := out ++ [".".intercalate (labels.drop i)]
  return out

def domainOf (addr : String) : String :=
  match addr.splitOn "@" with
  | [_, d] => d.toLower
  | _ => ""

/-- Whether `d` is one of `own` or under one of them. -/
def isOwn (own : List String) (d : String) : Bool := own.any fun o => d == o || d.endsWith ("." ++ o)

/-- The vault's own domains (the junk check's context), inside a transaction or not. -/
def ownDomainsUnlocked (db : Db) : IO (List String) := do
  let rows ← db.queryUnlocked "SELECT name FROM domains"
  let df ← db.queryUnlocked "SELECT default_from FROM account WHERE id = 1"
  let dfd := domainOf (((df[0]?).map (·.text "default_from")).getD "")
  return ((rows.toList.map (·.text "name")) ++ (if dfd.isEmpty then [] else [dfd])).map (·.toLower) |>.eraseDups

/-- The block a sender is under: its address, else its domain or a parent of it. -/
def blockForUnlocked (db : Db) (fromAddr : String) : IO (Option String) := do
  let a := (Str.trim fromAddr).toLower
  if a.isEmpty then return none
  let d := domainOf a
  let ds := if isDomain d then domainAndParents d else []
  let vals : Array Value := #[.text a] ++ ds.toArray.map Value.text
  let qs := ", ".intercalate (vals.toList.map fun _ => "?")
  let rows ← db.queryUnlocked s!"SELECT id FROM sender_blocks WHERE value IN ({qs}) ORDER BY scope LIMIT 1" vals
  return rows[0]?.map (·.text "id")

def countHitUnlocked (db : Db) (id : String) (now : Nat) : IO Unit :=
  db.execUnlocked "UPDATE sender_blocks SET hits = hits + 1, last_hit_at = ? WHERE id = ?" #[now, id]

/-! ## The verdict as JSON (`messages.junk_verdict`) -/

def reasonJson (r : Core.Junk.Reason) : Json := Json.mkObj [
  ("code", Json.str r.code), ("family", Json.str r.family.toString), ("weight", Json.num r.weight),
  ("detail", Json.mkObj (r.detail.map fun (k, v) => (k, Json.str v)))]

def verdictJson (v : Core.Junk.Verdict) : Json := Json.mkObj [
  ("v", Json.num v.v), ("verdict", Json.str v.verdict.toString), ("phish", Json.num v.phish),
  ("spam", Json.num v.spam), ("reasons", Json.arr (v.reasons.toArray.map reasonJson))]

/-- The reasons of a stored verdict, as `Core.Junk.Reason`s (for `Core.Junk.reasonText`). -/
def reasonsOfJson (j : Json) : List Core.Junk.Reason :=
  ((j.getObjValAs? (Array Json) "reasons").toOption.getD #[]).toList.map fun r =>
    let detail := match r.getObjVal? "detail" with
      | .ok (.obj kvs) => kvs.toArray.toList.filterMap fun (k, v) => match v with | .str s => some (k, s) | _ => none
      | _ => []
    { code := (r.getObjValAs? String "code").toOption.getD ""
      family := if (r.getObjValAs? String "family").toOption == some "phish" then .phish else .spam
      weight := (r.getObjValAs? Nat "weight").toOption.getD 0, detail }

/-- `kind` of a stored verdict (`clean`, `suspicious`, `spam`, `phishing`). -/
def kindOfJson (j : Json) : String := (j.getObjValAs? String "verdict").toOption.getD "clean"

end Server.Blocks
