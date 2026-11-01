import Lean.Data.Json
import Cli.Context
import Cli.Format

/-! # Settings and the operator: `tokens`, `admin`, `gmail`

`tokens` is `/api/tokens`, `gmail` is `/api/gmail`; both are session routes on the server, so a
bearer token alone is refused and they work through a session or a dev server's bypass. `admin` is
the operator container's `/admin/*` (`ROLE=operator`); elsewhere those routes are 404. -/

namespace Cli.Commands

open Lean Cli

def optDate (j : Json) (k : String) (none_ : String := "never") : String :=
  ((jNat? j k).map shortDate).getD none_

def strsOf (j : Json) (k : String) : List String :=
  (jArr j k).toList.filterMap fun | .str s => some s | _ => none

/-- GET, with a 404 explained. -/
def getExplained (cl : Client) (path why : String) (query : List (String × String) := []) : CliM Core.Http.Response := do
  let r ← cl.send "GET" path query
  if r.status == 404 then fail s!"GET {path}: 404; {why}"
  check "GET" path r

def postExplained (cl : Client) (path why : String) (body : Json := Json.mkObj []) : CliM Core.Http.Response := do
  let r ← cl.send "POST" path [] [("Content-Type", "application/json")] body.compress.toUTF8
  if r.status == 404 && (errorDetail r == "not found" || errorDetail r == "") then fail s!"POST {path}: 404; {why}"
  check "POST" path r

/-! ## `tokens` -/

def tokensUsage : String := "tokens list | create --name <n> [--scopes read,draft,send,act,junk] | revoke <id>"

def tokensCmd (c : Ctx) : CliM UInt32 := do
  match c.args.positional with
  | ["list"] | [] =>
    c.only []
    let r ← (← c.client).get "/api/tokens"
    if c.json then printBody (bodyString r)
    else
      for t in jArr (← parseJson r) "items" do
        let revoked := (jNat? t "revoked_at").isSome
        out s!"{c.style.dim (jStr t "id")}  {c.style.bold (fit 20 (jStr t "name"))}  {fit 22 (",".intercalate (strsOf t "scopes"))}  created {optDate t "created_at" ""}  used {optDate t "last_used_at"}{if revoked then "  revoked " ++ optDate t "revoked_at" "" else ""}"
    return 0
  | ["create"] =>
    c.only ["name", "scopes"]
    let some name := c.args.flag? "name" | usageError s!"usage: leanmail {tokensUsage}"
    let scopes := ((c.args.flag? "scopes").getD "read").splitOn "," |>.map Core.Text.Str.trim |>.filter (· ≠ "")
    let r ← (← c.client).postJson "/api/tokens" (Json.mkObj [("name", Json.str name),
      ("scopes", Json.arr (scopes.toArray.map Json.str))])
    if c.json then printBody (bodyString r)
    else
      let j ← parseJson r
      out s!"token   {jStr j "token"}"
      out s!"id      {jStr j "id"}"
      out s!"scopes  {",".intercalate (strsOf j "scopes")}"
      err "leanmail: the token is shown once; `leanmail login <server> --token <token>` saves it"
    return 0
  | ["revoke", id] =>
    c.only []
    let r ← (← c.client).request "DELETE" s!"/api/tokens/{percentEncode id}"
    if c.json then printBody (if r.body.isEmpty then "{\"ok\":true}" else bodyString r) else out s!"revoked {id}"
    return 0
  | _ => usageError s!"usage: leanmail {tokensUsage}"

/-! ## `admin` -/

def adminUsage : String := "admin signups [--all] | approve <id> | reject <id> | accounts | disable <account-id>"

def notOperator : String := "this server is not the operator container (ROLE=operator), or no such id"

def adminCmd (c : Ctx) : CliM UInt32 := do
  match c.args.positional with
  | ["signups"] =>
    c.only [] ["all"]
    let q := if c.args.has "all" then [("status", "all")] else []
    let r ← getExplained (← c.client) "/admin/signups" notOperator q
    if c.json then printBody (bodyString r)
    else
      for s in jArr (← parseJson r) "items" do
        let decision := let d := jStr s "decision"; if d.isEmpty then "pending" else d
        out s!"{c.style.dim (jStr s "id")}  {optDate s "created_at" ""}  {c.style.bold (fit 32 (jStr s "email"))}  {fit 8 decision}  {oneLine (jStr s "note")}"
    return 0
  | ["approve", id] =>
    c.only []
    let r ← postExplained (← c.client) s!"/admin/signups/{percentEncode id}/approve" notOperator
    if c.json then printBody (bodyString r)
    else
      let j ← parseJson r
      out s!"approved {id}: account {jStr j "account_id"}"
    return 0
  | ["reject", id] =>
    c.only []
    let r ← postExplained (← c.client) s!"/admin/signups/{percentEncode id}/reject" notOperator
    if c.json then printBody (if r.body.isEmpty then "{\"ok\":true}" else bodyString r) else out s!"rejected {id}"
    return 0
  | ["accounts"] =>
    c.only []
    let r ← getExplained (← c.client) "/admin/accounts" notOperator
    if c.json then printBody (bodyString r)
    else
      for a in jArr (← parseJson r) "items" do
        out s!"{c.style.dim (jStr a "id")}  {c.style.bold (fit 32 (jStr a "email"))}  {fit 8 (jStr a "status")}{if jBool a "is_admin" then "  admin" else ""}  {", ".intercalate (strsOf a "domains")}"
    return 0
  | ["disable", id] =>
    c.only []
    let r ← postExplained (← c.client) s!"/admin/accounts/{percentEncode id}/disable" notOperator
    if c.json then printBody (bodyString r) else out s!"disabled {id}"
    return 0
  | _ => usageError s!"usage: leanmail {adminUsage}"

/-! ## `gmail` -/

def gmailUsage : String :=
  "gmail status | backfill [status] | backfill start [--keep-forever] | backfill pause|resume|cancel|retry-failed"

def backfillLine (job : Json) : String :=
  if job.isNull then "backfill  none" else
  let pct := match job.getObjVal? "percent" with | .ok v => v.compress | .error _ => "?"
  let n (k : String) := ((jNat? job k).map toString).getD "0"
  s!"backfill  {jStr job "state"} {pct}%  seen {n "seen"}, imported {n "imported"}, duplicates {n "duplicates"}, skipped {n "skipped"}, failed {n "failed"}{let e := jStr job "error_code"; if e.isEmpty then "" else "  error " ++ e}"

def printGmail (j : Json) : CliM Unit := do
  out s!"client    {if jBool j "client_configured" then jStr j "client_id" else "not configured"}"
  out s!"connected {jBool j "connected"}{let e := jStr j "email"; if e.isEmpty then "" else " as " ++ e}{if jBool j "needs_reauth" then "  (needs re-authorization)" else ""}"
  let e := jStr j "last_error"
  unless e.isEmpty do out s!"error     {e}"
  out s!"polled    {optDate j "last_poll_at"}"
  out s!"push      {if jBool j "watch_active" then "active, last " ++ optDate j "last_push_at" else "off"}"
  let sa := strsOf j "send_as"
  unless sa.isEmpty do out s!"send as   {", ".intercalate sa}"
  out (backfillLine (jObj j "backfill"))
  if let some f := jNat? j "failures" then if f > 0 then out s!"failures  {f}"

def gmailCmd (c : Ctx) : CliM UInt32 := do
  let jobOut (r : Core.Http.Response) : CliM UInt32 := do
    if c.json then printBody (bodyString r) else out (backfillLine (jObj (← parseJson r) "job"))
    return 0
  match c.args.positional with
  | ["status"] | [] =>
    c.only []
    let r ← (← c.client).get "/api/gmail"
    if c.json then printBody (bodyString r) else printGmail (← parseJson r)
    return 0
  | ["backfill"] | ["backfill", "status"] =>
    c.only []
    jobOut (← (← c.client).get "/api/gmail/backfill")
  | ["backfill", "start"] =>
    c.only [] ["keep-forever"]
    jobOut (← (← c.client).postJson "/api/gmail/backfill" (Json.mkObj [("keep_forever", Json.bool (c.args.has "keep-forever"))]))
  | ["backfill", a] =>
    c.only []
    unless ["pause", "resume", "cancel", "retry-failed"].contains a do usageError s!"usage: leanmail {gmailUsage}"
    let r ← (← c.client).postJson s!"/api/gmail/backfill/{a}" (Json.mkObj [])
    if a == "retry-failed" then
      if c.json then printBody (bodyString r) else out s!"retry-failed: {(← parseJson r).compress}"
      return 0
    jobOut r
  | _ => usageError s!"usage: leanmail {gmailUsage}"

end Cli.Commands
