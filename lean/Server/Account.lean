import Lean.Data.Json
import Server.Router.Types
import Server.Auth.Magic
import Server.Clock

/-! # The account row

`GET`/`PATCH /api/account` for the signed-in user, and `POST /internal/provision`, which the
Worker sends when the operator approves a sign-up (it turns the operator's
`/internal/wake/:acct {provision}` into this call). Provisioning creates the one `account` row
if it is absent and then mails a welcome link. -/

namespace Server.Account

open Lean Server.Db Server.Router

def optNat (r : Row) (k : String) : Json := match r.nat? k with | some n => Json.num n | none => Json.null

def accountJson (c : Config) (r : Row) : Json := Json.mkObj [
  ("email", Json.str (r.text "email")),
  ("display_name", Json.str (r.text "display_name")),
  ("default_from", Json.str (r.text "default_from")),
  ("retention_days", optNat r "retention_days"),
  ("is_admin", Json.bool (r.bool "is_admin" || c.isAdminEmail (r.text "email"))),
  ("created_at", Json.num (r.nat "created_at"))]

def row (st : State) : IO (Option Row) := st.db.queryOne "SELECT * FROM account WHERE id = 1"

/-- What a bare dev server (no provisioned account) answers under the dev bypass. -/
def devAccountJson : Json := Json.mkObj [
  ("email", Json.str "dev@localhost"), ("display_name", Json.str "dev"), ("default_from", Json.str "dev@localhost"),
  ("retention_days", Json.null), ("is_admin", Json.bool true), ("created_at", Json.num 0)]

/-- `GET /api/account` (token `read`, or a session). -/
def get (c : Ctx) (_ : Req) : IO Resp := do
  let some r ← row c.st
    | return if c.principal == .dev then Resp.ok devAccountJson else Resp.notFound
  return Resp.ok (accountJson c.st.config r)

/-- `PATCH /api/account`: `display_name`, `default_from`, `retention_days` (1–3650 or null). -/
def patch (c : Ctx) (req : Req) : IO Resp := do
  let some _ ← row c.st | return Resp.notFound
  let j := req.json
  let now ← Clock.nowMs
  let mut sets : Array String := #[]
  let mut args : Array Value := #[]
  if let some d := jstr? j "display_name" then
    let d := Core.Text.Str.trim d
    if d.length > 200 then return Resp.error 400 "display_name too long"
    sets := sets.push "display_name = ?"; args := args.push (.text d)
  if let some f := jstr? j "default_from" then
    let f := Core.Text.Str.trim f
    let f ← if f.isEmpty then pure "" else match normEmail? f with
      | some e => pure e
      | none => return Resp.error 400 "bad default_from"
    sets := sets.push "default_from = ?"; args := args.push (.text f)
  match j.getObjVal? "retention_days" with
  | .ok .null => sets := sets.push "retention_days = NULL"
  | .ok v =>
    match v.getNat? with
    | .ok n =>
      if n < 1 || n > 3650 then return Resp.error 400 "bad retention_days"
      sets := sets.push "retention_days = ?"; args := args.push (.int n.toInt64)
    | .error _ => return Resp.error 400 "bad retention_days"
  | .error _ => pure ()
  if !sets.isEmpty then
    c.st.db.exec s!"UPDATE account SET {", ".intercalate sets.toList}, updated_at = ? WHERE id = 1" (args.push (.int now.toInt64))
  let some r ← row c.st | return Resp.notFound
  return Resp.ok (accountJson c.st.config r)

/-- `POST /internal/provision {email, is_admin, display_name?}` → `{ok, created}`. -/
def provision (c : Ctx) (req : Req) : IO Resp := do
  let j := req.json
  let some email := (jstr? j "email").bind normEmail? | return Resp.error 400 "bad email"
  if (← row c.st).isSome then
    return Resp.ok (Json.mkObj [("ok", Json.bool true), ("created", Json.bool false)])
  let now ← Clock.nowMs
  let name := String.ofList ((Core.Text.Str.trim ((jstr? j "display_name").getD "")).toList.take 200)
  c.st.db.exec "INSERT OR IGNORE INTO account(id, email, display_name, is_admin, created_at, updated_at) VALUES (1, ?, ?, ?, ?, ?)"
    #[email, name, (jbool? j "is_admin").getD false, now, now]
  let sent ← Auth.Magic.sendLink c.st email "welcome"
  return Resp.ok (Json.mkObj [("ok", Json.bool true), ("created", Json.bool true), ("welcome_sent", Json.bool sent)])

def routes : Array Route := #[
  route "GET" "/api/account" (.token "read") get,
  route "PATCH" "/api/account" .session patch,
  route "POST" "/internal/provision" .internal provision
]

end Server.Account
