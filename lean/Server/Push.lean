import Lean.Data.Json
import Server.State
import Server.Router
import Server.Clock
import Server.Log

/-! # Push tokens

The phone registers its APNs device token here; `Server.Worker.push` sends each new message to
every registered token through the Worker and forgets the ones Apple calls dead.

| route | auth | |
|---|---|---|
| `PUT /api/push/token {token, env?}` | token `act` | register or refresh; `env` is `prod` (default) or `sandbox` |
| `DELETE /api/push/token {token}` (or `?token=`) | token `act` | forget it |
-/

namespace Server.Push

open Lean Server.Db Server.Router

/-- An APNs device token: hex, 32 to 200 characters (64 today). -/
def tokenOk (t : String) : Bool :=
  32 ≤ t.length && t.length ≤ 200 && t.toList.all fun c => c.isDigit || ('a' ≤ c && c ≤ 'f') || ('A' ≤ c && c ≤ 'F')

def put (c : Ctx) (req : Req) : IO Resp := do
  let j := req.json
  let tok := ((jstr? j "token").getD "").toLower
  if !tokenOk tok then return Resp.error 400 "bad_token"
  let env := (jstr? j "env").getD "prod"
  if env != "prod" && env != "sandbox" then return Resp.error 400 "bad_env"
  let now ← Clock.nowMs
  c.st.db.exec "INSERT INTO push_tokens (token, env, created_at, last_seen_at) VALUES (?1, ?2, ?3, ?3)
    ON CONFLICT(token) DO UPDATE SET env = ?2, last_seen_at = ?3" #[tok, env, now]
  Log.info "push.token_registered" [("env", Json.str env)]
  return Resp.ok (Json.mkObj [("ok", Json.bool true), ("env", Json.str env)])

def delete (c : Ctx) (req : Req) : IO Resp := do
  let tok := (((jstr? req.json "token").orElse fun _ => req.param? "token").getD "").toLower
  if !tokenOk tok then return Resp.error 400 "bad_token"
  let n ← c.st.db.execCount "DELETE FROM push_tokens WHERE token = ?" #[tok]
  return Resp.ok (Json.mkObj [("ok", Json.bool true), ("deleted", Json.bool (n > 0))])

def routes : Array Route := #[
  route "PUT" "/api/push/token" (.token "act") put,
  route "DELETE" "/api/push/token" (.token "act") delete
]

end Server.Push
