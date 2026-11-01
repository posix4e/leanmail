import Lean.Data.Json
import Server.Router.Types
import Server.Auth.Tokens
import Server.Auth.Session
import Server.Clock

/-! # API tokens

`lm1.<acct>.<rand>.<mac>`, minted in Settings (or by `POST /auth/code` for the apps), shown
once, stored as `sha256` of the whole token with a name and scopes (`read draft send act junk`).
A bearer request passes `.token scope` when the MAC verifies, the hash is known and not revoked,
and the scope is among the token's. `last_used_at` moves at most once an hour. -/

namespace Server.Auth.ApiTokens

open Lean Server.Db Server.Router

def allScopes : List String := ["read", "draft", "send", "act", "junk"]
def hourMs : Nat := 3600 * 1000

def scopesOf (s : String) : List String :=
  match Json.parse s with
  | .ok (.arr xs) => xs.toList.filterMap fun | .str v => some v | _ => none
  | _ => []

def scopesJson (xs : List String) : Json := Json.arr (xs.toArray.map Json.str)

/-- Mint and store a token: `(id, token)`. -/
def mint (st : State) (name : String) (scopes : List String) : IO (String × String) := do
  let id ← Session.newId
  let tok := Tokens.mintApi st.config (← Crypto.randomToken 32)
  st.db.exec "INSERT INTO api_tokens(id, token_hash, name, scopes, created_at) VALUES (?, ?, ?, ?, ?)"
    #[id, Tokens.hash tok, String.ofList (name.toList.take 100), (scopesJson scopes).compress, ← Clock.nowMs]
  return (id, tok)

/-- The token in `Authorization: Bearer`, if it grants `scope`. -/
def fromBearer (st : State) (req : Req) (scope : String) : IO (Except Resp Principal) := do
  let unauth := Resp.error 401 "unauthorized"
  let some tok := req.bearer? | return .error unauth
  let some _ := Tokens.verifyApi st.config tok | return .error unauth
  let some row ← st.db.queryOne "SELECT id, scopes, last_used_at, revoked_at FROM api_tokens WHERE token_hash = ?"
      #[Tokens.hash tok] | return .error unauth
  if !(row.get "revoked_at").isNull then return .error unauth
  let scopes := scopesOf (row.text "scopes")
  if !scopes.contains scope then return .error (Resp.error 403 "insufficient scope")
  let now ← Clock.nowMs
  if (row.nat? "last_used_at").all (· + hourMs ≤ now) then
    st.db.exec "UPDATE api_tokens SET last_used_at = ? WHERE id = ?" #[now, row.text "id"]
  return .ok (.token (row.text "id") scopes)

/-! ## Routes (minting and revoking need a session; a `read` token may list them, for the CLI) -/

def list (c : Ctx) (_ : Req) : IO Resp := do
  let rows ← c.st.db.query "SELECT id, name, scopes, created_at, last_used_at, revoked_at FROM api_tokens ORDER BY created_at DESC"
  let opt (r : Row) (k : String) : Json := match r.nat? k with | some n => Json.num n | none => Json.null
  return Resp.ok <| Json.mkObj [("items", Json.arr <| rows.map fun r => Json.mkObj [
    ("id", Json.str (r.text "id")), ("name", Json.str (r.text "name")),
    ("scopes", scopesJson (scopesOf (r.text "scopes"))), ("created_at", Json.num (r.nat "created_at")),
    ("last_used_at", opt r "last_used_at"), ("revoked_at", opt r "revoked_at")])]

/-- `POST /api/tokens {name, scopes}` → the token, once. -/
def create (c : Ctx) (req : Req) : IO Resp := do
  let j := req.json
  let name := Core.Text.Str.trim ((jstr? j "name").getD "")
  let scopes : List String := match j.getObjVal? "scopes" with
    | .ok (.arr xs) => xs.toList.filterMap fun | .str v => some v | _ => none
    | _ => ["read"]
  if scopes.isEmpty || !scopes.all allScopes.contains then return Resp.error 400 "bad scopes"
  let scopes := allScopes.filter scopes.contains
  let (id, tok) ← mint c.st (if name.isEmpty then "token" else name) scopes
  return Resp.json 201 <| Json.mkObj [("id", Json.str id), ("token", Json.str tok), ("scopes", scopesJson scopes)]

def revoke (c : Ctx) (_ : Req) : IO Resp := do
  c.st.db.exec "UPDATE api_tokens SET revoked_at = ? WHERE id = ? AND revoked_at IS NULL" #[← Clock.nowMs, c.param "id"]
  if (← c.st.db.changes) == 0 then return Resp.notFound
  return Resp.okStatus

def routes : Array Route := #[
  route "GET" "/api/tokens" (.token "read") list,
  route "POST" "/api/tokens" .session create,
  route "DELETE" "/api/tokens/:id" .session revoke
]

end Server.Auth.ApiTokens
