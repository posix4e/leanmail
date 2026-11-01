import Lean.Data.Json
import Core.Ulid
import Server.Router.Types
import Server.Auth.Tokens
import Server.Clock

/-! # Browser sessions

A session is a `sessions` row (id ULID, `token_hash`, created/last-seen/expires, user agent) and
an `lm_session` cookie `v1.<acct>.<sid>.<exp>.<epoch>.<mac>` (`Server.Auth.Tokens`). A cookie is
good while its MAC, account and expiry verify, its row is neither revoked nor expired, and its
epoch equals `account.session_epoch` (bumped by logout-everywhere). The row is found by the sid,
which only a MAC'd cookie can name. Sessions slide: at most once an hour `last_seen_at` and
`expires_at` move to now + 30 days and a fresh cookie with the new expiry is set; `token_hash`
records the newest cookie. Older cookies of the same session stay valid until their own expiry.

CSRF: a cookie-authenticated request other than GET/HEAD needs `Origin` equal to `PUBLIC_ORIGIN`
(or no `Origin`, for same-origin non-browser clients) and `X-Requested-With: LeanMail`. Bearer
requests are exempt. -/

namespace Server.Auth.Session

open Lean Server.Db Server.Router

def cookieName : String := "lm_session"
def ttlMs : Nat := 30 * 24 * 3600 * 1000
def maxAgeS : Nat := 2592000
def slideMs : Nat := 3600 * 1000

def cookieHeader (c : Config) (value : String) (maxAge : Nat := maxAgeS) : String :=
  s!"{cookieName}={value}; HttpOnly{if c.secureCookies then "; Secure" else ""}; SameSite=Lax; Path=/; Max-Age={maxAge}"

def clearCookie (c : Config) : String := cookieHeader c "" 0

def newId : IO String := do return Core.Ulid.encode (← Clock.nowMs) (← Crypto.randomBytes 10)

def accountEpoch (st : State) : IO (Option Nat) := do
  return (← st.db.queryOne "SELECT session_epoch FROM account WHERE id = 1").map (·.nat "session_epoch")

/-- A new session: the cookie value, or `none` when the account row does not exist yet. -/
def create (st : State) (ua : String) : IO (Option String) := do
  let some ep ← accountEpoch st | return none
  let now ← Clock.nowMs
  let sid ← newId
  let exp := now + ttlMs
  let tok := Tokens.mintSession st.config sid exp ep
  st.db.exec "INSERT INTO sessions(id, token_hash, created_at, last_seen_at, expires_at, ua) VALUES (?, ?, ?, ?, ?, ?)"
    #[sid, Tokens.hash tok, now, now, exp, String.ofList (ua.toList.take 200)]
  return some tok

private def trimSlash (s : String) : String :=
  if s.endsWith "/" then String.ofList s.toList.dropLast else s

/-- The CSRF rule for a cookie-authenticated request. -/
def csrfOk (c : Config) (req : Req) : Bool :=
  if req.method == "GET" || req.method == "HEAD" then true else
  let originOk := match req.header? "origin" with
    | none => true
    | some o => trimSlash o == c.origin
  originOk && req.header? "x-requested-with" == some "LeanMail"

/-- The session the `lm_session` cookie names, with any `Set-Cookie` a slide adds. -/
def fromCookie (st : State) (req : Req) : IO (Except Resp (Principal × List (String × String))) := do
  let unauth := Resp.error 401 "unauthorized"
  let some tok := req.cookie? cookieName | return .error unauth
  let now ← Clock.nowMs
  let some cl := Tokens.verifySession st.config now tok | return .error unauth
  let some row ← st.db.queryOne
      "SELECT s.revoked_at, s.expires_at, s.last_seen_at, a.session_epoch AS epoch FROM sessions s, account a WHERE s.id = ? AND a.id = 1"
      #[cl.sid] | return .error unauth
  if !(row.get "revoked_at").isNull || row.nat "expires_at" ≤ now || row.nat "epoch" != cl.epoch then
    return .error unauth
  if !csrfOk st.config req then return .error (Resp.error 403 "csrf")
  if now ≥ row.nat "last_seen_at" + slideMs then
    let exp := now + ttlMs
    let tok' := Tokens.mintSession st.config cl.sid exp cl.epoch
    st.db.exec "UPDATE sessions SET last_seen_at = ?, expires_at = ?, token_hash = ? WHERE id = ?"
      #[now, exp, Tokens.hash tok', cl.sid]
    return .ok (.session cl.sid, [("Set-Cookie", cookieHeader st.config tok')])
  return .ok (.session cl.sid, [])

/-! ## Routes -/

def ok (c : Ctx) : Resp := (Resp.okStatus).withHeader "Set-Cookie" (clearCookie c.st.config)

/-- `POST /api/logout`: revoke this session. -/
def logout (c : Ctx) (_ : Req) : IO Resp := do
  if let .session sid := c.principal then
    c.st.db.exec "UPDATE sessions SET revoked_at = ? WHERE id = ? AND revoked_at IS NULL" #[← Clock.nowMs, sid]
  return ok c

/-- `POST /api/logout-everywhere`: bump the epoch, which no existing cookie carries. -/
def logoutEverywhere (c : Ctx) (_ : Req) : IO Resp := do
  let now ← Clock.nowMs
  c.st.db.transaction do
    c.st.db.execUnlocked "UPDATE account SET session_epoch = session_epoch + 1, updated_at = ? WHERE id = 1" #[now]
    c.st.db.execUnlocked "UPDATE sessions SET revoked_at = ? WHERE revoked_at IS NULL" #[now]
  return ok c

def list (c : Ctx) (_ : Req) : IO Resp := do
  let now ← Clock.nowMs
  let rows ← c.st.db.query
    "SELECT id, created_at, last_seen_at, expires_at, ua FROM sessions WHERE revoked_at IS NULL AND expires_at > ? ORDER BY last_seen_at DESC" #[now]
  let cur := match c.principal with | .session sid => sid | _ => ""
  return Resp.ok <| Json.mkObj [("items", Json.arr <| rows.map fun r => Json.mkObj [
    ("id", Json.str (r.text "id")), ("created_at", Json.num (r.nat "created_at")),
    ("last_seen_at", Json.num (r.nat "last_seen_at")), ("expires_at", Json.num (r.nat "expires_at")),
    ("ua", Json.str (r.text "ua")), ("current", Json.bool (r.text "id" == cur))])]

def revoke (c : Ctx) (_ : Req) : IO Resp := do
  c.st.db.exec "UPDATE sessions SET revoked_at = ? WHERE id = ? AND revoked_at IS NULL" #[← Clock.nowMs, c.param "id"]
  if (← c.st.db.changes) == 0 then return Resp.notFound
  return Resp.okStatus

def routes : Array Route := #[
  route "POST" "/api/logout" .session logout,
  route "POST" "/api/logout-everywhere" .session logoutEverywhere,
  route "GET" "/api/sessions" .session list,
  route "DELETE" "/api/sessions/:id" .session revoke
]

end Server.Auth.Session
