import Lean.Data.Json
import Server.Gmail.Client
import Server.Auth.Tokens
import Server.Router.Types

/-! # Gmail: OAuth

The owner brings their own Google OAuth client (`POST /api/gmail/client`). `GET
/api/gmail/authorize` sends the browser to Google with `access_type=offline prompt=consent` (so a
refresh token is issued every time) and a `state` the Worker can route on:

    v1.<acct>.goauth.<nonce>.<exp>.<mac>        mac as every credential (`Server.Auth.Tokens`)

The `goauth` purpose segment keeps it from verifying as a session (three payload parts, but the
first is never a session id here) or a magic link (two). The nonce is also kept in `settings`
(`gmail.oauth_nonce`) and is single use, so a state minted for one browser cannot be replayed.

Google redirects to `PUBLIC_ORIGIN/oauth/google/callback?code&state`; the Worker verifies the MAC
and proxies to the account the state names (`worker/src/gmail.ts`); this re-verifies, exchanges
the code at Google's token endpoint, reads the mailbox address from `users.getProfile`, and
answers 302 `/settings/gmail?connected=1`.

`accessToken` refreshes on demand and keeps the token in memory for 50 minutes (Google's last an
hour). A refused refresh (`invalid_grant`: the seven-day expiry of a Testing consent screen, or
consent revoked) is written to `last_error` as `refresh_rejected:<error>`; the lanes stop until
the owner connects again, which clears it. -/

namespace Server.Gmail.OAuth

open Lean Server.Db Server.Router Server.Gmail

def statePurpose : String := "goauth"
def stateTtlMs : Nat := 10 * 60000
def tokenTtlMs : Nat := 50 * 60000
def nonceKey : String := "gmail.oauth_nonce"
def sendAsKey : String := "gmail.send_as"

def redirectUri (c : Config) : String := c.origin ++ "/oauth/google/callback"

def mintState (c : Config) (nonce : String) (exp : Nat) : String :=
  Auth.Tokens.mint c.sessionSecret "v1" c.accountId [statePurpose, nonce, toString exp]

/-- The nonce of a state this account minted that has not expired. -/
def verifyState (c : Config) (now : Nat) (state : String) : Option String :=
  match Auth.Tokens.verifyFor c "v1" state with
  | some [p, nonce, exp] =>
    match exp.toNat? with
    | some e => if p == statePurpose && !nonce.isEmpty && now < e then some nonce else none
    | none => none
  | _ => none

def authorizeUrl (c : Config) (clientId state : String) : String :=
  authUrl ++ query [("client_id", clientId), ("redirect_uri", redirectUri c), ("response_type", "code"),
    ("scope", " ".intercalate scopes), ("access_type", "offline"), ("prompt", "consent"),
    ("include_granted_scopes", "true"), ("state", state)]

/-! ## Access tokens -/

/-- (client and refresh token digest, access token, monotonic expiry). -/
initialize cache : IO.Ref (Option (String × String × Nat)) ← IO.mkRef none

def clearCache : IO Unit := cache.set none

private def cacheKey (clientId refresh : String) : String := Crypto.sha256Hex (clientId ++ "\n" ++ refresh).toUTF8

private def remember (clientId refresh token : String) : IO Unit := do
  cache.set (some (cacheKey clientId refresh, token, (← IO.monoMsNow) + tokenTtlMs))

def tokenRequest (st : State) (kv : List (String × String)) : IO Core.Http.Response :=
  call st { method := "POST", url := tokenUrl, timeoutMs := 30000
            headers := [("Content-Type", "application/x-www-form-urlencoded")], body := formBody kv }

/-- A short-lived access token for the connected mailbox. -/
def accessToken (st : State) : IO String := do
  let some c ← conn? st | googleFail "no_client"
  let some refresh := c.refreshToken | googleFail "not_authorized"
  if !c.enabled then googleFail "not_authorized"
  let key := cacheKey c.clientId refresh
  if let some (k, tok, exp) ← cache.get then
    if k == key && (← IO.monoMsNow) < exp then return tok
  let r ← tokenRequest st [("client_id", c.clientId), ("client_secret", c.clientSecret),
    ("refresh_token", refresh), ("grant_type", "refresh_token")]
  if r.status == 400 || r.status == 401 then
    let why := String.ofList (((jStr? (parseBody r.body) "error").getD (toString r.status)).toList.take 40)
    Log.warn "gmail.refresh_rejected" [("status", Json.num r.status.toNat), ("error", Json.str why)]
    updateConn st "last_error = ?" #[.text s!"refresh_rejected:{why}"]
    clearCache
    googleFail "refresh_rejected"
  if !r.isOk then googleFail s!"token_{r.status}"
  let some tok := jStr? (parseBody r.body) "access_token" | googleFail "no_access_token"
  if c.lastError.isSome then updateConn st "last_error = NULL" #[]
  remember c.clientId refresh tok
  return tok

/-! ## Connecting -/

/-- Store or replace the owner's OAuth client. The refresh token survives saving the same client
again (a rolled secret); a different client cannot use it, so that one starts unauthorized. -/
def saveClient (st : State) (clientId secret : String) : IO (Except String Unit) := do
  let clientId := Core.Text.Str.trim clientId
  let secret := Core.Text.Str.trim secret
  if !clientId.endsWith ".apps.googleusercontent.com" || clientId.length > 200 then return .error "client_id"
  if secret.length < 10 || secret.length > 200 then return .error "client_secret"
  let now ← Clock.nowMs
  match ← conn? st with
  | some c =>
    let keep := c.clientId == clientId
    updateConn st (if keep then "client_secret = ?" else "client_id = ?, client_secret = ?, refresh_token = NULL, last_error = NULL")
      (if keep then #[.text secret] else #[.text clientId, .text secret])
    if !keep then clearCache
  | none =>
    st.db.exec "INSERT INTO gmail_connection (id, client_id, client_secret, enabled, created_at, updated_at)
      VALUES (1, ?, ?, 1, ?, ?)" #[clientId, secret, now, now]
  Log.info "gmail.client_saved" []
  return .ok ()

/-- The Google consent URL, with a fresh single-use state. -/
def startAuthorize (st : State) : IO (Except String String) := do
  let some c ← conn? st | return .error "no_client"
  let nonce ← Crypto.randomToken 18
  setSetting st nonceKey nonce
  let exp := (← Clock.nowMs) + stateTtlMs
  return .ok (authorizeUrl st.config c.clientId (mintState st.config nonce exp))

/-- The addresses Gmail sends as, refreshed from `settings.sendAs` (best effort). -/
def refreshSendAs (st : State) (tok : String) : IO Unit := do
  try
    let xs ← sendAsList st tok
    setSetting st sendAsKey (strArr xs).compress
  catch e => Log.warn "gmail.send_as_failed" [("code", Json.str (codeOf e))]

/-- Trade the authorization code for a refresh token, and learn which mailbox it is. -/
def exchange (st : State) (code : String) : IO Unit := do
  let some c ← conn? st | googleFail "no_client"
  let r ← tokenRequest st [("code", code), ("client_id", c.clientId), ("client_secret", c.clientSecret),
    ("redirect_uri", redirectUri st.config), ("grant_type", "authorization_code")]
  if !r.isOk then googleFail s!"token_{r.status}"
  let j := parseBody r.body
  let some refresh := jStr? j "refresh_token" | googleFail "no_refresh_token"
  let some tok := jStr? j "access_token" | googleFail "no_access_token"
  let p ← profile st tok
  let some email := (jStr? p "emailAddress").map (·.toLower) | googleFail "no_profile"
  let hid := jStrNum? p "historyId"
  -- a different mailbox than before starts its cursor and its watch afresh
  let same := c.email == some email
  updateConn st (if same then "refresh_token = ?, email = ?, enabled = 1, last_error = NULL, history_id = COALESCE(history_id, ?)"
      else "refresh_token = ?, email = ?, enabled = 1, last_error = NULL, history_id = ?, watch_expiration = NULL")
    #[.text refresh, .text email, (hid : Value)]
  remember c.clientId refresh tok
  refreshSendAs st tok
  Log.info "gmail.connected" [("same_mailbox", Json.bool same)]

/-- Forget the connection: stop the watch while the token still works, cancel the backfill,
let go of what the mirror and spam lanes still held, drop the row. Mail already stored stays. -/
def forget (st : State) : IO Unit := do
  if let some _ ← usable? st then
    try stopWatch st (← accessToken st)
    catch e => Log.warn "gmail.stop_watch_failed" [("code", Json.str (codeOf e))]
  let now ← Clock.nowMs
  st.db.exec "UPDATE history_jobs SET state = 'cancelled', finished_at = ?1, updated_at = ?1 WHERE state IN ('running','paused')" #[now]
  st.db.exec "UPDATE gmail_mirror SET state = 'released', reason = 'account_gone', updated_at = ? WHERE state IN ('removing','restoring')" #[now]
  st.db.exec "UPDATE gmail_spam SET state = 'released', reason = 'account_gone', updated_at = ? WHERE state IN ('reporting','restoring')" #[now]
  st.db.exec "DELETE FROM gmail_connection"
  delSetting st nonceKey
  delSetting st sendAsKey
  clearCache
  Log.info "gmail.forgotten" []

/-! ## Handlers -/

def redirectTo (loc : String) : Resp := { status := 302, headers := [("Location", loc), ("Cache-Control", "no-store")] }

/-- `POST /api/gmail/client {client_id, client_secret}`. -/
def clientPost (c : Ctx) (req : Req) : IO Resp := do
  let j := req.json
  match ← saveClient c.st ((jstr? j "client_id").getD "") ((jstr? j "client_secret").getD "") with
  | .error f => return Resp.json 400 (Json.mkObj [("error", Json.str "bad_request"), ("field", Json.str f)])
  | .ok () => return Resp.ok (Json.mkObj [("ok", Json.bool true), ("authorize_url", Json.str "/api/gmail/authorize")])

/-- `GET /api/gmail/authorize`: 302 to Google. -/
def authorizeGet (c : Ctx) (_ : Req) : IO Resp := do
  match ← startAuthorize c.st with
  | .error e => return Resp.error 400 e
  | .ok url => return redirectTo url

/-- `GET /oauth/google/callback?code&state` (public; the state is the credential). -/
def callback (c : Ctx) (req : Req) : IO Resp := do
  let st := c.st
  if (req.param? "error").isSome then return redirectTo "/settings/gmail?error=denied"
  let code := (req.param? "code").getD ""
  let state := (req.param? "state").getD ""
  let now ← Clock.nowMs
  let some nonce := verifyState st.config now state | return redirectTo "/settings/gmail?error=state"
  let stored ← settingRow? st nonceKey
  let fresh := match stored with
    | some (v, at_) => Crypto.constantTimeEq v.toUTF8 nonce.toUTF8 && now ≤ at_ + stateTtlMs
    | none => false
  if code.isEmpty || !fresh then return redirectTo "/settings/gmail?error=state"
  delSetting st nonceKey
  try
    exchange st code
    return redirectTo "/settings/gmail?connected=1"
  catch e =>
    let code := codeOf e
    Log.warn "gmail.exchange_failed" [("code", Json.str code)]
    return redirectTo s!"/settings/gmail?error={R2.uriEncode code}"

/-- `DELETE /api/gmail`. -/
def deleteConn (c : Ctx) (_ : Req) : IO Resp := do
  forget c.st
  return Resp.okStatus

end Server.Gmail.OAuth
