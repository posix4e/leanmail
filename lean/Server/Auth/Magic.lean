import Lean.Data.Json
import Core.Base64
import Core.Ulid
import Server.Router.Types
import Server.Auth.Tokens
import Server.Auth.Session
import Server.Auth.ApiTokens
import Server.Mail.Build
import Server.Worker
import Server.Clock

/-! # Magic links and codes

The Worker answers `POST /auth/login` itself (always 202) and asks the account's container
`POST /internal/auth/magic {email, purpose}`. When the email is the account's, the container
mints a link `v1.<acct>.<rand>.<exp>.<mac>` and a 6-digit code, keeps `sha256(link)` and a keyed
hash of the code in `magic_links`, and mails both through the Worker's `/internal/email/send`.

- `GET /auth/magic?t=<link>` (the Worker routes it by the MAC): one use, then a session cookie
  and a redirect to `/`.
- `POST /auth/code {email, code, name}` (native apps): the code of a live link, compared in
  constant time, at most five tries per link; then an API token with every scope, shown once.

Sign-in links live 15 minutes; welcome links (sent on provisioning) 24 hours. -/

namespace Server.Auth.Magic

open Lean Server.Db Server.Router

def signinTtlMs : Nat := 15 * 60 * 1000
def welcomeTtlMs : Nat := 24 * 3600 * 1000
def maxAttempts : Nat := 5

/-- What `magic_links.code6` holds: HMAC under the session secret, so a copied database does not
hand out live codes. -/
def codeHash (c : Config) (code : String) : String := Crypto.toHex (Crypto.hmacText c.sessionSecret ("code6:" ++ code))

/-- Six random digits, zero-padded. -/
def newCode : IO String := do
  let b ← Crypto.randomBytes 4
  let n := ((b[0]!.toNat <<< 24) ||| (b[1]!.toNat <<< 16) ||| (b[2]!.toNat <<< 8) ||| b[3]!.toNat) % 1000000
  let s := toString n
  return String.ofList (List.replicate (6 - s.length) '0') ++ s

def accountEmail (st : State) : IO (Option String) := do
  return (← st.db.queryOne "SELECT email FROM account WHERE id = 1").map (·.text "email")

def mailBody (purpose link code : String) (ttl : String) : String :=
  let intro := if purpose == "welcome" then
      "Your LeanMail account is ready. Use this link to sign in:"
    else "Use this link to sign in to LeanMail:"
  s!"Hello,\n\n{intro}\n\n{link}\n\nOr enter this code in the app: {code}\n\nThe link and the code work once and expire in {ttl}. If you did not ask for this, ignore this mail.\n"

/-- Mint a link and a code, store their hashes, and ask the Worker to mail them. True when the
Worker accepted the mail. -/
def sendLink (st : State) (email purpose : String) : IO Bool := do
  let c := st.config
  let now ← Clock.nowMs
  let ttl := if purpose == "welcome" then welcomeTtlMs else signinTtlMs
  let exp := now + ttl
  let tok := Tokens.mintMagic c (← Crypto.randomToken 24) exp
  let code ← newCode
  st.db.exec "INSERT INTO magic_links(token_hash, code6, purpose, expires_at, created_at) VALUES (?, ?, ?, ?, ?)"
    #[Tokens.hash tok, codeHash c code, purpose, exp, now]
  let link := s!"{c.origin}/auth/magic?t={tok}"
  let mid := s!"<{Core.Ulid.encode now (← Crypto.randomBytes 10)}@{Config.originHost c.publicOrigin}>"
  let raw := Mail.Build.textMessage {
    sender := c.mailFrom, recipient := email, date := now, messageId := mid
    subject := if purpose == "welcome" then "Welcome to LeanMail" else "Sign in to LeanMail"
    body := mailBody purpose link code (if purpose == "welcome" then "24 hours" else "15 minutes") }
  let r ← Worker.internal st "/internal/email/send" <| Json.mkObj [
    ("from", Json.str c.mailFrom), ("to", Json.arr #[Json.str email]),
    ("raw_b64", Json.str (Core.Base64.encode raw.toUTF8))]
  Log.info "auth.magic_sent" [("purpose", Json.str purpose), ("ok", Json.bool r.isSome)]
  return r.isSome

/-- `POST /internal/auth/magic {email, purpose}`: 202 whether or not the email is the account's. -/
def request (c : Ctx) (req : Req) : IO Resp := do
  let j := req.json
  let purpose := (jstr? j "purpose").getD "signin"
  if purpose != "signin" && purpose != "welcome" then return Resp.error 400 "bad purpose"
  let accepted := Resp.okStatus 202
  let some email := (jstr? j "email").bind normEmail? | return accepted
  let some mine ← accountEmail c.st | return accepted
  if mine.toLower != email then return accepted
  if ← sendLink c.st mine purpose then return accepted
  return Resp.error 502 "send failed"

def badLink : Resp :=
  { status := 400, headers := [("Content-Type", "text/html; charset=utf-8"), ("Cache-Control", "no-store")]
    body := "<!doctype html><meta charset=utf-8><title>LeanMail</title><p>This sign-in link has expired or was already used. <a href=\"/\">Ask for a new one.</a></p>".toUTF8 }

/-- `GET /auth/magic?t=`: spend the link, start a session. -/
def consume (c : Ctx) (req : Req) : IO Resp := do
  let st := c.st
  let some t := req.param? "t" | return badLink
  let now ← Clock.nowMs
  let some _ := Tokens.verifyMagic st.config now t | return badLink
  let spent ← st.db.transaction do
    st.db.execUnlocked "UPDATE magic_links SET used_at = ? WHERE token_hash = ? AND used_at IS NULL AND expires_at > ?"
      #[now, Tokens.hash t, now]
    return (← st.db.changes) == 1
  if !spent then return badLink
  let some cookie ← Session.create st ((req.header? "user-agent").getD "") | return badLink
  return { status := 302, headers := [("Location", "/"), ("Cache-Control", "no-store"),
                                      ("Set-Cookie", Session.cookieHeader st.config cookie)] }

/-- Spend the 6-digit code of a live link for the account's `email`: true once, for the right
code. A wrong code costs every live link one of its tries; the fifth burns it. Shared by
`POST /auth/code` (an API token) and the web sign-in form (a session, `Server.Pages.Signin`). -/
def spendCode (st : State) (email entered : String) : IO Bool := do
  let some email := normEmail? email | return false
  let given := Core.Text.Str.trim entered
  let some mine ← accountEmail st | return false
  if mine.toLower != email || given.length != 6 || !given.toList.all Char.isDigit then return false
  let now ← Clock.nowMs
  let h := codeHash st.config given
  st.db.transaction do
    let live ← st.db.queryUnlocked "SELECT token_hash, code6 FROM magic_links WHERE used_at IS NULL AND expires_at > ?" #[now]
    -- compare against every live link, without stopping early
    let hit := live.foldl (init := none) fun acc r =>
      if Crypto.constantTimeEq (r.text "code6").toUTF8 h.toUTF8 then some (r.blob "token_hash") else acc
    match hit with
    | some th =>
      st.db.execUnlocked "UPDATE magic_links SET used_at = ? WHERE token_hash = ? AND used_at IS NULL" #[now, th]
      return true
    | none =>
      st.db.execUnlocked
        "UPDATE magic_links SET attempts = attempts + 1, used_at = CASE WHEN attempts + 1 >= ? THEN ? ELSE used_at END WHERE used_at IS NULL AND expires_at > ?"
        #[maxAttempts, now, now]
      return false

/-- `POST /auth/code {email, code, name}` → `{token}`. -/
def code (c : Ctx) (req : Req) : IO Resp := do
  let st := c.st
  let j := req.json
  let name := Core.Text.Str.trim ((jstr? j "name").getD "")
  if !(← spendCode st ((jstr? j "email").getD "") ((jstr? j "code").getD "")) then return Resp.error 400 "invalid code"
  let (_, tok) ← ApiTokens.mint st (if name.isEmpty then "app" else name) ApiTokens.allScopes
  return Resp.ok (Json.mkObj [("token", Json.str tok)])

def routes : Array Route := #[
  route "POST" "/internal/auth/magic" .internal request,
  route "GET" "/auth/magic" .«public» consume,
  route "POST" "/auth/code" .«public» code
]

end Server.Auth.Magic
