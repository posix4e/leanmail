import Lean.Data.Json
import Server.Router.Types
import Server.Auth.Session
import Server.Auth.ApiTokens
import Server.Operator
import Server.Log

/-! # Routing

`authorize` is the one place a route's `Auth` is checked:

1. `x-account-id` (stamped by the account's Durable Object on everything it forwards) must equal
   `ACCOUNT_ID`: another value is 403; none is 401 unless this is a bare localhost run with no
   Worker (`Config.bareDev`).
2. `DEV_AUTH_BYPASS` (localhost origins only): a request carrying `x-account-id` is an admin
   session for `.session`, `.token` and `.admin`.
3. `.public` needs nothing more; `.internal` needs `X-Internal-Secret` (constant time);
   `.session` the `lm_session` cookie plus CSRF on mutations (`Server.Auth.Session`);
   `.token s` a bearer `lm1…` token with scope `s` (`Server.Auth.ApiTokens`) or a session;
   `.admin` the Worker's `x-admin-account` with a valid internal secret, or an admin's own
   session (`Server.Operator`), operator container only. -/

namespace Server.Router

open Lean

abbrev Granted := Principal × List (String × String)

/-- The principal the request may act as, plus headers to add to the response (a sliding
session's new cookie), or the refusal. -/
def authorize (st : State) (auth : Auth) (req : Req) : IO (Except Resp Granted) := do
  let cfg := st.config
  match req.header? "x-account-id" with
  | some a => if a != cfg.accountId then return .error (Resp.error 403 "wrong account")
  | none => if !cfg.bareDev then return .error (Resp.error 401 "unauthorized")
  let dev := cfg.devBypassOn && (req.header? "x-account-id").isSome
  match auth with
  | .«public» => return .ok (.anon, [])
  | .internal =>
    return if internalOk st req then .ok (.internal, []) else .error (Resp.error 401 "unauthorized")
  | .session =>
    if dev then return .ok (.dev, [])
    Auth.Session.fromCookie st req
  | .token scope =>
    if dev then return .ok (.dev, [])
    if (req.header? "authorization").isSome then
      return (← Auth.ApiTokens.fromBearer st req scope).map ((·, []))
    Auth.Session.fromCookie st req
  | .admin =>
    if !cfg.isOperator then return .error Resp.notFound
    if dev then return .ok (.admin "dev", [])
    match req.header? "x-admin-account" with
    | some acct =>
      if internalOk st req && (← Operator.isAdminAccount st acct) then return .ok (.admin acct, [])
      return .error (Resp.error 403 "forbidden")
    | none =>
      match ← Auth.Session.fromCookie st req with
      | .ok (_, hs) =>
        if ← Operator.selfIsAdmin st then return .ok (.admin cfg.accountId, hs)
        return .error (Resp.error 403 "forbidden")
      | .error r => return .error r

def dispatch (st : State) (routes : Array Route) (req : Req) : IO Resp := do
  let candidates := routes.filterMap fun r => (matchPath r.pattern req.path).map (r, ·)
  match candidates.find? (·.1.method == req.method) with
  | none => return if candidates.isEmpty then Resp.notFound else Resp.error 405 "method not allowed"
  | some (r, params) =>
    match ← authorize st r.auth req with
    | .error resp => return resp
    | .ok (principal, extra) =>
      try
        let resp ← r.handler { st, account := st.config.accountId, auth := r.auth, params, principal } req
        -- the handler's own Set-Cookie (a logout's clear) comes last and wins
        return { resp with headers := extra ++ resp.headers }
      catch e =>
        Log.error "http.handler_failed" [("path", Json.str ("/" ++ "/".intercalate req.path)), ("error", Json.str (toString e))]
        return Resp.error 500 "internal error"

end Server.Router
