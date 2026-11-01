import Lean.Data.Json
import Server.Router.Types
import Server.Gmail.Client
import Server.Gmail.OAuth
import Server.Gmail.Take
import Server.Gmail.Poll
import Server.Gmail.Backfill
import Server.Gmail.Mirror
import Server.Gmail.Spam
import Server.Gmail.Watch
import Server.Gmail.SendAs
import Server.Jobs

/-! # Gmail: the routes

| route | auth | |
|---|---|---|
| `GET /api/gmail` | token `read` (or a session) | the connection, the lanes, the backfill, the Worker hook URL |
| `POST /api/gmail/client {client_id, client_secret}` | session | the owner's OAuth client |
| `GET /api/gmail/authorize` | session | 302 to Google's consent screen |
| `GET /oauth/google/callback?code&state` | public (the state is MAC'd) | 302 `/settings/gmail?connected=1` |
| `DELETE /api/gmail` | session | forget the connection |
| `PUT /api/gmail/prefs {mirror_archive?, report_spam?, backfill_spam_trash?, pubsub_topic?}` | session | the switches |
| `GET /api/gmail/backfill` | token `read` | the backfill's progress |
| `POST /api/gmail/backfill {keep_forever?}` | token `act` | start one |
| `POST /api/gmail/backfill/pause` · `resume` · `cancel` · `retry-failed` | token `act` | |
| `POST /api/gmail/watch` | session | `users.watch` on the Pub/Sub topic |
| `POST /internal/gmail/push`, `POST /internal/tick` | internal | `Server.Jobs` |

Errors from Google's side answer 400 when it is the owner's to set up (`no_client`,
`not_authorized`, `no_topic`), 409 when Google refused the grant or scope, 502 otherwise; never
401, which would read as this app's own session ending. -/

namespace Server.Gmail

open Lean Server.Db Server.Router

def googleStatus (code : String) : Nat :=
  if ["no_client", "not_authorized", "no_topic", "bad_action"].contains code then 400
  else if ["refresh_rejected", "insufficient_scope", "forbidden", "already_running"].contains code then 409
  else if code == "not_found" then 404
  else 502

def codeResp (code : String) : Resp := Resp.error (googleStatus code) code

def lanesJson (st : State) : IO Json := do
  let rows ← st.db.query "SELECT name, lease_until, last_run_at, last_error FROM jobs WHERE name LIKE 'gmail.%' ORDER BY name"
  return Json.mkObj (rows.toList.map fun r => (r.text "name", Json.mkObj [
    ("last_run_at", optNum (r.nat? "last_run_at")), ("leased", Json.bool (r.nat? "lease_until").isSome),
    ("last_error", optStr (r.text? "last_error"))]))

/-- Everything the settings page and `GET /api/gmail` show. -/
def statusJson (st : State) : IO Json := do
  let c ← conn? st
  let now ← Clock.nowMs
  let failures := ((← st.db.queryOne "SELECT count(*) AS n FROM history_failures").map (·.nat "n")).getD 0
  let job ← (do match ← Backfill.active? st with | some r => pure (some r) | none => Backfill.latest? st)
  return Json.mkObj [
    ("client_configured", Json.bool c.isSome),
    ("client_id", optStr (c.map (·.clientId))),
    ("connected", Json.bool ((c.map (·.authorized)).getD false)),
    ("email", optStr (c.bind (·.email))),
    ("needs_reauth", Json.bool ((c.map (·.needsReauth)).getD false)),
    ("last_error", optStr (c.bind (·.lastError))),
    ("history_id", optStr (c.bind (·.historyId))),
    ("last_poll_at", optNum (c.bind (·.lastPollAt))),
    ("last_push_at", optNum (c.bind (·.lastPushAt))),
    ("watch_expiration", optNum (c.bind (·.watchExpiration))),
    ("watch_active", Json.bool ((c.map (Watch.watchActive · now)).getD false)),
    ("topic", optStr (← Watch.topic? st)),
    ("push_url", optStr (Watch.hookUrl st.config)),
    ("redirect_uri", Json.str (OAuth.redirectUri st.config)),
    ("scopes", strArr scopes),
    ("send_as", strArr (← SendAs.identities st)),
    ("prefs", Json.mkObj [
      ("mirror_archive", Json.bool (← prefOn st prefMirror true)),
      ("report_spam", Json.bool (← prefOn st prefSpam true)),
      ("backfill_spam_trash", Json.bool (← prefOn st prefSpamTrash true))]),
    ("backfill", Backfill.view job),
    ("failures", Json.num failures),
    ("mirror", ← Mirror.counts st "gmail_mirror"),
    ("spam", ← Mirror.counts st "gmail_spam"),
    ("lanes", ← lanesJson st),
    ("next_wake", optNum (← Jobs.nextWake st))]

def statusGet (c : Ctx) (_ : Req) : IO Resp := return Resp.ok (← statusJson c.st)

/-- Apply the switches in a JSON object (absent keys are left alone). -/
def applyPrefs (st : State) (j : Json) : IO Unit := do
  for (k, p) in [("mirror_archive", prefMirror), ("report_spam", prefSpam), ("backfill_spam_trash", prefSpamTrash)] do
    if let some b := jbool? j k then setPref st p b
  if let some t := jstr? j "pubsub_topic" then
    let t := Core.Text.Str.trim t
    if t.isEmpty then delSetting st Watch.topicKey else setSetting st Watch.topicKey t

def prefsPut (c : Ctx) (req : Req) : IO Resp := do
  let j := req.json
  if j.isNull then return Resp.error 400 "bad json"
  applyPrefs c.st j
  return Resp.ok (← statusJson c.st)

def backfillGet (c : Ctx) (_ : Req) : IO Resp := do
  let job ← (do match ← Backfill.active? c.st with | some r => pure (some r) | none => Backfill.latest? c.st)
  return Resp.ok (Json.mkObj [("job", Backfill.view job)])

def backfillPost (c : Ctx) (req : Req) : IO Resp := do
  let keep := (jbool? req.json "keep_forever").getD false
  match ← (try Backfill.start c.st keep catch e => pure (.error (codeOf e))) with
  | .error code => return codeResp code
  | .ok row => return Resp.ok (Json.mkObj [("job", Backfill.view (some row))])

def backfillAction (c : Ctx) (_ : Req) : IO Resp := do
  match c.param "action" with
  | "retry-failed" =>
    try return Resp.ok (← Backfill.retryFailed c.st)
    catch e => return codeResp (codeOf e)
  | a =>
    match ← Backfill.setState c.st a with
    | .error code => return codeResp code
    | .ok () => return Resp.ok (Json.mkObj [("job", Backfill.view (← Backfill.latest? c.st))])

def watchPost (c : Ctx) (_ : Req) : IO Resp := do
  match ← Watch.start c.st with
  | .error code => return codeResp code
  | .ok j => return Resp.ok j

def routes : Array Route := #[
  route "GET" "/api/gmail" (.token "read") statusGet,
  route "DELETE" "/api/gmail" .session OAuth.deleteConn,
  route "POST" "/api/gmail/client" .session OAuth.clientPost,
  route "GET" "/api/gmail/authorize" .session OAuth.authorizeGet,
  route "GET" "/oauth/google/callback" .«public» OAuth.callback,
  route "PUT" "/api/gmail/prefs" .session prefsPut,
  route "GET" "/api/gmail/backfill" (.token "read") backfillGet,
  route "POST" "/api/gmail/backfill" (.token "act") backfillPost,
  route "POST" "/api/gmail/backfill/:action" (.token "act") backfillAction,
  route "POST" "/api/gmail/watch" .session watchPost
] ++ Jobs.routes

end Server.Gmail
