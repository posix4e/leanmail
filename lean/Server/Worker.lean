import Lean.Data.Json
import Server.State
import Server.Http
import Server.Log
import Server.Db

namespace Server.Worker

/-! # The Worker's internal API

What only a Worker binding can do (APNs, Email Sending, the D1 projections) the container asks
of the Worker at `WORKER_ORIGIN`. Every call carries `X-Internal-Secret` (this container's
derived secret) and `x-account-id` (this container's account), which the Worker checks
together (`worker/src/internal.ts`, `authenticatedCaller`). -/

open Lean Server.Db

/-- The real client: HTTPS over libcurl to `{WORKER_ORIGIN}{path}`. -/
def http (c : Config) : WorkerClient where
  call method path json := do
    try
      let r ← Server.Http.request {
        method, url := c.workerOrigin ++ path, timeoutMs := 10000
        headers := [("Content-Type", "application/json"), ("X-Internal-Secret", c.internalSecret),
                    ("x-account-id", c.accountId)]
        body := json.compress.toUTF8 }
      if !r.isOk then
        Log.warn "worker.internal_status" [("path", Json.str path), ("status", Json.num r.status.toNat)]
        return none
      return some ((Json.parse (String.fromUTF8? r.body |>.getD "")).toOption.getD Json.null)
    catch e =>
      Log.warn "worker.internal_failed" [("path", Json.str path), ("error", Json.str (toString e))]
      return none

/-- `method` (POST unless told) `json` to the Worker's `path`; the reply, or `none` on failure. -/
def internal (st : State) (path : String) (json : Json) (method : String := "POST") : IO (Option Json) :=
  st.worker.call method path json

/-- What the phone shows: the sender's name (else address) and the subject, each cut short so
the APNs payload stays well under its 4 KiB. -/
def pushPayload (id fromName fromAddr subject : String) : Json :=
  let from_ := if fromName.isEmpty then fromAddr else fromName
  Json.mkObj [("id", Json.str id), ("from", Json.str (String.ofList (from_.toList.take 120))),
    ("subject", Json.str (String.ofList (subject.toList.take 200)))]

/-- The device tokens Apple said are dead, from the Worker's `{sent, dead, failed}`. -/
def deadTokens (j : Json) : List String :=
  ((j.getObjValAs? (Array Json) "dead").toOption.getD #[]).toList.filterMap fun
    | .str s => some s
    | _ => none

/-- A new message for the phone: `POST /internal/push {tokens:[{token, env}], payload:{id, from,
subject}}` to the Worker, which holds the APNs key; the tokens it answers `dead` are forgotten.
Nothing is sent when no phone is registered. Never throws. -/
def push (st : State) (id : String) : IO Unit := do
  try
    let toks ← st.db.query "SELECT token, env FROM push_tokens ORDER BY last_seen_at DESC LIMIT 20"
    if toks.isEmpty then return
    let some m ← st.db.queryOne "SELECT from_name, from_addr, subject FROM messages WHERE id = ?" #[id] | return
    let body := Json.mkObj [
      ("tokens", Json.arr (toks.map fun t => Json.mkObj [("token", Json.str (t.text "token")), ("env", Json.str (t.text "env" "prod"))])),
      ("payload", pushPayload id (m.text "from_name") (m.text "from_addr") (m.text "subject"))]
    match ← internal st "/internal/push" body with
    | none => Log.warn "push.failed" [("devices", Json.num toks.size)]
    | some r =>
      let dead := deadTokens r
      for d in dead do st.db.exec "DELETE FROM push_tokens WHERE token = ?" #[d]
      Log.info "push.sent" [("devices", Json.num toks.size), ("sent", (r.getObjVal? "sent").toOption.getD Json.null),
        ("dead", Json.num dead.length)]
  catch e => Log.warn "push.error" [("error", Json.str (toString e))]

end Server.Worker
