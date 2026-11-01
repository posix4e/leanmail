import Lean.Data.Json
import Server.Gmail.Client
import Server.Gmail.OAuth

/-! # Gmail: push over Pub/Sub

`POST /api/gmail/watch` asks Gmail (`users.watch`) to publish the mailbox's changes to the
owner's Pub/Sub topic: `GOOGLE_PUBSUB_TOPIC` from the environment, or `gmail.pubsub_topic` in
`settings` (the settings page sets it). A watch lasts seven days; the daily tick renews it once it
is within two days of expiring.

The topic's push subscription points at the Worker's `POST /hooks/gmail/<acct>/<sig>` with

    sig = base64url(HMAC-SHA256(INTERNAL_SECRET, "gmail-push:" + acct))

where `INTERNAL_SECRET` is this container's (derived per account by the Worker,
`worker/src/gmail.ts`), so the Worker can check it without asking anyone and no account can mint
another's. The Worker forwards `{emailAddress, historyId}` to `POST /internal/gmail/push`, which
notes the push and runs the poll lane: a notification carries no mail, so a forged one can at most
cause a poll. -/

namespace Server.Gmail.Watch

open Lean Server.Db Server.Gmail

def renewWithinMs : Nat := 2 * 86400000
def topicKey : String := "gmail.pubsub_topic"

def topic? (st : State) : IO (Option String) := do
  match ← setting? st topicKey with
  | some t => if t.isEmpty then pure none else pure (some t)
  | none =>
    match ← IO.getEnv "GOOGLE_PUBSUB_TOPIC" with
    | some t => pure (if t.isEmpty then none else some t)
    | none => pure none

def hookSig (c : Config) : Option String :=
  if c.internalSecret.isEmpty then none
  else some (Crypto.base64Url (Crypto.hmacText c.internalSecret ("gmail-push:" ++ c.accountId)))

/-- The URL the Pub/Sub push subscription must call. -/
def hookUrl (c : Config) : Option String :=
  (hookSig c).map fun s => s!"{c.origin}/hooks/gmail/{c.accountId}/{s}"

def watchActive (c : Conn) (now : Nat) : Bool := (c.watchExpiration.map (fun e => decide (e > now))).getD false

/-- Start (or renew) the watch: `{history_id, expiration}`, or the reason it could not. -/
def start (st : State) : IO (Except String Json) := do
  let some topic ← topic? st | return .error "no_topic"
  let some _ ← usable? st | return .error "not_authorized"
  try
    let j ← watch st (← OAuth.accessToken st) topic
    let exp := jNat? j "expiration"
    let hid := jStrNum? j "historyId"
    -- seed the cursor only the first time: overwriting it would skip what the poll has not read
    updateConn st "watch_expiration = ?, history_id = COALESCE(history_id, ?)" #[(exp : Value), (hid : Value)]
    Log.info "gmail.watch" [("renewed", Json.bool true)]
    return .ok (Json.mkObj [("history_id", optStr hid), ("expiration", optNum exp)])
  catch e => return .error (codeOf e)

/-- Renew when there is a topic and the watch is gone or within two days of expiring. -/
def renewIfDue (st : State) (now : Nat) : IO Bool := do
  let some c ← usable? st | return false
  if (← topic? st).isNone then return false
  if let some e := c.watchExpiration then
    if e > now + renewWithinMs then return false
  match ← start st with
  | .ok _ => return true
  | .error code =>
    Log.warn "gmail.watch_failed" [("code", Json.str code)]
    return false

/-- A push arrived: note it. `true` when the poll should run now. -/
def notePush (st : State) (email : Option String) (historyId : Option String) : IO Bool := do
  let some c ← usable? st | return false
  if let some e := email then
    if c.email.isSome && c.email != some e.toLower then
      Log.warn "gmail.push_ignored" [("reason", Json.str "other_mailbox")]
      return false
  let now ← Clock.nowMs
  updateConn st "last_push_at = ?" #[.int now.toInt64]
  if c.historyId.isNone then
    -- no cursor yet: adopt this one rather than replaying the mailbox
    if let some h := historyId then updateConn st "history_id = ?" #[.text h]
    return false
  return true

end Server.Gmail.Watch
