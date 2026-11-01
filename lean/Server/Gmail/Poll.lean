import Lean.Data.Json
import Server.Gmail.Client
import Server.Gmail.OAuth
import Server.Gmail.Take

/-! # Gmail: the poll lane

A port of MailVault's `pollAccounts`/`advance` (`src/gmail/routes.ts`) and `failures.ts`.

1. No cursor yet: take the mailbox's current `historyId` from the profile and stop; the past is
   the backfill's.
2. Retry what earlier polls could not take (`history_failures` with job id `poll`), whose wait is
   over: it doubles from 15 minutes to 6 hours, and after 24 tries the row is left for the owner.
3. `users.history.list` from the cursor (message additions, up to 10 pages), take each new id
   (`Gmail.take`), write down each failure, and move the cursor. A walk cut short by the page cap
   moves it only as far as the last record read, never to the mailbox's current id.
4. A 404 for the cursor is a history gap (Gmail keeps about a week): take the profile's id now,
   queue every message since a day before the last poll that no row holds (`messages.list
   q=after:`), as failures with no attempt yet, and retry them at once. -/

namespace Server.Gmail.Poll

open Lean Server.Db Server.Gmail

def pollJob : String := "poll"
def retryBaseMs : Nat := 15 * 60000
def retryMaxMs : Nat := 6 * 3600000
def retryAttempts : Nat := 24
def retryPerPoll : Nat := 20
def historyPages : Nat := 10
def gapPages : Nat := 10
def gapMarginMs : Nat := 86400000

structure Run where
  stored : Nat := 0
  duplicates : Nat := 0
  failed : Nat := 0
  retried : Nat := 0
  queued : Nat := 0
  gap : Bool := false
  seeded : Bool := false
  deriving Repr, Inhabited

def Run.json (r : Run) : Json := Json.mkObj [
  ("stored", Json.num r.stored), ("duplicates", Json.num r.duplicates), ("failed", Json.num r.failed),
  ("retried", Json.num r.retried), ("queued", Json.num r.queued), ("gap", Json.bool r.gap), ("seeded", Json.bool r.seeded)]

private def forgetFailure (st : State) (gmId : String) : IO Unit :=
  st.db.exec "DELETE FROM history_failures WHERE gm_msgid = ?" #[gmId]

/-- One recorded failure, again: `stored`, `already` (held, a draft, or gone from Gmail) or
`failed` (one attempt older). -/
def retryOne (st : State) (tok gmId : String) (o : TakeOpts := {}) : IO String := do
  match ← take st tok gmId o with
  | .stored _ => forgetFailure st gmId; return "stored"
  | .duplicate _ | .draft => forgetFailure st gmId; return "already"
  | .failed stage code =>
    if stage == "fetch" && code == "http_404" then
      forgetFailure st gmId
      return "already"
    st.db.exec "UPDATE history_failures SET attempts = attempts + 1, last_at = ?, stage = ?, code = ? WHERE gm_msgid = ?"
      #[← Clock.nowMs, stage, String.ofList (code.toList.take 60), gmId]
    return "failed"

/-- The poll's recorded failures whose wait is over, oldest first, at most `limit`. -/
def retryDue (st : State) (tok : String) (now : Nat) (limit : Nat := retryPerPoll) : IO (Nat × Nat) := do
  let due ← st.db.query "SELECT gm_msgid FROM history_failures
    WHERE job_id = ?1 AND attempts < ?2 AND last_at + MIN(?3 * (1 << MIN(MAX(attempts - 1, 0), 5)), ?4) <= ?5
    ORDER BY last_at ASC LIMIT ?6" #[pollJob, retryAttempts, retryBaseMs, retryMaxMs, now, limit]
  let mut stored := 0
  let mut tried := 0
  for r in due do
    tried := tried + 1
    if (← retryOne st tok (r.text "gm_msgid")) == "stored" then stored := stored + 1
  return (tried, stored)

/-- Message ids added since `start`, and where the cursor may move. -/
def addedSince (st : State) (tok start : String) : IO (Array String × Option String) := do
  let mut ids : Array String := #[]
  let mut pageToken : Option String := none
  let mut mailboxNow : Option String := none
  let mut lastRecord : Option String := none
  for _ in [:historyPages] do
    let j ← historyPage st tok start pageToken
    mailboxNow := (jStrNum? j "historyId").orElse fun _ => mailboxNow
    for h in jArr j "history" do
      if let some hid := jStrNum? h "id" then lastRecord := some hid
      for m in jArr h "messagesAdded" do
        if let some id := (m.getObjVal? "message").toOption.bind (jStr? · "id") then
          if !ids.contains id then ids := ids.push id
    match jStr? j "nextPageToken" with
    | none => return (ids, mailboxNow)
    | some p => pageToken := some p
  -- out of pages with more to come: only as far as was read
  return (ids, lastRecord)

/-- Queue the mailbox's messages since `sinceMs` that no row holds, for the retry. -/
def queueGap (st : State) (tok : String) (sinceMs : Nat) : IO Nat := do
  let search := s!"after:{sinceMs / 1000}"
  let spamTrash ← prefOn st prefSpamTrash true
  let mut pageToken : Option String := none
  let mut queued := 0
  let now ← Clock.nowMs
  for _ in [:gapPages] do
    let page ← listMessages st tok pageToken 500 (some search) spamTrash
    for id in page.ids do
      if (← heldId? st id).isNone then
        st.db.exec "INSERT INTO history_failures (gm_msgid, job_id, stage, code, attempts, first_at, last_at)
          VALUES (?, ?, 'fetch', 'gap', 0, ?, 0) ON CONFLICT(gm_msgid) DO NOTHING" #[id, pollJob, now]
        queued := queued + 1
    match page.nextPageToken with
    | none => return queued
    | some p => pageToken := some p
  return queued

/-- A cursor Gmail no longer has: move to the mailbox's position now, after queueing the gap. -/
def recoverCursor (st : State) (tok : String) (c : Conn) (now : Nat) : IO Nat := do
  let some fresh := jStrNum? (← profile st tok) "historyId" | googleFail "no_history_id"
  let lastSeen := Nat.min now (c.lastPollAt.getD now)
  let queued ← queueGap st tok (lastSeen - gapMarginMs)
  updateConn st "history_id = ?" #[.text fresh]
  Log.info "gmail.history_gap" [("queued", Json.num queued)]
  return queued

/-- One run of the lane. -/
def poll (st : State) : IO Run := do
  let some c ← usable? st | return {}
  let tok ← OAuth.accessToken st
  let now ← Clock.nowMs
  let some cursor := c.historyId | do
    let hid := jStrNum? (← profile st tok) "historyId"
    updateConn st "history_id = COALESCE(history_id, ?), last_poll_at = ?" #[(hid : Value), .int now.toInt64]
    return { seeded := true }
  let mut run : Run := {}
  let (tried, again) ← retryDue st tok now
  run := { run with retried := tried, stored := again }
  let added ← (try (some <$> addedSince st tok cursor) catch e =>
    if codeOf e == "http_404" then pure none else throw e)
  match added with
  | none =>
    let q ← recoverCursor st tok c now
    let (tried2, again2) ← retryDue st tok now (limit := 500)
    run := { run with gap := true, queued := q, retried := run.retried + tried2, stored := run.stored + again2 }
  | some (ids, next) =>
    for id in ids do
      match ← take st tok id with
      | .stored _ => run := { run with stored := run.stored + 1 }
      | .duplicate _ => run := { run with duplicates := run.duplicates + 1 }
      | .draft => pure ()
      | .failed stage code =>
        run := { run with failed := run.failed + 1 }
        recordFailure st pollJob id stage code
    if let some n := next then updateConn st "history_id = ?" #[.text n]
  updateConn st "last_poll_at = ?" #[.int now.toInt64]
  Log.info "gmail.poll" [("stored", Json.num run.stored), ("duplicates", Json.num run.duplicates),
    ("failed", Json.num run.failed), ("gap", Json.bool run.gap)]
  return run

end Server.Gmail.Poll
