import Lean.Data.Json
import Core.Ulid
import Server.Gmail.Client
import Server.Ingest
import Server.Spool

/-! # Gmail: one message into the vault

What the poll and the backfill share. A message already held under its Gmail id is a duplicate
without a fetch. Otherwise its raw bytes (`format=raw`) are written to the store as
`raw/<acct>/<ulid>` (the ULID from Gmail's `internalDate`) and go through `Ingest.ingest` with
source `gmail` (the poll: new mail) or `import` (the backfill: old mail, which never pushes); the row then gets `gm_msgid` and `gm_labels`. A message without `INBOX` is stored
done (`archived_at` = its date), `SPAM` also as junk, `SENT` without `INBOX` in the Sent folder.
Drafts are skipped. A duplicate by Message-ID (the same mail routed here too) gets the Gmail id
if it has none, so the mirror can find it. -/

namespace Server.Gmail

open Lean Server.Db

inductive Taken where
  | stored (id : String)
  | duplicate (id : String)
  | draft
  | failed (stage code : String)
  deriving Repr, BEq, Inhabited

def Taken.isFailed : Taken → Bool | .failed _ _ => true | _ => false

/-- The row already holding this Gmail id. -/
def heldId? (st : State) (gmId : String) : IO (Option String) := do
  return (← st.db.queryOne "SELECT id FROM messages WHERE gm_msgid = ?" #[gmId]).map (·.text "id")

/-- Fetch one message's raw form; failures as their code. -/
def fetchOne (st : State) (tok gmId : String) : IO (Except String RawMessage) := do
  try return .ok (← rawMessage st tok gmId)
  catch e => return .error (codeOf e)

structure TakeOpts where
  /-- `gmail` for live mail; the backfill stores as `import` (old mail: never pushed). -/
  source : String := "gmail"
  expiresAt : Option Nat := none
  keepForever : Bool := false

/-- Store a fetched message. Throws only when storage itself fails. -/
def storeFetched (st : State) (gmId : String) (m : RawMessage) (o : TakeOpts := {}) : IO Taken := do
  if m.labels.contains "DRAFT" then return .draft
  if let some id ← heldId? st gmId then return .duplicate id
  let now ← Clock.nowMs
  let receivedAt := m.internalDate.getD now
  let id := Core.Ulid.encode receivedAt (← Crypto.randomBytes 10)
  let key := Spool.rawKey st.config.accountId id
  st.store.put key m.raw [("source", o.source), ("received_at", toString receivedAt), ("size", toString m.raw.size)]
  let inbox := m.labels.contains "INBOX"
  let sent := m.labels.contains "SENT" && !inbox
  let labelsJson := (strArr m.labels.toList).compress
  let res ← Ingest.ingest st {
    raw := m.raw, key := some key, rcpt := none, source := o.source, receivedAt, forcedId := some id
    folder := if sent then "sent" else "inbox"
    archivedAt := if inbox || sent then none else some receivedAt
    junkKind := if m.labels.contains "SPAM" then some "spam" else none
    expiresAt := o.expiresAt, keepForever := o.keepForever }
  match res with
  | .stored i | .blocked i =>
    st.db.exec "UPDATE OR IGNORE messages SET gm_msgid = ?, gm_labels = ? WHERE id = ?" #[gmId, labelsJson, i]
    return .stored i
  | .duplicate twin =>
    if twin != id then st.store.delete key
    st.db.exec "UPDATE OR IGNORE messages SET gm_msgid = ?, gm_labels = ? WHERE id = ? AND gm_msgid IS NULL" #[gmId, labelsJson, twin]
    return .duplicate twin

/-- Fetch and store, every failure as a `Taken.failed`. -/
def take (st : State) (tok gmId : String) (o : TakeOpts := {}) : IO Taken := do
  if let some id ← heldId? st gmId then return .duplicate id
  match ← fetchOne st tok gmId with
  | .error code => return .failed "fetch" code
  | .ok m =>
    try storeFetched st gmId m o
    catch e =>
      Log.warn "gmail.ingest_failed" [("error", Json.str (codeOf e))]
      return .failed "ingest" "ingest"

/-- Write down a message that could not be taken (the poll's job id is `poll`). -/
def recordFailure (st : State) (jobId gmId stage code : String) : IO Unit := do
  let now ← Clock.nowMs
  st.db.exec "INSERT INTO history_failures (gm_msgid, job_id, stage, code, attempts, first_at, last_at)
    VALUES (?, ?, ?, ?, 1, ?, ?)
    ON CONFLICT(gm_msgid) DO UPDATE SET job_id = excluded.job_id, stage = excluded.stage, code = excluded.code,
      attempts = history_failures.attempts + 1, last_at = excluded.last_at"
    #[gmId, jobId, stage, String.ofList (code.toList.take 60), now, now]

end Server.Gmail
