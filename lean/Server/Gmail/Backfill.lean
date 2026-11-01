import Lean.Data.Json
import Core.Ulid
import Server.Gmail.Client
import Server.Gmail.OAuth
import Server.Gmail.Take
import Server.Gmail.Poll

/-! # Gmail: the backfill lane

A port of MailVault's `stepApiHistory` (`src/gmail/backfill.ts`): one `history_jobs` row walks
the whole mailbox with `users.messages.list`, 25 ids a page, the opaque page token as the only
cursor. Each step works for at most 240 s under the `gmail.backfill` lease (270 s, so a killed
step frees the lane before the next one); the ids of a page are fetched six at a time
(`IO.asTask`) and stored one by one. A page is committed (token and counts) only when every id on
it was dealt with; a page cut short by the budget or by Google is listed again next step, and
what it already stored comes back as duplicates.

A message that cannot be read or stored is counted and written to `history_failures` under the
job's id. Rate limits, 5xx and transport failures stop the page instead (`transient`). The job
pauses on what needs a person (`needsAPerson`: the grant refused, consent revoked, a narrower
scope) or after five steps in a row that failed; anything else retries next step.

It stores as source `import` (old mail: no push to the phone), with `gm_msgid` like the poll.
Retention counts from the day the backfill started (`expires_at`), never from a message's own
date, so old mail is not expired on arrival; `keep_forever` keeps it all. -/

namespace Server.Gmail.Backfill

open Lean Server.Db Server.Gmail

def pageSize : Nat := 25
def lanes : Nat := 6
def stepBudgetMs : Nat := 240000
def leaseMs : Nat := 270000
def stallLimit : Nat := 5

def active? (st : State) : IO (Option Row) :=
  st.db.queryOne "SELECT * FROM history_jobs WHERE state IN ('running','paused') ORDER BY started_at DESC LIMIT 1"

def latest? (st : State) : IO (Option Row) :=
  st.db.queryOne "SELECT * FROM history_jobs ORDER BY started_at DESC LIMIT 1"

def running? (st : State) : IO Bool := do
  return (← st.db.queryOne "SELECT 1 AS x FROM history_jobs WHERE state = 'running' LIMIT 1").isSome

/-- Seen against the mailbox's own count, in tenths of a percent. -/
def percentTenths (r : Row) : Nat :=
  match r.nat? "planned_total" with
  | some t => if t == 0 then 0 else Nat.min 1000 (r.nat "seen" * 1000 / t)
  | none => 0

def view : Option Row → Json
  | none => Json.null
  | some r => Json.mkObj [
      ("id", Json.str (r.text "id")), ("state", Json.str (r.text "state")),
      ("percent", Json.num (JsonNumber.mk (percentTenths r) 1)),
      ("planned_total", optNum (r.nat? "planned_total")), ("seen", Json.num (r.nat "seen")),
      ("imported", Json.num (r.nat "imported")), ("duplicates", Json.num (r.nat "duplicates")),
      ("skipped", Json.num (r.nat "skipped")), ("oversize", Json.num (r.nat "oversize")),
      ("failed", Json.num (r.nat "failed")), ("bytes", Json.num (r.nat "bytes")),
      ("keep_forever", Json.bool (r.bool "keep_forever")), ("expires_at", optNum (r.nat? "expires_at")),
      ("error_code", optStr (r.text? "error_code")), ("has_page_token", Json.bool (r.text? "page_token").isSome),
      ("started_at", Json.num (r.nat "started_at")), ("updated_at", Json.num (r.nat "updated_at")),
      ("finished_at", optNum (r.nat? "finished_at"))]

private def retentionDays (st : State) : IO Nat := do
  let r ← st.db.queryOne "SELECT retention_days FROM account WHERE id = 1"
  return (r.bind (·.nat? "retention_days")).getD st.config.retentionDaysDefault

/-- Start a backfill: `already_running`, `no_client` or `not_authorized` when it cannot. -/
def start (st : State) (keepForever : Bool) : IO (Except String Row) := do
  if (← active? st).isSome then return .error "already_running"
  let some c ← conn? st | return .error "no_client"
  if !c.authorized || c.needsReauth then return .error "not_authorized"
  let total : Option Nat ← (try (do
      let tok ← OAuth.accessToken st
      pure (jNat? (← profile st tok) "messagesTotal")) catch _ => pure none)
  let now ← Clock.nowMs
  let id := Core.Ulid.encode now (← Crypto.randomBytes 10)
  let expiresAt : Option Nat ← if keepForever then pure none else do pure (some (now + (← retentionDays st) * 86400000))
  st.db.exec "INSERT INTO history_jobs (id, state, page_token, planned_total, expires_at, keep_forever, started_at, updated_at)
    VALUES (?, 'running', NULL, ?, ?, ?, ?, ?)" #[id, (total : Value), (expiresAt : Value), keepForever, now, now]
  Log.info "gmail.backfill_start" [("total", Json.num (total.getD 0)), ("keep_forever", Json.bool keepForever)]
  return .ok ((← st.db.queryOne "SELECT * FROM history_jobs WHERE id = ?" #[id]).getD default)

/-- `pause` (a running job), `resume` (a paused one, its stall count reset), `cancel`. -/
def setState (st : State) (action : String) : IO (Except String Unit) := do
  let some j ← active? st | return .error "not_found"
  let now ← Clock.nowMs
  match action with
  | "pause" => st.db.exec "UPDATE history_jobs SET state = 'paused', updated_at = ? WHERE id = ?" #[now, j.text "id"]
  | "resume" => st.db.exec "UPDATE history_jobs SET state = 'running', stall_count = 0, error_code = NULL, updated_at = ? WHERE id = ?" #[now, j.text "id"]
  | "cancel" => st.db.exec "UPDATE history_jobs SET state = 'cancelled', finished_at = ?1, updated_at = ?1 WHERE id = ?2" #[now, j.text "id"]
  | _ => return .error "bad_action"
  Log.info "gmail.backfill_state" [("action", Json.str action)]
  return .ok ()

structure Counts where
  seen : Nat := 0
  imported : Nat := 0
  duplicates : Nat := 0
  skipped : Nat := 0
  oversize : Nat := 0
  failed : Nat := 0
  bytes : Nat := 0
  deriving Inhabited

def Counts.add (a b : Counts) : Counts :=
  { seen := a.seen + b.seen, imported := a.imported + b.imported, duplicates := a.duplicates + b.duplicates,
    skipped := a.skipped + b.skipped, oversize := a.oversize + b.oversize, failed := a.failed + b.failed,
    bytes := a.bytes + b.bytes }

/-- Advance the running job by one bounded slice. The code it stopped on, if any. The caller
holds the lane's lease (`Jobs.tick`). -/
def step (st : State) (budgetMs : Nat := stepBudgetMs) (clock : IO Nat := IO.monoMsNow) : IO (Option String) := do
  let some job ← active? st | return none
  if job.text "state" != "running" then return none
  let jobId := job.text "id"
  let opts : TakeOpts := { source := "import", expiresAt := job.nat? "expires_at", keepForever := job.bool "keep_forever" }
  let spamTrash ← prefOn st prefSpamTrash true
  let deadline := (← clock) + budgetMs
  let mut pageToken : Option String := job.text? "page_token"
  let start0 : Counts := {
    seen := job.nat "seen", imported := job.nat "imported", duplicates := job.nat "duplicates"
    skipped := job.nat "skipped", oversize := job.nat "oversize", failed := job.nat "failed", bytes := job.nat "bytes" }
  let mut total := start0
  let mut stalls := job.nat "stall_count"
  let mut errorCode : Option String := none
  let mut exhausted := false
  let persist (tok : Option String) (c : Counts) (stalls : Nat) : IO Unit := do
    st.db.exec "UPDATE history_jobs SET page_token = ?, seen = ?, imported = ?, duplicates = ?, skipped = ?, oversize = ?,
      failed = ?, bytes = ?, stall_count = ?, error_code = NULL, updated_at = ? WHERE id = ?"
      #[(tok : Value), c.seen, c.imported, c.duplicates, c.skipped, c.oversize, c.failed, c.bytes, stalls, ← Clock.nowMs, jobId]
  try
    let tok ← OAuth.accessToken st
    let mut lastPageMs := 0
    while (← clock) < deadline do
      if lastPageMs > 0 && (← clock) + lastPageMs > deadline then break
      let started ← clock
      let page ← listMessages st tok pageToken pageSize none spamTrash
      let mut pc : Counts := {}
      let mut whole := true
      let mut i := 0
      while i < page.ids.size do
        if (← clock) > deadline then
          whole := false
          break
        let group := page.ids.extract i (i + lanes)
        i := i + lanes
        -- held already: no fetch
        let mut todo : Array String := #[]
        for id in group do
          if (← heldId? st id).isSome then
            pc := { pc with seen := pc.seen + 1, duplicates := pc.duplicates + 1 }
          else
            todo := todo.push id
        let tasks ← todo.mapM fun id => IO.asTask (fetchOne st tok id) Task.Priority.dedicated
        let mut stop : Option String := none
        for h : k in [:todo.size] do
          let id := todo[k]
          let fetched ← match ← IO.wait tasks[k]! with
            | .ok r => pure r
            | .error e => pure (Except.error (codeOf e))
          match fetched with
          | .error code =>
            if transient code || needsAPerson code then
              if stop.isNone then stop := some code
            else
              pc := { pc with seen := pc.seen + 1, failed := pc.failed + 1 }
              recordFailure st jobId id "fetch" code
          | .ok m =>
            if stop.isNone then
              pc := { pc with seen := pc.seen + 1 }
              if m.raw.size > Ingest.parseMax then pc := { pc with oversize := pc.oversize + 1 }
              match ← (try (some <$> storeFetched st id m opts) catch _ => pure none) with
              | some (.stored _) => pc := { pc with imported := pc.imported + 1, bytes := pc.bytes + m.raw.size }
              | some (.duplicate _) => pc := { pc with duplicates := pc.duplicates + 1 }
              | some .draft => pc := { pc with skipped := pc.skipped + 1 }
              | some (.failed _ _) => pc := { pc with failed := pc.failed + 1 }
              | none => stop := some "storage"
        if let some code := stop then
          errorCode := some code
          whole := false
          break
      if !whole then break
      total := total.add pc
      lastPageMs := (← clock) - started
      pageToken := page.nextPageToken
      stalls := 0
      persist pageToken total stalls
      if pageToken.isNone then
        exhausted := true
        break
  catch e =>
    errorCode := some (codeOf e)
  persist pageToken total stalls
  let now ← Clock.nowMs
  if exhausted then
    st.db.exec "UPDATE history_jobs SET state = 'done', finished_at = ?1, updated_at = ?1 WHERE id = ?2" #[now, jobId]
    Log.info "gmail.backfill_done" [("imported", Json.num total.imported), ("failed", Json.num total.failed)]
    return none
  if let some code := errorCode then
    let stalls' := stalls + 1
    let pause := needsAPerson code || stalls' ≥ stallLimit
    st.db.exec "UPDATE history_jobs SET state = ?, error_code = ?, stall_count = ?, updated_at = ? WHERE id = ?"
      #[if pause then "paused" else "running", code, stalls', now, jobId]
    Log.warn "gmail.backfill_interrupted" [("code", Json.str code), ("paused", Json.bool pause), ("stalls", Json.num stalls')]
  return errorCode

/-- Try recorded failures again, the least tried first: `{stored, already, failed, remaining}`. -/
def retryFailed (st : State) (limit : Nat := 10) : IO Json := do
  let rows ← st.db.query "SELECT gm_msgid, job_id FROM history_failures ORDER BY attempts ASC, last_at ASC LIMIT ?" #[Nat.min 50 (Nat.max 1 limit)]
  let mut stored := 0
  let mut already := 0
  let mut failed := 0
  if !rows.isEmpty then
    let tok ← OAuth.accessToken st
    for r in rows do
      let job ← st.db.queryOne "SELECT expires_at, keep_forever FROM history_jobs WHERE id = ?" #[r.text "job_id"]
      let o : TakeOpts := match job with
        | some j => { source := "import", expiresAt := j.nat? "expires_at", keepForever := j.bool "keep_forever" }
        | none => {}
      match ← Poll.retryOne st tok (r.text "gm_msgid") o with
      | "stored" => stored := stored + 1
      | "failed" => failed := failed + 1
      | _ => already := already + 1
  let left := ((← st.db.queryOne "SELECT count(*) AS n FROM history_failures").map (·.nat "n")).getD 0
  return Json.mkObj [("stored", Json.num stored), ("already", Json.num already), ("failed", Json.num failed),
    ("remaining", Json.num left)]

end Server.Gmail.Backfill
