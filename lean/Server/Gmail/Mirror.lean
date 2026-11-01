import Lean.Data.Json
import Server.Gmail.Client
import Server.Gmail.OAuth
import Server.Gmail.Spam

/-! # Gmail: the archive mirror

A port of MailVault's `src/gmail/mirror.ts`, driven by a queue instead of a scan. With the
`gmail.mirror_archive` switch on (the default), marking a Gmail message done here takes `INBOX`
off it in Gmail (`users.messages.modify removeLabelIds:[INBOX]`), and undoing that (`inbox`, or
any action that clears `archived_at`) puts it back, but only where the mirror itself took it out.

`onAction` is the hook `Actions.apply` calls after every action: it reads the message once and
queues `gmail_mirror` rows (`removing`, `restoring`) and the spam lane's `gmail_spam` rows
(`Gmail.Spam.onAction`), then kicks the job loop. The lane (`step`, under the `gmail.mirror`
lease) reads each message's labels before removing anything: a message not in Gmail's inbox
(already archived there, Sent, filtered past it), in Trash or Spam, or gone, is `skipped`, and an
undo never adds `INBOX` to one the mirror did not take it from. Mail sent from here
(`source = 'sent'`) is never touched. A removal Google refuses for now stays queued; after 24
tries it is `failed`. -/

namespace Server.Gmail.Mirror

open Lean Server.Db Server.Gmail

def perTick : Nat := 100
def maxAttempts : Nat := 24

structure Run where
  archived : Nat := 0
  restored : Nat := 0
  skipped : Nat := 0
  failed : Nat := 0
  deriving Repr, Inhabited

def state? (st : State) (id : String) : IO (Option String) := do
  return (← st.db.queryOne "SELECT state FROM gmail_mirror WHERE message_id = ?" #[id]).map (·.text "state")

def setRow (st : State) (id state : String) (reason : Option String := none) : IO Unit := do
  st.db.exec "UPDATE gmail_mirror SET state = ?, reason = ?, updated_at = ? WHERE message_id = ?" #[state, (reason : Value), ← Clock.nowMs, id]

/-- The mirror's half of the hook. Whether anything was queued. -/
def queue (st : State) (action : String) (row : Row) : IO Bool := do
  let some gm := row.text? "gm_msgid" | return false
  if row.text "source" == "sent" then return false
  if !(← prefOn st prefMirror true) then return false
  let id := row.text "id"
  let cur ← state? st id
  match row.nat? "archived_at" with
  | some archivedAt =>
    if action != "done" then return false
    let some c ← conn? st | return false
    if !c.authorized then return false
    match cur with
    | some "archived" | some "removing" => return false
    | some "restoring" => setRow st id "archived"; return false
    | _ =>
      st.db.exec "INSERT INTO gmail_mirror (message_id, gm_msgid, archived_at, state, reason, attempts, updated_at)
        VALUES (?1, ?2, ?3, 'removing', NULL, 0, ?4)
        ON CONFLICT(message_id) DO UPDATE SET gm_msgid = excluded.gm_msgid, archived_at = excluded.archived_at,
          state = 'removing', reason = NULL, attempts = 0, updated_at = excluded.updated_at"
        #[id, gm, archivedAt, ← Clock.nowMs]
      return true
  | none =>
    match cur with
    | some "archived" => setRow st id "restoring"; return true
    | some "removing" => setRow st id "released"; return false
    | _ => return false

/-- After an action on message `id` (`Actions.apply`): queue what Gmail should be told, and wake
the job loop. Never fails the action. -/
def onAction (st : State) (id action : String) : IO Unit := do
  try
    let some row ← st.db.queryOne "SELECT id, gm_msgid, source, archived_at, junk_at FROM messages WHERE id = ? AND deleted = 0" #[id]
      | return
    if (row.text? "gm_msgid").isNone then return
    let a ← queue st action row
    let b ← Spam.onAction st action row
    if a || b then kick
  catch e =>
    Log.warn "gmail.hook_failed" [("error", Json.str (toString e))]

/-- One run: undos first, then removals. -/
def step (st : State) : IO Run := do
  let some _ ← usable? st | return {}
  if !(← prefOn st prefMirror true) then return {}
  let back ← st.db.query "SELECT message_id, gm_msgid FROM gmail_mirror WHERE state = 'restoring' LIMIT ?" #[perTick]
  let todo ← st.db.query "SELECT g.message_id, g.gm_msgid, g.attempts, m.archived_at AS live_archived, m.deleted AS gone
    FROM gmail_mirror g LEFT JOIN messages m ON m.id = g.message_id
    WHERE g.state = 'removing' LIMIT ?" #[perTick]
  if back.isEmpty && todo.isEmpty then return {}
  let tok ← OAuth.accessToken st
  let mut run : Run := {}
  for r in back do
    let id := r.text "message_id"
    if ← Spam.tryModify st tok (r.text "gm_msgid") ["INBOX"] [] then
      setRow st id "restored"; run := { run with restored := run.restored + 1 }
    else
      setRow st id "skipped" (some "gone"); run := { run with skipped := run.skipped + 1 }
  for r in todo do
    let id := r.text "message_id"
    let gm := r.text "gm_msgid"
    if (r.nat? "live_archived").isNone || r.bool "gone" then
      setRow st id "released"
      continue
    let attempts := r.nat "attempts" + 1
    if attempts > maxAttempts then
      setRow st id "failed" (some "attempts"); run := { run with failed := run.failed + 1 }
      continue
    st.db.exec "UPDATE gmail_mirror SET attempts = ? WHERE message_id = ?" #[attempts, id]
    match ← labelsOf st tok gm with
    | none => setRow st id "skipped" (some "gone"); run := { run with skipped := run.skipped + 1 }
    | some labels =>
      if labels.contains "TRASH" || labels.contains "SPAM" then
        setRow st id "skipped" (some "trash"); run := { run with skipped := run.skipped + 1 }
      else if !labels.contains "INBOX" then
        setRow st id "skipped" (some "not_in_inbox"); run := { run with skipped := run.skipped + 1 }
      else if ← Spam.tryModify st tok gm [] ["INBOX"] then
        setRow st id "archived"; run := { run with archived := run.archived + 1 }
      else
        setRow st id "skipped" (some "gone"); run := { run with skipped := run.skipped + 1 }
  Log.info "gmail.mirror" [("archived", Json.num run.archived), ("restored", Json.num run.restored),
    ("skipped", Json.num run.skipped), ("failed", Json.num run.failed)]
  return run

/-- Daily: forget rows for messages the vault no longer holds. -/
def prune (st : State) : IO Unit :=
  st.db.exec "DELETE FROM gmail_mirror WHERE NOT EXISTS (SELECT 1 FROM messages m WHERE m.id = gmail_mirror.message_id AND m.deleted = 0)"

/-- Counts by state, for the settings page. -/
def counts (st : State) (table : String) : IO Json := do
  let rows ← st.db.query s!"SELECT state, count(*) AS n FROM {table} GROUP BY state"
  return Json.mkObj (rows.toList.map fun r => (r.text "state", Json.num (r.nat "n")))

end Server.Gmail.Mirror
