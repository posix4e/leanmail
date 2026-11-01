import Lean.Data.Json
import Server.Gmail.Client
import Server.Gmail.OAuth

/-! # Gmail: telling Gmail about junk

A port of MailVault's `src/gmail/spam.ts`, driven by a queue instead of a scan. Reporting a
message as junk here (`Actions.apply "junk"`, with the `gmail.report_spam` switch on and the
message from Gmail) queues a `gmail_spam` row (`reporting`) and sets `messages.junk_gmail`; the
lane then reads the message's labels and marks it spam in Gmail (`SPAM` on, `INBOX` off),
remembering whether it had `INBOX`. "Not junk" queues the undo (`restoring`: `SPAM` off, and
`INBOX` back only where this lane took it off). What it will not do: report what Gmail already has
in Spam or Trash, or touch a message Gmail no longer has (`skipped`). A report Google refuses for
now stays queued; after 24 tries it is `failed`. -/

namespace Server.Gmail.Spam

open Lean Server.Db Server.Gmail

def perTick : Nat := 100
def maxAttempts : Nat := 24

structure Run where
  reported : Nat := 0
  restored : Nat := 0
  skipped : Nat := 0
  failed : Nat := 0
  deriving Repr, Inhabited

def state? (st : State) (id : String) : IO (Option String) := do
  return (← st.db.queryOne "SELECT state FROM gmail_spam WHERE message_id = ?" #[id]).map (·.text "state")

def setRow (st : State) (id state : String) (reason : Option String := none) : IO Unit := do
  st.db.exec "UPDATE gmail_spam SET state = ?, reason = ?, updated_at = ? WHERE message_id = ?" #[state, (reason : Value), ← Clock.nowMs, id]

/-- The hook: `row` is the message after the action. Whether anything was queued. -/
def onAction (st : State) (action : String) (row : Row) : IO Bool := do
  let some gm := row.text? "gm_msgid" | return false
  if row.text "source" == "sent" then return false
  let id := row.text "id"
  let cur ← state? st id
  match row.nat? "junk_at" with
  | some junkAt =>
    if action != "junk" then return false
    if !(← prefOn st prefSpam true) then return false
    let some c ← conn? st | return false
    if !c.authorized then return false
    match cur with
    | some "reported" | some "reporting" => return false
    | some "restoring" => setRow st id "reported"; return false
    | _ =>
      let now ← Clock.nowMs
      st.db.exec "INSERT INTO gmail_spam (message_id, gm_msgid, junk_at, state, reason, had_inbox, attempts, updated_at)
        VALUES (?1, ?2, ?3, 'reporting', NULL, 0, 0, ?4)
        ON CONFLICT(message_id) DO UPDATE SET gm_msgid = excluded.gm_msgid, junk_at = excluded.junk_at,
          state = 'reporting', reason = NULL, had_inbox = 0, attempts = 0, updated_at = excluded.updated_at"
        #[id, gm, junkAt, now]
      st.db.exec "UPDATE messages SET junk_gmail = 1 WHERE id = ?" #[id]
      return true
  | none =>
    match cur with
    | some "reported" => setRow st id "restoring"; return true
    | some "reporting" => setRow st id "released"; return false
    | _ => return false

/-- Change labels on one message; `false` when Gmail no longer has it. -/
def tryModify (st : State) (tok gm : String) (add remove : List String) : IO Bool := do
  try modify st tok gm add remove; return true
  catch e => if isMissing (codeOf e) then return false else throw e

/-- One run: undos first (the person is waiting to see the message back), then reports. -/
def step (st : State) : IO Run := do
  let some _ ← usable? st | return {}
  let back ← st.db.query "SELECT message_id, gm_msgid, had_inbox FROM gmail_spam WHERE state = 'restoring' LIMIT ?" #[perTick]
  let todo ← st.db.query "SELECT g.message_id, g.gm_msgid, g.attempts, m.junk_at AS live_junk, m.deleted AS gone
    FROM gmail_spam g LEFT JOIN messages m ON m.id = g.message_id
    WHERE g.state = 'reporting' LIMIT ?" #[perTick]
  if back.isEmpty && todo.isEmpty then return {}
  let tok ← OAuth.accessToken st
  let mut run : Run := {}
  for r in back do
    let id := r.text "message_id"
    let add := if r.bool "had_inbox" then ["INBOX"] else []
    if ← tryModify st tok (r.text "gm_msgid") add ["SPAM"] then
      setRow st id "restored"; run := { run with restored := run.restored + 1 }
    else
      setRow st id "skipped" (some "gone"); run := { run with skipped := run.skipped + 1 }
  for r in todo do
    let id := r.text "message_id"
    let gm := r.text "gm_msgid"
    if (r.nat? "live_junk").isNone || r.bool "gone" then
      setRow st id "released"
      continue
    let attempts := r.nat "attempts" + 1
    if attempts > maxAttempts then
      setRow st id "failed" (some "attempts"); run := { run with failed := run.failed + 1 }
      continue
    st.db.exec "UPDATE gmail_spam SET attempts = ? WHERE message_id = ?" #[attempts, id]
    match ← labelsOf st tok gm with
    | none => setRow st id "skipped" (some "gone"); run := { run with skipped := run.skipped + 1 }
    | some labels =>
      if labels.contains "TRASH" then
        setRow st id "skipped" (some "trash"); run := { run with skipped := run.skipped + 1 }
      else if labels.contains "SPAM" then
        setRow st id "skipped" (some "already"); run := { run with skipped := run.skipped + 1 }
      else
        let hadInbox := labels.contains "INBOX"
        st.db.exec "UPDATE gmail_spam SET had_inbox = ? WHERE message_id = ?" #[hadInbox, id]
        if ← tryModify st tok gm ["SPAM"] ["INBOX"] then
          setRow st id "reported"; run := { run with reported := run.reported + 1 }
        else
          setRow st id "skipped" (some "gone"); run := { run with skipped := run.skipped + 1 }
  Log.info "gmail.spam" [("reported", Json.num run.reported), ("restored", Json.num run.restored),
    ("skipped", Json.num run.skipped), ("failed", Json.num run.failed)]
  return run

/-- Daily: forget rows for messages the vault no longer holds. -/
def prune (st : State) : IO Unit :=
  st.db.exec "DELETE FROM gmail_spam WHERE NOT EXISTS (SELECT 1 FROM messages m WHERE m.id = gmail_spam.message_id AND m.deleted = 0)"

end Server.Gmail.Spam
