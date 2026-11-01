import Lean.Data.Json
import Core.Mime.Date
import Server.Router.Types
import Server.Api
import Server.Clock
import Server.Gmail.Mirror

/-! # Message actions

Done, snooze, to-read, junk and keep-forever as column updates on `messages` (`archived_at`,
`snoozed_until`, `to_read_at`, `junk_at`/`junk_kind`, `expires_at`), each with `updated_at`.
`PATCH /api/msg/:id {action, until?}` (token `act`, or a session with the CSRF headers) answers the
updated list item; the pages' forms call `apply` directly.

Actions: `done`, `inbox` (undo done and snooze), `snooze` (`until` epoch ms in the future),
`unsnooze`, `toread`, `untoread`, `junk`, `notjunk`, `forever`; and the button shortcuts
`snooze_1d`, `snooze_3d`, `snooze_week` (next Monday 08:00 UTC).

After each applied action `Gmail.Mirror.onAction` queues what Gmail should hear (the archive
mirror for done and its undo, the spam report for junk and not junk). -/

namespace Server.Actions

open Lean Server.Db Server.Router

def dayMs : Nat := 86400000

/-- 08:00 UTC on the Monday after `now`. 1970-01-01 was a Thursday. -/
def nextMonday (now : Nat) : Nat :=
  let day := now / dayMs
  let dow := (day + 3) % 7  -- 0 = Monday
  (day + (7 - dow)) * dayMs + 8 * 3600 * 1000

/-- The SET clause and its arguments for an action, or an error message. -/
def plan (action : String) (untilMs : Option Nat) (now : Nat) : Except String (String × Array Value) :=
  match action with
  | "done" => .ok ("archived_at = ?, snoozed_until = NULL", #[.int now.toInt64])
  | "inbox" => .ok ("archived_at = NULL, snoozed_until = NULL", #[])
  | "snooze" =>
    match untilMs with
    | some u => if u > now then .ok ("snoozed_until = ?, archived_at = NULL", #[.int u.toInt64]) else .error "until must be in the future"
    | none => .error "snooze needs until"
  | "snooze_1d" => .ok ("snoozed_until = ?, archived_at = NULL", #[.int (now + dayMs).toInt64])
  | "snooze_3d" => .ok ("snoozed_until = ?, archived_at = NULL", #[.int (now + 3 * dayMs).toInt64])
  | "snooze_week" => .ok ("snoozed_until = ?, archived_at = NULL", #[.int (nextMonday now).toInt64])
  | "unsnooze" => .ok ("snoozed_until = NULL", #[])
  | "toread" => .ok ("to_read_at = ?", #[.int now.toInt64])
  | "untoread" => .ok ("to_read_at = NULL", #[])
  | "junk" => .ok ("junk_at = ?, junk_kind = 'manual'", #[.int now.toInt64])
  | "notjunk" => .ok ("junk_at = NULL, junk_kind = NULL", #[])
  | "forever" => .ok ("expires_at = NULL", #[])
  | _ => .error "unknown action"

/-- Apply an action to a live message: the updated row, `none` when there is no such message,
or the reason it was refused. -/
def apply (st : State) (id action : String) (untilMs : Option Nat := none) : IO (Except String (Option Row)) := do
  let now ← Clock.nowMs
  match plan action untilMs now with
  | .error e => return .error e
  | .ok (set, args) =>
    let n ← st.db.execCount s!"UPDATE messages SET {set}, updated_at = ? WHERE id = ? AND deleted = 0"
      (args ++ #[.int now.toInt64, .text id])
    if n == 0 then return .ok none
    -- Gmail: done/undo mirrored, junk reported (queued for the lanes; never fails the action)
    Gmail.Mirror.onAction st id action
    return .ok (← Api.messageRow st id)

/-- Whether the message leaves the current list view after this action. -/
def leavesList (action : String) : Bool :=
  ["done", "snooze", "snooze_1d", "snooze_3d", "snooze_week", "junk"].contains action

/-- `PATCH /api/msg/:id {action, until?}`. -/
def patch (c : Ctx) (req : Req) : IO Resp := do
  let j := req.json
  let some action := jstr? j "action" | return Resp.error 400 "missing action"
  let untilMs := (j.getObjValAs? Nat "until").toOption
  match ← apply c.st (c.param "id") action untilMs with
  | .error e => return Resp.error 400 e
  | .ok none => return Resp.notFound
  | .ok (some r) => return Resp.ok (Api.listItem r)

def routes : Array Route := #[
  route "PATCH" "/api/msg/:id" (.token "act") patch
]

end Server.Actions
