import Lean.Data.Json
import Server.State
import Server.Router.Types
import Server.Spool
import Server.Clock
import Server.Log
import Server.Gmail.Client
import Server.Gmail.OAuth
import Server.Gmail.Poll
import Server.Gmail.Backfill
import Server.Gmail.Mirror
import Server.Gmail.Spam
import Server.Gmail.Watch
import Server.Expiry
import Server.AI

/-! # Jobs: lanes under a lease

`runLane st name leaseMs body` takes the lane's row in `jobs` (`lease_until`) with one
`UPDATE … RETURNING` under the database lock, so a lane never runs twice at once, whether the
second caller is the in-process timer, a Worker wake (`POST /internal/tick`) or a push; a lane
already leased is skipped. It records `last_run_at` and `last_error` and frees the lease. A run
killed without freeing it is free again once `lease_until` passes.

`tick` runs what is due: drain the spool, then with a Gmail connection the `gmail.poll`,
`gmail.mirror`, `gmail.spam` lanes and a `gmail.backfill` step while a backfill runs, and with the
AI on the `ai` triage lane (`Server.AI.triageStep`); `daily`
adds the watch renewal, the send-as refresh and pruning (`history_failures` for mail now held or
given up on over 30 days ago, mirror and spam rows for deleted mail).

`loop` is the 60-second timer while the container is awake (started by `Server.Main`); it also
runs the mirror and spam lanes within a second of a hook's `Gmail.kick`. Between wakes the Worker
schedules the next one from the `X-Next-Wake` header every response carries (`nextWake`): 60 s
while a backfill runs, 1800 s with an active Pub/Sub watch, 300 s with a connection and no watch,
and none without Gmail. -/

namespace Server.Jobs

open Lean Server.Db Server.Router

/-- Take the lane's lease until `now + leaseMs` unless someone holds it. -/
def acquire (st : State) (name : String) (leaseMs now : Nat) : IO Bool := do
  st.db.exec "INSERT OR IGNORE INTO jobs (name) VALUES (?)" #[name]
  let rows ← st.db.query "UPDATE jobs SET lease_until = ?1 WHERE name = ?2 AND (lease_until IS NULL OR lease_until <= ?3) RETURNING name"
    #[now + leaseMs, name, now]
  return rows.size > 0

/-- Run `body` as lane `name`; `false` when the lane was leased by another run. -/
def runLane (st : State) (name : String) (leaseMs : Nat) (body : IO Unit) : IO Bool := do
  if !(← acquire st name leaseMs (← Clock.nowMs)) then return false
  let err : Option String ← try body; pure none catch e => do
    let code := Gmail.codeOf e
    let msg := if code == "error" then String.ofList ((toString e).toList.take 200) else code
    Log.warn "jobs.lane_failed" [("lane", Json.str name), ("error", Json.str code)]
    pure (some msg)
  st.db.exec "UPDATE jobs SET lease_until = NULL, last_run_at = ?, last_error = ? WHERE name = ?" #[← Clock.nowMs, (err : Value), name]
  return true

/-- The `X-Next-Wake` value, in seconds. -/
def nextWake (st : State) : IO (Option Nat) := do
  let some c ← Gmail.conn? st | return none
  if !c.authorized then return none
  if ← Gmail.Backfill.running? st then return some 60
  if Gmail.Watch.watchActive c (← Clock.nowMs) then return some 1800
  return some 300

def withNextWake (st : State) (r : Resp) : IO Resp := do
  match ← (try nextWake st catch _ => pure none) with
  | some s => return r.withHeader "X-Next-Wake" (toString s)
  | none => return r

def backfillLane (st : State) : IO Bool :=
  runLane st "gmail.backfill" Gmail.Backfill.leaseMs (discard <| Gmail.Backfill.step st)

/-- The lanes the hooks wake early. -/
def labelLanes (st : State) : IO Unit := do
  discard <| runLane st "gmail.mirror" 120000 (discard <| Gmail.Mirror.step st)
  discard <| runLane st "gmail.spam" 120000 (discard <| Gmail.Spam.step st)

/-- Daily housekeeping. -/
def prune (st : State) : IO Unit := do
  let old := (← Clock.nowMs) - 30 * 86400000
  st.db.exec "DELETE FROM history_failures WHERE gm_msgid IN (SELECT gm_msgid FROM messages WHERE gm_msgid IS NOT NULL)
    OR (attempts >= ? AND last_at < ?)" #[Gmail.Poll.retryAttempts, old]
  Gmail.Mirror.prune st
  Gmail.Spam.prune st

/-- Run what is due. With `background` the backfill step (up to four minutes) runs on its own
thread and the call returns at once. The lanes that ran, by name. -/
def tick (st : State) (daily : Bool) (background : Bool := false) : IO (Array String) := do
  let mut ran : Array String := #[]
  try discard <| Spool.drain st catch e => Log.warn "jobs.drain_failed" [("error", Json.str (toString e))]
  if (← Gmail.usable? st).isSome then
    if ← runLane st "gmail.poll" 120000 (discard <| Gmail.Poll.poll st) then ran := ran.push "gmail.poll"
    if ← runLane st "gmail.mirror" 120000 (discard <| Gmail.Mirror.step st) then ran := ran.push "gmail.mirror"
    if ← runLane st "gmail.spam" 120000 (discard <| Gmail.Spam.step st) then ran := ran.push "gmail.spam"
    if daily then
      let renew : IO Unit := do
        discard <| Gmail.Watch.renewIfDue st (← Clock.nowMs)
        Gmail.OAuth.refreshSendAs st (← Gmail.OAuth.accessToken st)
      if ← runLane st "gmail.watch" 60000 renew then ran := ran.push "gmail.watch"
    if ← Gmail.Backfill.running? st then
      if background then
        discard <| IO.asTask (discard <| backfillLane st) Task.Priority.dedicated
        ran := ran.push "gmail.backfill"
      else if ← backfillLane st then ran := ran.push "gmail.backfill"
  if daily then
    try prune st; ran := ran.push "prune" catch e => Log.warn "jobs.prune_failed" [("error", Json.str (toString e))]
  -- expiry and housekeeping (`Server.Expiry`): the cheap lanes every tick, the walks daily
  let now ← Clock.nowMs
  if ← runLane st "expire" 120000 (discard <| Expiry.expireDue st now) then ran := ran.push "expire"
  if ← runLane st "purge" 300000 (discard <| Expiry.purgeDue st now) then ran := ran.push "purge"
  if ← runLane st "stuck_sends" 60000 (discard <| Expiry.stuckSends st now) then ran := ran.push "stuck_sends"
  -- the optional AI's triage (`Server.AI`), 20 new messages a tick while it is on
  -- (on its own thread for a Worker wake: up to 20 model calls)
  if ← AI.enabled st then
    let aiLane : IO Bool := runLane st AI.lane 600000 (discard <| AI.triageStep st)
    if background then
      discard <| IO.asTask (discard aiLane) Task.Priority.dedicated
      ran := ran.push AI.lane
    else if ← aiLane then ran := ran.push AI.lane
  if daily then
    if ← runLane st Expiry.orphanLane 600000 (discard <| Expiry.orphanGc st now) then ran := ran.push "orphan_gc"
    if ← runLane st "spool_gc" 300000 (discard <| Expiry.spoolGc st now) then ran := ran.push "spool_gc"
  return ran

/-- The in-process timer: a tick every 60 s, the label lanes on a kick, until `stop`. -/
def loop (st : State) (stop : IO.Ref Bool) (periodMs : Nat := 60000) : IO Unit := do
  let mut last ← IO.monoMsNow
  repeat
    if ← stop.get then break
    IO.sleep 1000
    try
      if ← Gmail.kicked.get then
        Gmail.kicked.set false
        labelLanes st
      let now ← IO.monoMsNow
      if now - last ≥ periodMs then
        last := now
        discard <| tick st false
    catch e => Log.error "jobs.loop_failed" [("error", Json.str (toString e))]

/-! ## Routes -/

/-- `POST /internal/tick[?daily=1]`: the Worker's wake. -/
def tickRoute (c : Ctx) (req : Req) : IO Resp := do
  let ran ← tick c.st (req.param? "daily" == some "1") (background := true)
  return Resp.ok (Json.mkObj [("ok", Json.bool true), ("ran", Json.arr (ran.map Json.str))])

/-- `POST /internal/gmail/push {emailAddress, historyId}` from the Worker's Pub/Sub hook. -/
def pushRoute (c : Ctx) (req : Req) : IO Resp := do
  let j := req.json
  let email := jstr? j "emailAddress"
  let hid := Gmail.jStrNum? j "historyId"
  let go ← Gmail.Watch.notePush c.st email hid
  let polled ← if go then runLane c.st "gmail.poll" 120000 (discard <| Gmail.Poll.poll c.st) else pure false
  Log.info "gmail.push" [("polled", Json.bool polled)]
  return Resp.ok (Json.mkObj [("ok", Json.bool true), ("polled", Json.bool polled)])

def routes : Array Route := #[
  route "POST" "/internal/tick" .internal tickRoute,
  route "POST" "/internal/gmail/push" .internal pushRoute
]

end Server.Jobs
