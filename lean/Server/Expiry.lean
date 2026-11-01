import Lean.Data.Json
import Core.Ulid
import Server.State
import Server.Router
import Server.Api
import Server.Spool
import Server.Clock
import Server.Log

/-! # Expiry and housekeeping lanes (`Server.Jobs.tick` runs them)

A port of MailVault's `src/expiry.ts`:

* `expire` — mail and events whose `expires_at` has passed are soft-deleted in batches of 500
  (`deleted = 1`, `deleted_at`, and for mail `purge_after = now + 7 days`: the undo window).
* `purge` — once `purge_after` passes, the raw object is deleted from the store and the row's
  text, attachment metadata and FTS entry go; the row stays as a tombstone with `purged_at`.
  Events deleted over 7 days ago are dropped.
* `orphan_gc` — a walk of `raw/<acct>/` with its cursor in `jobs.cursor`, three pages a day,
  deleting objects uploaded over 24 hours ago that no row holds and no spool marker names. An
  object whose upload time the store does not report is left alone.
* `spool_gc` — `in/` markers over 24 hours old whose message is already held, or whose raw object
  is gone, are deleted; the rest are counted (the drain retries them).
* `stuck_sends` — a Sent copy still `sending` 24 hours on is `unknown`: whether it went is not
  known, and it is not retried.

Also `GET /api/expiring?days=7` and `POST /api/msg/expiry {ids, expires_at | never}`. -/

namespace Server.Expiry

open Lean Server.Db Server.Router

def dayMs : Nat := 86400000
def batch : Nat := 500
def rounds : Nat := 10
def purgeDeferMs : Nat := 7 * dayMs
def orphanPages : Nat := 3
def orphanPageSize : Nat := 1000
def orphanLane : String := "orphan_gc"

/-- Soft-delete what is due: (messages, events). -/
def expireDue (st : State) (now : Nat) : IO (Nat × Nat) := do
  let mut msgs : Nat := 0
  for _ in [0:rounds] do
    let n ← st.db.execCount "UPDATE messages SET deleted = 1, deleted_at = ?1, purge_after = ?2, updated_at = ?1
      WHERE id IN (SELECT id FROM messages WHERE deleted = 0 AND expires_at IS NOT NULL AND expires_at <= ?1
                   ORDER BY expires_at LIMIT ?3)" #[now, now + purgeDeferMs, batch]
    msgs := msgs + n
    if n < batch then break
  let mut evs : Nat := 0
  for _ in [0:rounds] do
    let n ← st.db.execCount "UPDATE events SET deleted = 1, deleted_at = ?1, updated_at = ?1
      WHERE id IN (SELECT id FROM events WHERE deleted = 0 AND kept = 0 AND expires_at IS NOT NULL AND expires_at <= ?1
                   ORDER BY expires_at LIMIT ?2)" #[now, batch]
    evs := evs + n
    if n < batch then break
  if msgs + evs > 0 then
    Log.info "expiry.soft_deleted" [("messages", Json.num msgs), ("events", Json.num evs)]
  return (msgs, evs)

/-- Delete the raw objects and the derived rows of mail past its undo window. The number purged. -/
def purgeDue (st : State) (now : Nat) : IO Nat := do
  let mut purged : Nat := 0
  for _ in [0:rounds] do
    let rows ← st.db.query "SELECT id, rowid, raw_key FROM messages WHERE deleted = 1 AND purged_at IS NULL
      AND purge_after IS NOT NULL AND purge_after <= ? LIMIT ?" #[now, batch]
    if rows.isEmpty then break
    let mut done : Array Row := #[]
    for r in rows do
      match r.text? "raw_key" with
      | some k =>
        -- a raw object another live row still points at stays
        let shared ← st.db.queryOne "SELECT 1 AS x FROM messages WHERE raw_key = ? AND id != ? AND purged_at IS NULL LIMIT 1"
          #[k, r.text "id"]
        let ok ← if shared.isSome then pure true else
          (do st.store.delete k; pure true) <|> pure false
        if ok then done := done.push r
      | none => done := done.push r
    st.db.transaction do
      for r in done do
        let id := r.text "id"
        let rowid := r.int "rowid"
        st.db.execUnlocked "INSERT INTO fts (fts, rowid, from_, to_, cc, subject, body, list_id, attachments)
          SELECT 'delete', rowid, from_, to_, cc, subject, body, list_id, attachments FROM fts_src WHERE rowid = ?" #[rowid]
        st.db.execUnlocked "DELETE FROM fts_src WHERE rowid = ?" #[rowid]
        st.db.execUnlocked "DELETE FROM message_text WHERE message_id = ?" #[id]
        st.db.execUnlocked "DELETE FROM attachments WHERE message_id = ?" #[id]
        st.db.execUnlocked "UPDATE messages SET purged_at = ?, raw_key = NULL WHERE id = ?" #[now, id]
    purged := purged + done.size
    if done.size < rows.size || rows.size < batch then break
  let evs ← st.db.execCount "DELETE FROM events WHERE deleted = 1 AND deleted_at IS NOT NULL AND deleted_at <= ?"
    #[(now - purgeDeferMs : Nat)]
  if purged + evs > 0 then Log.info "expiry.purged" [("messages", Json.num purged), ("events", Json.num evs)]
  return purged

def cursorOf (st : State) (lane : String) : IO String := do
  return ((← st.db.queryOne "SELECT cursor FROM jobs WHERE name = ?" #[lane]).bind (·.text? "cursor")).getD ""

def setCursor (st : State) (lane cursor : String) : IO Unit := do
  st.db.exec "INSERT OR IGNORE INTO jobs (name) VALUES (?)" #[lane]
  st.db.exec "UPDATE jobs SET cursor = ? WHERE name = ?" #[cursor, lane]

/-- The orphan walk: up to `orphanPages` pages from the stored cursor. The number deleted. -/
def orphanGc (st : State) (now : Nat) : IO Nat := do
  let acct := st.config.accountId
  let pre := s!"raw/{acct}/"
  let mut cursor ← cursorOf st orphanLane
  let markers := (← st.store.list s!"in/{acct}/").map fun k => (k.splitOn "/").getLast!
  let mut removed : Nat := 0
  for _ in [0:orphanPages] do
    let page ← st.store.listPage pre cursor orphanPageSize
    for o in page do
      let some up := o.uploadedAt | continue
      if now < up + dayMs then continue
      let id := (o.key.splitOn "/").getLast!
      if markers.contains id then continue
      let held ← st.db.queryOne "SELECT 1 AS x FROM messages WHERE raw_key = ? OR id = ? LIMIT 1" #[o.key, id]
      if held.isSome then continue
      st.store.delete o.key
      removed := removed + 1
    if page.size < orphanPageSize then
      cursor := ""
      break
    cursor := (page.back?.map (·.key)).getD ""
  setCursor st orphanLane cursor
  if removed > 0 then Log.info "expiry.orphans_removed" [("count", Json.num removed)]
  return removed

/-- Stale spool markers: (deleted, still pending). -/
def spoolGc (st : State) (now : Nat) : IO (Nat × Nat) := do
  let acct := st.config.accountId
  let page ← st.store.listPage s!"in/{acct}/" "" 1000
  let mut deleted : Nat := 0
  let mut pending : Nat := 0
  for o in page do
    let some up := o.uploadedAt | continue
    if now < up + dayMs then continue
    let id := (o.key.splitOn "/").getLast!
    let held ← st.db.queryOne "SELECT 1 AS x FROM messages WHERE id = ?" #[id]
    if held.isSome || (← st.store.get (Spool.rawKey acct id)).isNone then
      st.store.delete o.key
      deleted := deleted + 1
    else pending := pending + 1
  if deleted + pending > 0 then
    Log.info "spool.gc" [("deleted", Json.num deleted), ("stuck", Json.num pending)]
  return (deleted, pending)

/-- Sent copies `sending` for over a day become `unknown`. -/
def stuckSends (st : State) (now : Nat) : IO Nat := do
  let n ← st.db.execCount "UPDATE messages SET send_status = 'unknown', send_error = COALESCE(send_error, 'stuck'), updated_at = ?1
    WHERE send_status = 'sending' AND updated_at < ?2" #[now, (now - dayMs : Nat)]
  if n > 0 then Log.warn "send.stuck" [("count", Json.num n)]
  return n

/-! ## Routes -/

/-- Mail and events that expire within `days` (default 7), soonest first. -/
def expiringJson (st : State) (now days : Nat) (limit : Nat := 200) : IO Json := do
  let until_ := now + days * dayMs
  let msgs ← st.db.query "SELECT * FROM messages WHERE deleted = 0 AND expires_at IS NOT NULL AND expires_at > ? AND expires_at <= ?
    ORDER BY expires_at, id LIMIT ?" #[now, until_, limit]
  let evs ← st.db.query "SELECT id, data, expires_at, message_id FROM events WHERE deleted = 0 AND kept = 0 AND expires_at IS NOT NULL
    AND expires_at > ? AND expires_at <= ? ORDER BY expires_at LIMIT ?" #[now, until_, limit]
  return Json.mkObj [("until", Json.num until_), ("messages", Json.arr (msgs.map Api.listItem)),
    ("events", Json.arr (evs.map fun r => Json.mkObj [("id", Json.str (r.text "id")), ("expires_at", Json.num (r.nat "expires_at")),
      ("message_id", Api.optText r "message_id"),
      ("summary", Json.str ((((Json.parse (r.text "data")).toOption.bind fun j => (j.getObjValAs? String "summary").toOption)).getD ""))]))]

def expiringGet (c : Ctx) (req : Req) : IO Resp := do
  return Resp.ok (← expiringJson c.st (← Clock.nowMs) (min 90 (req.natParam "days" 7)))

/-- Set (or clear, `never`) the expiry of up to 500 live messages. The number changed. -/
def setExpiry (st : State) (ids : List String) (expiresAt : Option Nat) : IO Nat := do
  let now ← Clock.nowMs
  let mut n : Nat := 0
  for id in ids.take 500 do
    n := n + (← st.db.execCount "UPDATE messages SET expires_at = ?, updated_at = ? WHERE id = ? AND deleted = 0"
      #[(expiresAt : Value), now, id])
  return n

def expiryPost (c : Ctx) (req : Req) : IO Resp := do
  let j := req.json
  let some ids := (j.getObjValAs? (Array String) "ids").toOption | return Resp.error 400 "ids required"
  if ids.isEmpty || ids.size > 500 then return Resp.error 400 "1 to 500 ids"
  let exp : Option Nat ← match j.getObjVal? "expires_at" with
    | .ok .null | .ok (.str "never") => pure none
    | .ok v => match v.getNat?.toOption with
      | some n => pure (some n)
      | none => return Resp.error 400 "bad expires_at"
    | .error _ => if jbool? j "never" == some true then pure none else return Resp.error 400 "expires_at or never"
  let n ← setExpiry c.st ids.toList exp
  return Resp.ok (Json.mkObj [("updated", Json.num n), ("expires_at", match exp with | some e => Json.num e | none => Json.null)])

def routes : Array Route := #[
  route "GET" "/api/expiring" (.token "read") expiringGet,
  route "POST" "/api/msg/expiry" (.token "act") expiryPost
]

end Server.Expiry
