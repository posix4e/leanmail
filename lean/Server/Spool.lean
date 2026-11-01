import Lean.Data.Json
import Std.Data.HashSet
import Core.Ulid
import Core.Text.Str
import Server.State
import Server.Ingest
import Server.Clock
import Server.Log

/-! # The spool

`raw/<acct>/<ulid>` holds a message's RFC 822 bytes and `in/<acct>/<ulid>` (zero bytes) marks it
for ingest. The Worker's `email()` writes the custom metadata `rcpt`, `from`, `received_at`,
`source` and `size`; the old vault's exporter (`scripts/export-to-leanmail.ts`, `spoolMetadata`)
also writes the message's state under `Ingest.Incoming`'s names: `rcpts` (a JSON array),
`folder`, `expires_at` (epoch ms or `never`), `archived_at`, `snoozed_until`, `to_read_at` and
`junk_kind`. A key that is absent keeps the default.

`drain` ingests every marked message, at most `concurrency` at once, and deletes each marker after
its commit; the raw object stays, as `raw_key`. It runs on boot and on every wake. A failure on one
message is logged and leaves its marker for the next drain; a marker whose raw object is missing
is dropped. -/

namespace Server.Spool

open Lean

def rawKey (acct ulid : String) : String := s!"raw/{acct}/{ulid}"
def markerKey (acct ulid : String) : String := s!"in/{acct}/{ulid}"

/-- Markers ingested at once. The SQLite writes serialise on the `Db` lock; what overlaps is the
store's GETs and DELETEs and the parsing. -/
def concurrency : Nat := 8

/-- What the raw object's metadata says about the message, as an `Incoming` (without `raw`, `key`
and `forcedId`). `now` stands in for a missing `received_at`. -/
def incomingOf (md : List (String × String)) (now : Nat) : Ingest.Incoming :=
  let get (k : String) := (md.find? (·.1.toLower == k)).map (·.2)
  let text (k : String) : Option String := (get k).bind fun v =>
    let t := Core.Text.Str.trim v
    if t.isEmpty then none else some t
  let nat (k : String) : Option Nat := (text k).bind (·.toNat?)
  let rcpts := Ingest.rcptsOf (get "rcpts")
  -- `rcpts` is the whole list (its first is `rcpt`); a `rcpt` beside it is kept too
  let (rcpt, moreRcpts) := match rcpts with
    | r :: rest => (some r, rest ++ (text "rcpt").toList)
    | [] => (text "rcpt", [])
  let expires := (text "expires_at").map (·.toLower)
  { raw := .empty, key := none, rcpt, moreRcpts
    source := (text "source").getD "route"
    receivedAt := (nat "received_at").getD now
    folder := ((text "folder").map (·.toLower)).getD "inbox"
    expiresAt := expires.bind (·.toNat?)
    keepForever := expires == some "never" || expires == some "null"
    archivedAt := nat "archived_at"
    snoozedUntil := nat "snoozed_until"
    toReadAt := nat "to_read_at"
    junkKind := text "junk_kind" }

/-- Ingest one marked message. `true` when the marker was consumed. -/
def drainOne (st : State) (acct : String) (marker : String) : IO Bool := do
  let ulid := (marker.splitOn "/").getLast!
  let key := rawKey acct ulid
  match ← st.store.get key with
  | none =>
    Log.warn "spool.raw_missing" [("ulid", Json.str ulid)]
    st.store.delete marker
    return true
  | some (raw, md) =>
    let inc := incomingOf md (← Clock.nowMs)
    if let some sz := ((md.find? (·.1.toLower == "size")).map (·.2)).bind (·.toNat?) then
      if sz != raw.size then
        Log.warn "spool.size_mismatch" [("ulid", Json.str ulid), ("meta", Json.num sz), ("bytes", Json.num raw.size)]
    let r ← Ingest.ingest st { inc with
      raw, key := some key, forcedId := if Core.Ulid.isValid ulid then some ulid else none }
    st.store.delete marker
    Log.info "spool.drained" [("ulid", Json.str ulid), ("id", Json.str r.id)]
    return true

/-- Run `drainOne` on every marker at once, one task each, and wait for them all. A task that
throws is its own `.error`; the others are unaffected. -/
def drainBatch (st : State) (acct : String) (markers : Array String) :
    IO (Array (String × Except IO.Error Bool)) := do
  let tasks ← markers.mapM fun m => do
    return (m, ← IO.asTask (drainOne st acct m) (prio := .dedicated))
  tasks.mapM fun (m, t) => do return (m, ← IO.wait t)

/-- Drain `in/<acct>/` until it is empty: list a page (≤ `Store.listMax`), drain it `concurrency`
at a time, list again. A marker that failed is not retried in the same drain; the drain stops when
a page makes no progress. The number of markers consumed. -/
def drain (st : State) : IO Nat := do
  let acct := st.config.accountId
  let mut n : Nat := 0
  let mut failed : Std.HashSet String := {}
  let mut pass := 0
  repeat
    let listed ← st.store.list s!"in/{acct}/"
    let todo := listed.filter (!failed.contains ·)
    if todo.isEmpty || pass ≥ 1000 then break
    let t0 ← IO.monoMsNow
    let mut progress : Nat := 0
    let mut i : Nat := 0
    while i < todo.size do
      for (m, r) in ← drainBatch st acct (todo.extract i (i + concurrency)) do
        match r with
        | .ok true => progress := progress + 1
        | .ok false => failed := failed.insert m
        | .error e =>
          failed := failed.insert m
          Log.error "spool.failed" [("key", Json.str m), ("error", Json.str (toString e))]
      i := i + concurrency
    let ms := (← IO.monoMsNow) - t0
    Log.info "spool.batch" [("count", Json.num progress), ("failed", Json.num (todo.size - progress)),
      ("ms", Json.num ms)]
    n := n + progress
    pass := pass + 1
    if progress == 0 then break
  return n

end Server.Spool
