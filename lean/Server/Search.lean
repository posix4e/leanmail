import Core.Query
import Core.Search
import Server.State
import Server.Clock

/-! # Search over SQLite

`Core.Search` makes the MATCH string; this half turns the rest of the query into SQL predicates and
runs it. With FTS text the order is `bm25` (column weights from_ 3, to_ 3, cc 1, subject 5, body 1,
list_id 1, attachments 2) plus a small recency term, paged by offset; without, newest first with a
keyset cursor on `(received_at, id)`. -/

namespace Server.Search

open Server.Db Core.Query

def dayMs : Nat := 86400000

/-- WHERE fragments (AND-ed) and their arguments for everything that is not text. -/
def filterSql (f : Filters) (now : Nat) : Array String × Array Value := Id.run do
  let mut cs : Array String := #[]
  let mut args : Array Value := #[]
  let flag (b : Bool) (yes no : String) := if b then yes else no
  if let some b := f.inSent then cs := cs.push (flag b "m.folder = 'sent'" "m.folder = 'inbox'")
  if let some b := f.hasAttachment then cs := cs.push (flag b "m.has_attachments = 1" "m.has_attachments = 0")
  if let some b := f.hasInvite then cs := cs.push (flag b "m.has_invite = 1" "m.has_invite = 0")
  if let some b := f.isEvent then cs := cs.push (flag b "m.has_invite = 1" "m.has_invite = 0")
  if let some b := f.isExpiring then
    cs := cs.push (flag b "(m.expires_at IS NOT NULL AND m.expires_at < ?)" "(m.expires_at IS NULL OR m.expires_at >= ?)")
    args := args.push (.int (now + 7 * dayMs).toInt64)
  if let some b := f.isForever then cs := cs.push (flag b "m.expires_at IS NULL" "m.expires_at IS NOT NULL")
  if let some b := f.isSnoozed then
    cs := cs.push (flag b "(m.snoozed_until IS NOT NULL AND m.snoozed_until > ?)" "(m.snoozed_until IS NULL OR m.snoozed_until <= ?)")
    args := args.push (.int now.toInt64)
  if let some b := f.isArchived then cs := cs.push (flag b "m.archived_at IS NOT NULL" "m.archived_at IS NULL")
  if let some b := f.isToRead then cs := cs.push (flag b "m.to_read_at IS NOT NULL" "m.to_read_at IS NULL")
  if let some b := f.isJunk then cs := cs.push (flag b "m.junk_at IS NOT NULL" "m.junk_at IS NULL")
  if let some t := f.before then
    cs := cs.push "m.received_at < ?"
    args := args.push (.int t.toInt64)
  if let some t := f.after then
    cs := cs.push "m.received_at >= ?"
    args := args.push (.int t.toInt64)
  if !f.source.isEmpty then
    cs := cs.push ("m.source IN (" ++ ", ".intercalate (f.source.toList.map fun _ => "?") ++ ")")
    args := args ++ f.source.map (fun s => (.text (if s == "sweep" then "gmail" else s) : Value))
  return (cs, args)

/-- `<received_at>.<id>`: the keyset cursor. -/
def cursorOf (r : Row) : String := s!"{r.nat "received_at"}.{r.text "id"}"

def parseCursor (s : String) : Option (Nat × String) :=
  match s.splitOn "." with
  | [t, id] => t.toNat?.map (·, id)
  | _ => none

structure Page where
  rows : Array Row
  /-- A cursor (no FTS) or an offset (FTS) for the next page, when there may be one. -/
  next : Option String

/-- Run a query string. `extra` adds caller predicates (a list view's folder rules). -/
def runQuery (st : State) (q : Query) (limit : Nat := 50) (offset : Nat := 0) (before : Option String := none)
    (extra : Array String × Array Value := (#[], #[])) : IO Page := do
  let now ← Clock.nowMs
  let limit := max 1 (min limit 200)
  let (fcs, fargs) := filterSql q.filters now
  let mut cs : Array String := #["m.deleted = 0"] ++ fcs ++ extra.1
  let mut args : Array Value := fargs ++ extra.2
  match Core.Search.toFts q with
  | some m =>
    let sql := "SELECT m.* FROM fts JOIN messages m ON m.rowid = fts.rowid WHERE fts MATCH ? AND " ++
      " AND ".intercalate cs.toList ++
      " ORDER BY bm25(fts, 3, 3, 1, 5, 1, 1, 2) - m.received_at / 1e11, m.id DESC LIMIT ? OFFSET ?"
    let rows ← st.db.query sql (#[(.text m : Value)] ++ args ++ #[.int limit.toInt64, .int offset.toInt64])
    return { rows, next := if rows.size == limit then some (toString (offset + limit)) else none }
  | none =>
    if let some n := Core.Search.negMatch q then
      cs := cs.push "m.rowid NOT IN (SELECT rowid FROM fts WHERE fts MATCH ?)"
      args := args.push (.text n)
    if let some (t, id) := before.bind parseCursor then
      cs := cs.push "(m.received_at, m.id) < (?, ?)"
      args := args ++ #[.int t.toInt64, .text id]
    let sql := "SELECT m.* FROM messages m WHERE " ++ " AND ".intercalate cs.toList ++
      " ORDER BY m.received_at DESC, m.id DESC LIMIT ?"
    let rows ← st.db.query sql (args.push (.int limit.toInt64))
    return { rows, next := if rows.size == limit then rows.back?.map cursorOf else none }

def run (st : State) (q : String) (limit : Nat := 50) (offset : Nat := 0) (before : Option String := none) :
    IO (Array Row) := do
  return (← runQuery st (Core.Query.parse q) limit offset before).rows

end Server.Search
