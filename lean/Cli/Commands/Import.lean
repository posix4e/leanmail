import Lean.Data.Json
import Core.Mime.Header
import Core.Mime.Date
import Cli.Context
import Cli.Format

/-! # `import <path…>`

Each path is a `.eml` file, a directory (every `.eml` directly inside, sorted by name) or an mbox
file (split on lines that start `From `, with `>From ` un-escaped mboxrd style). Each message is
one `POST /api/import` with the raw bytes and `X-Received-At` (the `Date` header, else the mbox
`From ` line's date, else the file's mtime), `X-Source`, `X-Folder` and `X-Rcpts`. A message that
fails is reported and skipped; the command exits 1 at the end if any did, and 2 at once if the
server cannot be reached. -/

namespace Cli.Commands

open Lean Cli

/-- The server reads request bodies up to this. -/
def importMax : Nat := 32 * 1024 * 1024

structure Item where
  label : String
  raw : ByteArray
  /-- When the message has no usable `Date` header. -/
  fallbackAt : Option Nat

private def fromBytes : List UInt8 := "From ".toUTF8.toList

/-- Does `b` hold `pat` at `i`? -/
def bytesAt (b : ByteArray) (i : Nat) (pat : List UInt8) : Bool :=
  go i pat
where
  go : Nat → List UInt8 → Bool
    | _, [] => true
    | j, p :: ps => j < b.size && b[j]! == p && go (j + 1) ps

/-- Start offsets of every line. -/
def lineStarts (b : ByteArray) : Array Nat := Id.run do
  let mut out := if b.size > 0 then #[0] else #[]
  for i in [0:b.size] do
    if b[i]! == 10 && i + 1 < b.size then out := out.push (i + 1)
  return out

/-- A byte range as text, each byte one character (headers are ASCII; this never fails). -/
def latin1 (b : ByteArray) (lo hi : Nat) : String := Id.run do
  let mut s := ""
  for i in [lo:hi] do s := s.push (Char.ofNat b[i]!.toNat)
  return s

/-- The end of line `i` (exclusive of the LF, and of a CR before it). -/
def lineEnd (b : ByteArray) (i : Nat) : Nat := Id.run do
  let mut j := i
  while j < b.size && b[j]! != 10 do j := j + 1
  if j > i && b[j - 1]! == 13 then j - 1 else j

/-- One mbox message body: un-escape `>+From ` by one `>`, drop the separator blank line. -/
def unescapeMbox (b : ByteArray) (lo hi : Nat) : ByteArray := Id.run do
  let mut out := ByteArray.emptyWithCapacity (hi - lo)
  let mut atLine := true
  let mut i := lo
  while i < hi do
    if atLine && b[i]! == 62 then
      -- count the `>`s; drop one when `From ` follows
      let mut j := i
      while j < hi && b[j]! == 62 do j := j + 1
      if bytesAt b j fromBytes then i := i + 1
    let c := b[i]!
    out := out.push c
    atLine := c == 10
    i := i + 1
  -- the blank line mbox writes before the next `From `
  let n := out.size
  if n ≥ 4 && out[n-4]! == 13 && out[n-3]! == 10 && out[n-2]! == 13 && out[n-1]! == 10 then
    return out.extract 0 (n - 2)
  if n ≥ 2 && out[n-2]! == 10 && out[n-1]! == 10 then return out.extract 0 (n - 1)
  return out

/-- The date on an mbox `From sender Sat Sep 12 14:32:12 2026` line. -/
def fromLineDate (line : String) : Option Nat :=
  match (line.splitOn " ").filter (!·.isEmpty) with
  | _ :: _ :: rest => Core.Mime.Date.parse? (" ".intercalate rest)
  | _ => none

/-- Split an mbox into (separator line, message bytes). -/
def splitMbox (b : ByteArray) : Array (String × ByteArray) := Id.run do
  let seps := (lineStarts b).filter fun i => bytesAt b i fromBytes
  let mut out := #[]
  for k in [0:seps.size] do
    let s := seps[k]!
    let lineHi := lineEnd b s
    let bodyLo := Nat.min b.size (if lineHi < b.size && b[lineHi]! == 13 then lineHi + 2 else lineHi + 1)
    let bodyHi := if k + 1 < seps.size then seps[k + 1]! else b.size
    if bodyLo < bodyHi then out := out.push (latin1 b s lineHi, unescapeMbox b bodyLo bodyHi)
  return out

/-- The `Date` header, from the header block only. -/
def dateHeader? (raw : ByteArray) : Option Nat := Id.run do
  let cap := Nat.min raw.size (256 * 1024)
  let mut lines : Array String := #[]
  let mut i := 0
  while i < cap do
    let e := lineEnd raw i
    let line := latin1 raw i (Nat.min e cap)
    if line.isEmpty then break
    lines := lines.push line
    i := (if e < raw.size && raw[e]! == 13 then e + 2 else e + 1)
  let fields := Core.Mime.Header.parseLines lines
  match fields.find? (·.key == "date") with
  | some f => Core.Mime.Date.parse? f.value
  | none => none

def mtimeMs (p : System.FilePath) : IO Nat := do
  let m ← p.metadata
  return m.modified.sec.toNat * 1000 + m.modified.nsec.toNat / 1000000

private def isEml (p : System.FilePath) : Bool := (p.extension.map (·.toLower)) == some "eml"

/-- The files a path names: itself, or the `.eml` files directly inside a directory. -/
def expand (p : String) : CliM (Array System.FilePath) := do
  let fp := System.FilePath.mk p
  unless ← (fp.pathExists : IO Bool) do fail s!"{p}: no such file or directory"
  if ← (fp.isDir : IO Bool) then
    let entries ← liftIO fp.readDir (pre := s!"{p}: ")
    let files := entries.filter (fun e => isEml e.path) |>.map (·.path)
    return files.qsort (fun a b => a.toString < b.toString)
  return #[fp]

/-- The messages one file holds. -/
def itemsOf (fp : System.FilePath) : CliM (Array Item) := do
  let raw ← liftIO (IO.FS.readBinFile fp) (pre := s!"{fp}: ")
  let mt ← liftIO (mtimeMs fp)
  if !isEml fp && bytesAt raw 0 fromBytes then
    let msgs := splitMbox raw
    return (msgs.zipIdx.map fun ((line, m), k) =>
      { label := s!"{fp}#{k + 1}", raw := m, fallbackAt := (fromLineDate line).orElse fun _ => some mt })
  return #[{ label := fp.toString, raw, fallbackAt := some mt }]

structure Tally where
  stored : Nat := 0
  duplicate : Nat := 0
  blocked : Nat := 0
  failed : Nat := 0
  results : Array Json := #[]

def importOne (cl : Client) (headers : List (String × String)) (it : Item) : CliM (String × String) := do
  if it.raw.isEmpty then fail "empty message"
  if it.raw.size > importMax then fail s!"{humanSize it.raw.size} is over the {humanSize importMax} import limit"
  let at_ := (dateHeader? it.raw).orElse fun _ => it.fallbackAt
  let hs := headers ++ (at_.map fun t => [("X-Received-At", toString t)]).getD [] ++
    [("Content-Type", "message/rfc822")]
  let r ← cl.postRaw "/api/import" it.raw hs
  let j ← parseJson r
  return (jStr j "status", jStr j "id")

def importCmd (c : Ctx) : CliM UInt32 := do
  c.only ["source", "rcpt", "folder"]
  let paths ← c.arity 1 100000 "import <path…> [--source import|route] [--rcpt addr] [--folder inbox|sent]"
  let source := (c.args.flag? "source").getD "import"
  unless ["import", "route"].contains source do usageError "--source is import or route"
  let folder := (c.args.flag? "folder").getD "inbox"
  unless ["inbox", "sent"].contains folder do usageError "--folder is inbox or sent"
  let rcpts := c.args.flags "rcpt"
  let headers := [("X-Source", source), ("X-Folder", folder)] ++
    (if rcpts.isEmpty then [] else [("X-Rcpts", (Json.arr (rcpts.toArray.map Json.str)).compress)])
  let cl ← c.client
  let mut t : Tally := {}
  for p in paths do
    let files ← try expand p catch e =>
      err s!"leanmail: {e.msg}"
      t := { t with failed := t.failed + 1, results := t.results.push (Json.mkObj [("source", Json.str p), ("error", Json.str e.msg)]) }
      pure #[]
    for fp in files do
      let items ← try itemsOf fp catch e =>
        err s!"leanmail: {e.msg}"
        t := { t with failed := t.failed + 1, results := t.results.push (Json.mkObj [("source", Json.str fp.toString), ("error", Json.str e.msg)]) }
        pure #[]
      for it in items do
        try
          let (status, id) ← importOne cl headers it
          t := match status with
            | "stored" => { t with stored := t.stored + 1 }
            | "duplicate" => { t with duplicate := t.duplicate + 1 }
            | "blocked" => { t with blocked := t.blocked + 1 }
            | _ => t
          t := { t with results := t.results.push (Json.mkObj [("source", Json.str it.label), ("status", Json.str status), ("id", Json.str id)]) }
        catch e =>
          if e.code == 2 then throw e
          err s!"leanmail: {it.label}: {e.msg}"
          t := { t with failed := t.failed + 1, results := t.results.push (Json.mkObj [("source", Json.str it.label), ("error", Json.str e.msg)]) }
  if c.json then
    out (Json.mkObj [("stored", Json.num t.stored), ("duplicate", Json.num t.duplicate),
      ("blocked", Json.num t.blocked), ("failed", Json.num t.failed), ("results", Json.arr t.results)]).compress
  else
    let total := t.stored + t.duplicate + t.blocked + t.failed
    out s!"imported {total} message{if total == 1 then "" else "s"}: stored {t.stored}, duplicate {t.duplicate}, blocked {t.blocked}, failed {t.failed}"
  return if t.failed > 0 then 1 else 0

end Cli.Commands
