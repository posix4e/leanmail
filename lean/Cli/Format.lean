import Lean.Data.Json
import Core.Mime.Date
import Cli.Base

/-! # Text output: message lines, records, sizes and dates -/

namespace Cli

open Lean

def jStr (j : Json) (k : String) : String := (j.getObjValAs? String k).toOption.getD ""
def jNat? (j : Json) (k : String) : Option Nat := (j.getObjValAs? Nat k).toOption
def jBool (j : Json) (k : String) : Bool := (j.getObjValAs? Bool k).toOption.getD false
def jArr (j : Json) (k : String) : Array Json := (j.getObjValAs? (Array Json) k).toOption.getD #[]
def jObj (j : Json) (k : String) : Json := (j.getObjVal? k).toOption.getD Json.null

private def pad2 (n : Nat) : String := if n < 10 then s!"0{n}" else toString n

/-- `YYYY-MM-DD HH:MM` in UTC. -/
def shortDate (ms : Nat) : String :=
  let days := ms / 86400000
  let rem := ms % 86400000
  let (y, m, d) := Core.Mime.Date.civilFromDays days
  s!"{y}-{pad2 m}-{pad2 d} {pad2 (rem / 3600000)}:{pad2 (rem / 60000 % 60)}"

/-- Exactly `w` characters: cut with `…` or padded with spaces. -/
def fit (w : Nat) (s : String) : String :=
  let cs := s.toList.map fun c => if c.toNat < 0x20 then ' ' else c
  if cs.length > w then String.ofList (cs.take (w - 1) ++ ['…'])
  else String.ofList (cs ++ List.replicate (w - cs.length) ' ')

/-- One line of a printable string: control characters become spaces. -/
def oneLine (s : String) : String := String.ofList (s.toList.map fun c => if c.toNat < 0x20 then ' ' else c)

/-- `Name <address>`, or whichever is present. -/
def mailbox (j : Json) : String :=
  let n := jStr j "name"
  let a := jStr j "address"
  if n.isEmpty then a else if a.isEmpty then n else s!"{n} <{a}>"

def humanSize (n : Nat) : String :=
  if n < 1024 then s!"{n} B"
  else if n < 1024 * 1024 then s!"{(n * 10 / 1024 + 5) / 10} KiB"
  else s!"{(n * 10 / (1024 * 1024)) / 10}.{(n * 10 / (1024 * 1024)) % 10} MiB"

/-- `<id>  <date>  <from, 24 cols>  [📎 ]<subject>`. -/
def messageLine (st : Style) (m : Json) : String :=
  let from_ := jObj m "from"
  let who := if (jStr from_ "name").isEmpty then jStr from_ "address" else jStr from_ "name"
  let date := ((jNat? m "received_at").map shortDate).getD (fit 16 "")
  let subj := oneLine (jStr m "subject")
  let subj := if subj.isEmpty then "(no subject)" else subj
  let clip := if jBool m "has_attachments" then "📎 " else ""
  s!"{st.dim (jStr m "id")}  {date}  {st.bold (fit 24 who)}  {clip}{subj}"

/-- Print a list answer (`{items, next}`) one message per line. -/
def printList (st : Style) (j : Json) : CliM Unit := do
  for m in jArr j "items" do out (messageLine st m)

/-- Print text verbatim, with exactly one trailing newline. -/
def printBody (s : String) : CliM Unit :=
  (IO.print (if s.endsWith "\n" then s else s ++ "\n") : IO Unit)

end Cli
