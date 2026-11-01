import Lean.Data.Json

namespace Server.Log

/-! JSON lines on stdout, content-free: event names, counts, ids, durations. Never an address,
a subject or a body. -/

open Lean

private def rank : String → Nat
  | "debug" => 0 | "info" => 1 | "warn" => 2 | "error" => 3 | "off" => 4 | _ => 1

/-- The lowest level written (`LOG_LEVEL`; `Main` sets it from the config, tests raise it). -/
initialize minLevel : IO.Ref String ← IO.mkRef "info"

def setLevel (level : String) : IO Unit := minLevel.set level

def emit (level : String) (event : String) (fields : List (String × Json) := []) : IO Unit := do
  if rank level < rank (← minLevel.get) then return
  let t ← IO.monoMsNow
  let j := Json.mkObj ([("t", Json.num t), ("lvl", Json.str level), ("ev", Json.str event)] ++ fields)
  IO.println j.compress
  (← IO.getStdout).flush

def info (event : String) (fields : List (String × Json) := []) : IO Unit := emit "info" event fields
def warn (event : String) (fields : List (String × Json) := []) : IO Unit := emit "warn" event fields
def error (event : String) (fields : List (String × Json) := []) : IO Unit := emit "error" event fields

end Server.Log
