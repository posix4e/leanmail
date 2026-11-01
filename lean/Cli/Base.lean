/-! # How a command fails

A command runs in `CliM`: `IO` plus an early exit carrying the process exit code and a one-line
message for stderr. Exit 1 is a refusal or a bad answer from the server, 2 a usage error or a
server that could not be reached. -/

namespace Cli

structure Exit where
  code : UInt32
  msg : String
  deriving Inhabited

abbrev CliM := ExceptT Exit IO

def fail (msg : String) (code : UInt32 := 1) : CliM α := throw { code, msg }

def usageError (msg : String) : CliM α := fail msg 2

/-- Run an `IO` action, turning its exception into an exit. -/
def liftIO (k : IO α) (code : UInt32 := 1) (pre : String := "") : CliM α := do
  match ← (k.toBaseIO : BaseIO (Except IO.Error α)) with
  | .ok a => pure a
  | .error e => fail (pre ++ toString e) code

def out (s : String) : CliM Unit := (IO.println s : IO Unit)
def err (s : String) : CliM Unit := (IO.eprintln s : IO Unit)

/-- Whether output may carry ANSI colour (only with `--color`). -/
structure Style where
  color : Bool := false
  deriving Inhabited

def Style.wrap (st : Style) (code : String) (s : String) : String :=
  if st.color then s!"\x1b[{code}m{s}\x1b[0m" else s

def Style.bold (st : Style) := st.wrap "1"
def Style.dim (st : Style) := st.wrap "2"

end Cli
