import Cli.Args
import Cli.Base
import Cli.Config
import Cli.Client

/-! # What every command is handed -/

namespace Cli

structure Ctx where
  args : Args
  /-- `--json`: print the server's JSON as it came. -/
  json : Bool
  style : Style
  config : Config

def Ctx.client (c : Ctx) : CliM Client := Client.ofConfig c.config

def Ctx.natFlag (c : Ctx) (name : String) : CliM (Option Nat) :=
  match c.args.natFlag? name with
  | .ok v => pure v
  | .error e => usageError e

/-- Exit 2 unless the command got between `lo` and `hi` positional words. -/
def Ctx.arity (c : Ctx) (lo hi : Nat) (shape : String) : CliM (List String) := do
  let ps := c.args.positional
  if ps.length < lo || ps.length > hi then usageError s!"usage: leanmail {shape}"
  return ps

/-- Exit 2 on a flag the command does not take. -/
def Ctx.only (c : Ctx) (named : List String) (switches : List String := []) : CliM Unit := do
  for (k, _) in c.args.named do
    unless named.contains k do usageError s!"unknown flag --{k}"
  for k in c.args.switches do
    unless (switches ++ ["json", "color"]).contains k do usageError s!"unknown flag --{k}"

end Cli
