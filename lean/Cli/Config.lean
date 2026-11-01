import Lean.Data.Json
import Cli.Base

/-! # `~/.config/leanmail/config.json`

`{"server": "<origin>", "token": "lm1…", "account": "<id>", "mcp": "proxy"|"local"}`. The path is `$LEANMAIL_CONFIG` when
set, else `$XDG_CONFIG_HOME/leanmail/config.json`, else `~/.config/leanmail/config.json`. The file
holds a bearer token, so it is made 0600 before anything is written to it, in a directory made 0700.
`account` is the `x-account-id` a development server without a Worker in front expects. -/

namespace Cli

open Lean

structure Config where
  server : String := ""
  token : Option String := none
  account : Option String := none
  /-- `"proxy"` makes `leanmail mcp` forward to the server's `/mcp`; anything else is `--local`. -/
  mcp : Option String := none
  deriving Inhabited, Repr

namespace Config

def path : IO System.FilePath := do
  if let some p ← IO.getEnv "LEANMAIL_CONFIG" then
    if !p.isEmpty then return p
  if let some x ← IO.getEnv "XDG_CONFIG_HOME" then
    if !x.isEmpty then return System.FilePath.mk x / "leanmail" / "config.json"
  let home := (← IO.getEnv "HOME").getD "."
  return System.FilePath.mk home / ".config" / "leanmail" / "config.json"

private def optStr (j : Json) (k : String) : Option String :=
  match j.getObjValAs? String k with
  | .ok s => if s.isEmpty then none else some s
  | .error _ => none

def ofJson (j : Json) : Config :=
  { server := (optStr j "server").getD "", token := optStr j "token", account := optStr j "account"
    mcp := optStr j "mcp" }

def toJson (c : Config) : Json :=
  Json.mkObj <| [("server", Json.str c.server)] ++
    (c.token.map fun t => [("token", Json.str t)]).getD [] ++
    (c.account.map fun a => [("account", Json.str a)]).getD [] ++
    (c.mcp.map fun m => [("mcp", Json.str m)]).getD []

/-- The saved configuration, or the empty one when there is no file. -/
def load : CliM Config := do
  let p ← (path : IO _)
  unless ← (p.pathExists : IO Bool) do return {}
  let text ← liftIO (IO.FS.readFile p) (pre := s!"cannot read {p}: ")
  match Json.parse text with
  | .ok j => return ofJson j
  | .error e => fail s!"{p} is not JSON ({e}); fix or delete it"

def ownerOnlyFile : UInt32 := 0o600
def ownerOnlyDir : UInt32 := 0o700

def save (c : Config) : CliM System.FilePath := do
  let p ← (path : IO _)
  liftIO (pre := s!"cannot write {p}: ") do
    if let some dir := p.parent then
      if !dir.toString.isEmpty then
        let existed ← dir.pathExists
        IO.FS.createDirAll dir
        unless existed do IO.Prim.setAccessRights dir ownerOnlyDir
    -- create empty, restrict, then write: the token never sits in a world-readable file
    IO.FS.writeFile p ""
    IO.Prim.setAccessRights p ownerOnlyFile
    IO.FS.writeFile p (c.toJson.pretty ++ "\n")
  return p

end Config

/-- An origin as `scheme://host[:port]`, without a trailing slash. -/
def normalizeOrigin (s : String) : Except String String :=
  let s := String.ofList (s.toList.reverse.dropWhile (· == '/') |>.reverse)
  if s.startsWith "http://" || s.startsWith "https://" then .ok s
  else .error s!"'{s}' is not an origin; expected http(s)://host[:port]"

end Cli
