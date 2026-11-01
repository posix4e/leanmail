import Lean.Data.Json
import Core.Version
import Cli.Context
import Cli.Format

/-! # `version`, `login`, `logout`, `health`, `status` -/

namespace Cli.Commands

open Lean Cli

def version (c : Ctx) : CliM UInt32 := do
  c.only []
  discard <| c.arity 0 0 "version"
  if c.json then out (Json.mkObj [("version", Json.str Core.version)]).compress
  else out Core.version
  return 0

private def isLoopback (origin : String) : Bool :=
  ["http://127.0.0.1", "http://localhost", "http://[::1]"].any fun p =>
    origin == p || origin.startsWith (p ++ ":") || origin.startsWith (p ++ "/")

/-- Print the `/health` record. -/
def printHealth (c : Ctx) (origin : String) (r : Core.Http.Response) : CliM Unit := do
  if c.json then printBody (bodyString r); return
  let j ← parseJson r
  out s!"server   {origin}"
  out s!"ok       {jBool j "ok"}"
  out s!"version  {jStr j "version"}"
  out s!"account  {jStr j "account"}"
  out s!"role     {jStr j "role"}"
  out s!"sqlite   {jStr j "sqlite"}{if jBool j "fts5" then " (fts5)" else " (no fts5)"}"
  if let some n := jNat? j "schema" then out s!"schema   {n}"
  if let some n := jNat? j "db_bytes" then out s!"database {humanSize n}"
  if let some n := jNat? j "uptime_ms" then out s!"uptime   {n / 1000} s"

def health (c : Ctx) : CliM UInt32 := do
  c.only []
  let ps ← c.arity 0 1 "health [server-origin]"
  let origin ← match ps with
    | [o] => match normalizeOrigin o with | .ok o => pure o | .error e => usageError e
    | _ => if c.config.server.isEmpty then usageError "usage: leanmail health <server-origin> (or log in first)"
           else pure c.config.server
  let cl : Client := { origin, timeoutMs := 15000 }
  let r ← cl.get "/health"
  printHealth c origin r
  return 0

def login (c : Ctx) : CliM UInt32 := do
  c.only ["token", "account"]
  let ps ← c.arity 1 1 "login <server-origin> [--token <lm1…>] [--account <id>]"
  let origin ← match normalizeOrigin ps.head! with | .ok o => pure o | .error e => usageError e
  let token := c.args.flag? "token" |>.filter (!·.isEmpty)
  let account := c.args.flag? "account" |>.filter (!·.isEmpty)
  if token.isSome && origin.startsWith "http://" && !isLoopback origin then
    err s!"leanmail: warning: {origin} is plain http; the token will cross the network unencrypted"
  let cfg : Config := { server := origin, token, account, mcp := c.config.mcp }
  let path ← cfg.save
  let r ← ({ origin, timeoutMs := 15000 } : Client).get "/health"
  let j ← parseJson r
  if c.json then
    out (Json.mkObj [("server", Json.str origin), ("config", Json.str path.toString), ("health", j)]).compress
  else
    out s!"saved {path}"
    out s!"{origin}: ok, version {jStr j "version"}, account {jStr j "account"}"
  return 0

def logout (c : Ctx) : CliM UInt32 := do
  c.only []
  discard <| c.arity 0 0 "logout"
  let p ← (Config.path : IO _)
  if !(← (p.pathExists : IO Bool)) then out "not logged in"; return 0
  let path ← { c.config with token := none }.save
  if c.json then out (Json.mkObj [("config", Json.str path.toString), ("token", Json.null)]).compress
  else out s!"token removed from {path}"
  return 0

/-- Health, plus whether the saved credential is accepted, plus any counts the server reports. -/
def status (c : Ctx) : CliM UInt32 := do
  c.only []
  discard <| c.arity 0 0 "status"
  let cl ← c.client
  let h ← cl.get "/health"
  let hj ← parseJson h
  let probe ← cl.send "GET" "/api/msgs" [("limit", "1"), ("filter", "all")]
  let authLine :=
    if probe.isOk then "ok" else
    let d := errorDetail probe
    s!"refused ({probe.status}{if d.isEmpty then "" else " " ++ d})"
  if c.json then
    out (Json.mkObj [("server", Json.str cl.origin), ("health", hj),
      ("auth", Json.mkObj [("ok", Json.bool probe.isOk), ("status", Json.num probe.status.toNat)])]).compress
  else
    printHealth { c with json := false } cl.origin h
    out s!"auth     {authLine}{if cl.token.isNone then " (no token saved)" else ""}"
    match hj.getObjVal? "counts" with
    | .ok (.obj kvs) =>
      for (k, v) in kvs.toArray do out s!"{fit 8 k} {v.compress}"
    | _ => pure ()
  return if probe.isOk then 0 else 1

end Cli.Commands
