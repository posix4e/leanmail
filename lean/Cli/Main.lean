import Core.Version
import Cli.Args
import Cli.Base
import Cli.Config
import Cli.Context
import Cli.Commands.Session
import Cli.Commands.Mail
import Cli.Commands.Import
import Cli.Commands.Act
import Cli.Commands.Admin
import Cli.Commands.Mcp

/-! # leanmail

The command-line client: `leanmail <command> [args] [--flag value]`. Every command takes
`--json` for the server's JSON as it came; the default is plain text. Output carries ANSI colour
only with `--color`. Exit codes: 0 ok, 1 refused or failed, 2 usage or server unreachable. -/

open Cli

def usage : String := String.intercalate "\n" [
  s!"leanmail {Core.version}: the LeanMail command-line client",
  "",
  "usage: leanmail <command> [args] [--flag value]",
  "",
  "  login <server-origin> [--token <lm1…>] [--account <id>]   save the server, GET /health",
  "  logout                                                   forget the saved token",
  "  version                                                  print the client version",
  "  health [server-origin]                                   GET /health",
  "  status                                                   health, and whether the credential works",
  "  recent [--limit n] [--filter inbox|snoozed|archived|toread|junk|sent|all]",
  "  search <query…> [--limit n] [--offset n]",
  "  show <id> [--html] [--raw]                               headers and text; HTML part; raw message",
  "  thread <id>                                              the messages of a thread (thread or message id)",
  "  attachment <id> <idx> [-o file]                          save an attachment (-o - for stdout)",
  "  import <path…> [--source import|route] [--rcpt addr] [--folder inbox|sent]",
  "                                                           .eml files, directories of them, mbox files",
  "  action <id> done|inbox|snooze <1d|3d|week|date>|unsnooze|toread|untoread|junk|notjunk|forever",
  "  draft new [--count n] | list | show <id> | delete <id> [--revision n]",
  "  draft save <id> --file <json|-> [--revision n] [--attach path…]   create or update (never sends)",
  "  draft send <id> [--revision n]                           send the draft; exit 1 when the send failed",
  "  send --to a [--cc b] [--bcc c] --subject s --body-file <f|-> [--from x] [--reply-of id]",
  "       [--forward-of id] [--attach path…] [--client-id ulid]   prints <id> <send_status>",
  "  tokens list | create --name n [--scopes read,act,…] | revoke <id>",
  "  admin signups [--all] | approve <id> | reject <id> | accounts | disable <id>   (operator)",
  "  gmail status | backfill [status|start [--keep-forever]|pause|resume|cancel|retry-failed]",
  "  mcp [--local | --proxy]                                  stdio MCP server (local tools, or proxy to /mcp)",
  "",
  "every command: --json (raw JSON), --color (ANSI colour), --help",
  "config: $LEANMAIL_CONFIG, else $XDG_CONFIG_HOME/leanmail/config.json, else ~/.config/leanmail/config.json",
  ""]

def commands : List (String × (Ctx → CliM UInt32)) := [
  ("version", Commands.version),
  ("login", Commands.login),
  ("logout", Commands.logout),
  ("health", Commands.health),
  ("status", Commands.status),
  ("recent", Commands.recent),
  ("search", Commands.search),
  ("show", Commands.showMsg),
  ("thread", Commands.thread),
  ("attachment", Commands.attachment),
  ("import", Commands.importCmd),
  ("action", Commands.actionCmd),
  ("draft", Commands.draftCmd),
  ("send", Commands.sendCmd),
  ("tokens", Commands.tokensCmd),
  ("admin", Commands.adminCmd),
  ("gmail", Commands.gmailCmd),
  ("mcp", Commands.mcpCmd)]

def run (argv : List String) : CliM UInt32 := do
  match argv with
  | [] => (IO.eprint usage : IO Unit); return 2
  | cmd :: rest =>
    if cmd == "--help" || cmd == "-h" || cmd == "help" then (IO.eprint usage : IO Unit); return 2
    let some (_, f) := commands.find? (·.1 == cmd)
      | (IO.eprint (s!"leanmail: unknown command '{cmd}'\n\n" ++ usage) : IO Unit); return 2
    let args ← match parseArgs rest with
      | .ok a => pure a
      | .error e => usageError e
    if args.has "help" || args.has "h" then (IO.eprint usage : IO Unit); return 2
    -- `version` and `health <origin>` work with no config, or a broken one
    let config ← if cmd == "version" then pure {} else Config.load
    f { args, json := args.has "json", style := { color := args.has "color" }, config }

def main (argv : List String) : IO UInt32 := do
  match ← (run argv).run with
  | .ok code => return code
  | .error e =>
    IO.eprintln s!"leanmail: {e.msg}"
    return e.code
