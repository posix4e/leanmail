import Lean.Data.Json
import Core.Mime.Header
import Core.Mime.Date
import Cli.Context
import Cli.Format

/-! # Reading: `recent`, `search`, `thread`, `show`, `attachment` -/

namespace Cli.Commands

open Lean Cli

def filters : List String := ["inbox", "snoozed", "archived", "toread", "junk", "sent", "all"]

/-- Print a list answer: the server's JSON, or one line per message. -/
def listOut (c : Ctx) (r : Core.Http.Response) : CliM Unit := do
  if c.json then printBody (bodyString r)
  else printList c.style (← parseJson r)

def recent (c : Ctx) : CliM UInt32 := do
  c.only ["limit", "filter", "folder", "before"]
  discard <| c.arity 0 0 "recent [--limit n] [--filter inbox|snoozed|archived|toread|junk|sent|all]"
  let filter := (c.args.flag? "filter").getD "inbox"
  unless filters.contains filter do usageError s!"--filter is one of {", ".intercalate filters}"
  let limit ← c.natFlag "limit"
  let q := [("filter", filter)] ++ ((limit.map fun n => [("limit", toString n)]).getD []) ++
    ((c.args.flag? "folder").map fun f => [("folder", f)]).getD [] ++
    ((c.args.flag? "before").map fun b => [("before", b)]).getD []
  listOut c (← (← c.client).get "/api/msgs" q)
  return 0

def search (c : Ctx) : CliM UInt32 := do
  c.only ["limit", "offset"]
  let ps ← c.arity 1 1000 "search <query…> [--limit n] [--offset n]"
  let limit ← c.natFlag "limit"
  let offset ← c.natFlag "offset"
  let q := [("q", " ".intercalate ps)] ++ ((limit.map fun n => [("limit", toString n)]).getD []) ++
    ((offset.map fun n => [("offset", toString n)]).getD [])
  listOut c (← (← c.client).get "/api/search" q)
  return 0

def thread (c : Ctx) : CliM UInt32 := do
  c.only []
  let ps ← c.arity 1 1 "thread <thread-id | message-id>"
  let cl ← c.client
  let path := s!"/api/thread/{percentEncode ps.head!}"
  let r ← cl.send "GET" path
  -- a message id names its thread
  let r ← if r.status != 404 then check "GET" path r else
    let m ← cl.send "GET" s!"/api/msg/{percentEncode ps.head!}"
    let tid := if m.isOk then ((Json.parse (bodyString m)).toOption.map (jStr · "thread_id")).getD "" else ""
    if tid.isEmpty then check "GET" path r else cl.get s!"/api/thread/{percentEncode tid}"
  listOut c r
  return 0

private def addrList (j : Json) (k : String) : String :=
  ", ".intercalate ((jArr j k).toList.map mailbox)

/-- The headers block and the text body of `GET /api/msg/:id`. -/
def printMessage (st : Style) (m : Json) : CliM Unit := do
  let row (k v : String) : CliM Unit := do
    unless v.isEmpty do out s!"{st.bold (k ++ ":")}{String.ofList (List.replicate (13 - k.length) ' ')}{oneLine v}"
  row "From" (mailbox (jObj m "from"))
  row "To" (addrList m "to")
  row "Cc" (addrList m "cc")
  row "Reply-To" (jStr m "reply_to")
  row "Date" (((jNat? m "received_at").map (fun t => shortDate t ++ " UTC")).getD "")
  row "Subject" (jStr m "subject")
  row "Id" (jStr m "id")
  row "Thread" (jStr m "thread_id")
  row "Message-ID" (jStr m "message_id")
  row "Folder" (jStr m "folder")
  row "List-Id" (jStr m "list_id")
  let atts := jArr m "attachments"
  unless atts.isEmpty do
    out s!"{st.bold "Attachments:"}"
    for a in atts do
      let idx := ((jNat? a "idx").map toString).getD "?"
      let name := jStr a "filename"
      out s!"  [{idx}] {if name.isEmpty then "(unnamed)" else oneLine name}  {jStr a "mime"}  {humanSize ((jNat? a "size").getD 0)}{if jBool a "inline" then "  inline" else ""}"
  out ""
  printBody (jStr m "text")
  if jBool m "text_truncated" then out (st.dim "[text truncated; `leanmail show --raw` has the whole message]")

def writeStdout (b : ByteArray) : CliM Unit := do
  let h ← (IO.getStdout : IO _)
  liftIO (h.write b *> h.flush)

def showMsg (c : Ctx) : CliM UInt32 := do
  c.only [] ["html", "raw"]
  let ps ← c.arity 1 1 "show <id> [--html] [--raw]"
  let id := percentEncode ps.head!
  let cl ← c.client
  if c.args.has "html" && c.args.has "raw" then usageError "--html and --raw are exclusive"
  if c.args.has "raw" then
    writeStdout (← cl.get s!"/api/msg/{id}/raw").body
  else if c.args.has "html" then
    writeStdout (← cl.get s!"/api/msg/{id}/html").body
  else
    let r ← cl.get s!"/api/msg/{id}"
    if c.json then printBody (bodyString r) else printMessage c.style (← parseJson r)
  return 0

/-- A file name safe to create in the current directory: the last path component, no leading
dots, no control characters; `none` when nothing is left. -/
def safeLocalName (s : String) : Option String :=
  let base := ((s.splitOn "/").getLastD "").splitOn "\\" |>.getLastD ""
  let cs := base.toList.filter (fun c => c.toNat ≥ 0x20 && c.toNat != 0x7F) |>.dropWhile (fun c => c == '.' || c == ' ')
  let t := String.ofList cs
  if t.isEmpty then none else some t

/-- `filename` from `Content-Disposition`. -/
def dispositionFilename (r : Core.Http.Response) : Option String :=
  (r.header? "content-disposition").bind fun v =>
    let s := Core.Mime.Header.parseStructured v
    (s.param? "filename").bind safeLocalName

def attachment (c : Ctx) : CliM UInt32 := do
  c.only ["output"]
  let ps ← c.arity 2 2 "attachment <id> <idx> [-o file]"
  let id := ps[0]!
  let idx := ps[1]!
  if idx.toNat?.isNone then usageError s!"attachment index must be a number, got '{idx}'"
  let r ← (← c.client).get s!"/api/msg/{percentEncode id}/attachments/{idx}"
  match c.args.flag? "output" with
  | some "-" => writeStdout r.body; return 0
  | some p =>
    liftIO (IO.FS.writeBinFile p r.body) (pre := s!"cannot write {p}: ")
    if c.json then out (Json.mkObj [("path", Json.str p), ("bytes", Json.num r.body.size)]).compress
    else out s!"wrote {p} ({humanSize r.body.size})"
    return 0
  | none =>
    let name := (dispositionFilename r).getD s!"attachment-{idx}"
    if ← ((System.FilePath.mk name).pathExists : IO Bool) then
      fail s!"{name} already exists; pass -o <file> to choose where to write"
    liftIO (IO.FS.writeBinFile name r.body) (pre := s!"cannot write {name}: ")
    if c.json then out (Json.mkObj [("path", Json.str name), ("bytes", Json.num r.body.size)]).compress
    else out s!"wrote {name} ({humanSize r.body.size})"
    return 0

end Cli.Commands
