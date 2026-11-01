import Lean.Data.Json
import Core.Base64
import Core.Mime.Date
import Core.Ulid
import Cli.Context
import Cli.Format

/-! # Acting on mail: `action`, `draft`, `send`

`action <id> <verb>` is `PATCH /api/msg/:id {action, until?}`. `draft` reads and writes
`/api/drafts`, compare-and-swap on the revision (`If-Match`). `send` is `POST /api/send` with a
fresh ULID `client_id` (or `--client-id` to retry one). The server answers a send 200 whether the
wire took it or not; a `failed` send prints its id and status and then exits 1. -/

namespace Cli.Commands

open Lean Cli

/-! ## Shared -/

/-- Everything on stdin. -/
partial def readStdin : IO String := do
  let h ← IO.getStdin
  let rec go (acc : String) : IO String := do
    let l ← h.getLine
    if l.isEmpty then return acc else go (acc ++ l)
  go ""

/-- A file's text, or stdin for `-`. -/
def readTextArg (p : String) : CliM String :=
  if p == "-" then liftIO readStdin
  else liftIO (IO.FS.readFile p) (pre := s!"cannot read {p}: ")

def mimeOf (name : String) : String :=
  match (System.FilePath.mk name).extension.map (·.toLower) with
  | some "pdf" => "application/pdf"
  | some "txt" => "text/plain"
  | some "md" => "text/markdown"
  | some "html" | some "htm" => "text/html"
  | some "csv" => "text/csv"
  | some "json" => "application/json"
  | some "ics" => "text/calendar"
  | some "eml" => "message/rfc822"
  | some "png" => "image/png"
  | some "jpg" | some "jpeg" => "image/jpeg"
  | some "gif" => "image/gif"
  | some "webp" => "image/webp"
  | some "svg" => "image/svg+xml"
  | some "zip" => "application/zip"
  | some "gz" => "application/gzip"
  | some "doc" => "application/msword"
  | some "docx" => "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  | some "xlsx" => "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  | _ => "application/octet-stream"

/-- The server's cap on attachment bytes per message. -/
def attachMax : Nat := 3 * 1024 * 1024

/-- `--attach` paths as `[{filename, mime, data_b64}]`. -/
def attachmentsJson (paths : List String) : CliM (Array Json) := do
  if paths.length > 10 then usageError "at most 10 attachments"
  let mut out := #[]
  let mut total := 0
  for p in paths do
    let b ← liftIO (IO.FS.readBinFile p) (pre := s!"cannot read {p}: ")
    total := total + b.size
    let name := (System.FilePath.mk p).fileName.getD "attachment"
    out := out.push (Json.mkObj [("filename", Json.str name), ("mime", Json.str (mimeOf name)),
      ("data_b64", Json.str (Core.Base64.encode b))])
  if total > attachMax then fail s!"attachments total {humanSize total}, over the {humanSize attachMax} limit"
  return out

def strArray (xs : List String) : Json := Json.arr (xs.toArray.map Json.str)

/-- An object's fields, `k` replaced (or added) by `v`. -/
def setField (j : Json) (k : String) (v : Json) : Json :=
  match j with
  | .obj kvs => Json.mkObj ((kvs.toArray.toList.filter (·.1 != k)) ++ [(k, v)])
  | _ => Json.mkObj [(k, v)]

def dropFields (j : Json) (ks : List String) : Json :=
  match j with
  | .obj kvs => Json.mkObj (kvs.toArray.toList.filter (!ks.contains ·.1))
  | other => other

/-- Print a send outcome (`{id, send_status, send_error, …}`); exit 1 when it failed. -/
def sendOutcome (c : Ctx) (r : Core.Http.Response) (retry : String) : CliM UInt32 := do
  let j ← parseJson r
  if c.json then printBody (bodyString r)
  else out s!"{jStr j "id"}  {jStr j "send_status"}{if jBool j "idempotent" then "  (already sent with this client id)" else ""}"
  if jStr j "send_status" == "failed" then
    let e := jStr j "send_error"
    fail s!"send failed: {if e.isEmpty then "unknown error" else e}; {retry}"
  return 0

/-! ## `action` -/

def actionVerbs : List String :=
  ["done", "inbox", "snooze", "unsnooze", "toread", "untoread", "junk", "notjunk", "forever"]

/-- The body of a snooze: `1d`, `3d`, `week` are the server's shortcuts; else epoch ms or a date
(ISO 8601 or RFC 5322). -/
def snoozeBody (w : String) : Except String Json :=
  match w.toLower with
  | "1d" => .ok (Json.mkObj [("action", Json.str "snooze_1d")])
  | "3d" => .ok (Json.mkObj [("action", Json.str "snooze_3d")])
  | "week" => .ok (Json.mkObj [("action", Json.str "snooze_week")])
  | _ =>
    let t := match w.toNat? with
      | some n => some n
      | none => Core.Mime.Date.parse? w
    match t with
    | some ms => .ok (Json.mkObj [("action", Json.str "snooze"), ("until", Json.num ms)])
    | none => .error s!"snooze wants 1d, 3d, week, epoch ms or a date like 2026-11-02T09:00Z; got '{w}'"

/-- `archived, snoozed until …` from a list item. -/
def stateWords (m : Json) : String :=
  let ws := (if (jNat? m "archived_at").isSome then ["archived"] else []) ++
    ((jNat? m "snoozed_until").map fun t => [s!"snoozed until {shortDate t} UTC"]).getD [] ++
    (if (jNat? m "to_read_at").isSome then ["to read"] else []) ++
    (if !(jStr m "junk_kind").isEmpty then [s!"junk ({jStr m "junk_kind"})"] else []) ++
    (if (jNat? m "expires_at").isNone then ["kept forever"] else [])
  if ws.isEmpty then "inbox" else ", ".intercalate ws

def actionUsage : String :=
  "action <id> done|inbox|snooze <1d|3d|week|date>|unsnooze|toread|untoread|junk|notjunk|forever"

def actionCmd (c : Ctx) : CliM UInt32 := do
  c.only []
  let ps ← c.arity 2 3 actionUsage
  let id := ps[0]!
  let verb := ps[1]!
  let body ← match verb, ps[2]? with
    | "snooze", some w => match snoozeBody w with
      | .ok b => pure b
      | .error e => usageError e
    | "snooze", none => usageError s!"usage: leanmail {actionUsage}"
    | v, none => if actionVerbs.contains v then pure (Json.mkObj [("action", Json.str v)])
                 else usageError s!"unknown action '{v}'; usage: leanmail {actionUsage}"
    | _, some _ => usageError s!"usage: leanmail {actionUsage}"
  let r ← (← c.client).jsonReq "PATCH" s!"/api/msg/{percentEncode id}" body
  if c.json then printBody (bodyString r)
  else
    let m ← parseJson r
    out s!"{jStr m "id"}  {verb}: {stateWords m}"
  return 0

/-! ## `draft` -/

def draftUsage : String :=
  "draft new [--count n] | list | show <id> | save <id> --file <json|-> [--revision n] [--attach path…] | delete <id> [--revision n] | send <id> [--revision n]"

/-- `<id>  r<rev>  <state>  <updated>  <to>  <subject>`. -/
def draftLine (st : Style) (d : Json) : String :=
  let to := ", ".intercalate ((jArr d "to").toList.filterMap fun | .str s => some s | _ => none)
  let subj := oneLine (jStr d "subject")
  let rev := ((jNat? d "revision").map toString).getD "?"
  let upd := ((jNat? d "updated_at").map shortDate).getD (fit 16 "")
  s!"{st.dim (jStr d "id")}  r{rev}  {fit 7 (jStr d "state")}  {upd}  {st.bold (fit 24 to)}  {if subj.isEmpty then "(no subject)" else subj}"

def printDraft (st : Style) (d : Json) : CliM Unit := do
  let strs (k : String) := ", ".intercalate ((jArr d k).toList.filterMap fun | .str s => some s | _ => none)
  let row (k v : String) : CliM Unit := do
    unless v.isEmpty do out s!"{st.bold (k ++ ":")}{String.ofList (List.replicate (13 - k.length) ' ')}{oneLine v}"
  row "Id" (jStr d "id")
  row "Revision" (((jNat? d "revision").map toString).getD "")
  row "State" (jStr d "state")
  row "From" (jStr d "from")
  row "To" (strs "to")
  row "Cc" (strs "cc")
  row "Bcc" (strs "bcc")
  row "Subject" (jStr d "subject")
  row "Reply-Of" (jStr d "reply_of")
  row "Forward-Of" (jStr d "forward_of")
  row "Updated" (((jNat? d "updated_at").map (fun t => shortDate t ++ " UTC")).getD "")
  row "Send-Status" (jStr d "send_status")
  row "Message" (jStr d "message_id")
  let atts := jArr d "attachments"
  unless atts.isEmpty do
    out s!"{st.bold "Attachments:"}"
    for a in atts do out s!"  {oneLine (jStr a "filename")}  {jStr a "mime"}  {humanSize ((jNat? a "size").getD 0)}"
  out ""
  printBody (jStr d "text")

/-- The draft's current revision, `none` when the server holds no such draft. -/
def currentRevision (cl : Client) (id : String) : CliM (Option Nat) := do
  let path := s!"/api/drafts/{percentEncode id}"
  let r ← cl.send "GET" path
  if r.status == 404 then return none
  let r ← check "GET" path r
  return jNat? (← parseJson r) "revision"

/-- Exit 1 naming the revision the server holds on a 409. -/
def checkDraft (method path id : String) (r : Core.Http.Response) : CliM Core.Http.Response := do
  if r.status == 409 then
    match Json.parse (bodyString r) with
    | .ok j =>
      if jStr j "error" == "revision" then
        let rev := ((jNat? j "revision").map toString).getD "?"
        fail s!"draft {id}: the server holds revision {rev}; `leanmail draft show {id}` and pass --revision {rev}"
      if jStr j "error" == "state" then
        fail s!"draft {id} is {jStr j "state"}, not editable"
    | .error _ => pure ()
  check method path r

def draftCmd (c : Ctx) : CliM UInt32 := do
  let usage : CliM UInt32 := usageError s!"usage: leanmail {draftUsage}"
  match c.args.positional with
  | ["new"] =>
    c.only ["count"]
    let n := (← c.natFlag "count").getD 1
    if n == 0 || n > 100 then usageError "--count is 1 to 100"
    let mut ids := #[]
    for _ in [0:n] do ids := ids.push (← liftIO newUlid)
    if c.json then out (Json.mkObj [("ids", strArray ids.toList)]).compress
    else for i in ids do out i
    return 0
  | ["list"] =>
    c.only []
    let r ← (← c.client).get "/api/drafts"
    if c.json then printBody (bodyString r)
    else for d in jArr (← parseJson r) "items" do out (draftLine c.style d)
    return 0
  | ["show", id] =>
    c.only []
    let r ← (← c.client).get s!"/api/drafts/{percentEncode id}"
    if c.json then printBody (bodyString r) else printDraft c.style (← parseJson r)
    return 0
  | ["save", id] =>
    c.only ["file", "revision", "attach"]
    let some file := c.args.flag? "file" | usageError "draft save needs --file <json|->"
    let j ← match Json.parse (← readTextArg file) with
      | .ok j@(.obj _) => pure j
      | .ok _ => usageError s!"{file}: the draft must be a JSON object"
      | .error e => usageError s!"{file} is not JSON: {e}"
    let cl ← c.client
    if !Core.Ulid.isValid id then usageError s!"'{id}' is not a draft id (a ULID; `leanmail draft new` makes one)"
    -- the revision this write edits: --revision, else the file's, else whatever the server holds now
    let rev ← match ← c.natFlag "revision" with
      | some r => pure (some r)
      | none => match jNat? j "revision" with
        | some r => pure (some r)
        | none => currentRevision cl id
    let mid ← match (j.getObjValAs? String "mutation_id").toOption with
      | some m => if m.isEmpty then liftIO newUlid else pure m
      | none => liftIO newUlid
    let mut body := setField (dropFields j ["id", "revision"]) "mutation_id" (Json.str mid)
    let paths := c.args.flags "attach"
    unless paths.isEmpty do
      body := setField body "attachments" (Json.arr (jArr j "attachments" ++ (← attachmentsJson paths)))
    let path := s!"/api/drafts/{percentEncode id}"
    let r ← cl.send "PUT" path [] ([("Content-Type", "application/json")] ++ ifMatch rev) body.compress.toUTF8
    let r ← checkDraft "PUT" path id r
    if c.json then printBody (bodyString r)
    else
      let d ← parseJson r
      out s!"{jStr d "id"}  revision {((jNat? d "revision").map toString).getD "?"}  {if r.status == 201 then "created" else "saved"}"
    return 0
  | ["delete", id] =>
    c.only ["revision"]
    let path := s!"/api/drafts/{percentEncode id}"
    let cl ← c.client
    let r ← cl.send "DELETE" path [] (ifMatch (← c.natFlag "revision"))
    let r ← checkDraft "DELETE" path id r
    if c.json then printBody (bodyString r) else out s!"deleted {id}"
    return 0
  | ["send", id] =>
    c.only ["revision"]
    let path := s!"/api/drafts/{percentEncode id}/send"
    let cl ← c.client
    let r ← cl.send "POST" path [] ([("Content-Type", "application/json")] ++ ifMatch (← c.natFlag "revision")) "{}".toUTF8
    let r ← checkDraft "POST" path id r
    sendOutcome c r s!"the draft is editable again; `leanmail draft send {id}` retries"
  | _ => usage

/-! ## `send` -/

def sendUsage : String :=
  "send --to a [--to b] [--cc c] [--bcc d] --subject s --body-file <f|-> [--from x] [--reply-of id] [--forward-of id] [--attach path…] [--client-id ulid]"

def sendCmd (c : Ctx) : CliM UInt32 := do
  c.only ["to", "cc", "bcc", "subject", "body-file", "html-file", "from", "reply-of", "forward-of", "attach", "client-id"]
  discard <| c.arity 0 0 sendUsage
  let to := c.args.flags "to"
  let cc := c.args.flags "cc"
  let bcc := c.args.flags "bcc"
  if to.isEmpty && cc.isEmpty && bcc.isEmpty then usageError s!"send needs a recipient; usage: leanmail {sendUsage}"
  let text ← match c.args.flag? "body-file" with
    | some f => readTextArg f
    | none => pure ""
  let html ← match c.args.flag? "html-file" with
    | some f => pure (some (← readTextArg f))
    | none => pure none
  let cid ← match c.args.flag? "client-id" with
    | some v => if Core.Ulid.isValid v then pure v else usageError s!"--client-id '{v}' is not a ULID"
    | none => liftIO newUlid
  let atts ← attachmentsJson (c.args.flags "attach")
  let opt (k flag : String) : List (String × Json) :=
    ((c.args.flag? flag).map fun v => [(k, Json.str v)]).getD []
  let body := Json.mkObj <| [("client_id", Json.str cid), ("to", strArray to), ("cc", strArray cc), ("bcc", strArray bcc),
    ("subject", Json.str ((c.args.flag? "subject").getD "")), ("text", Json.str text)] ++
    (html.map fun h => [("html", Json.str h)]).getD [] ++
    opt "from" "from" ++ opt "reply_of" "reply-of" ++ opt "forward_of" "forward-of" ++
    (if atts.isEmpty then [] else [("attachments", Json.arr atts)])
  let r ← (← c.client).postJson "/api/send" body
  sendOutcome c r s!"retry with --client-id {cid}"

end Cli.Commands
