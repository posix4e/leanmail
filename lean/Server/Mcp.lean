import Lean.Data.Json
import Core.Mcp
import Core.Ulid
import Core.Base64
import Core.Version
import Core.Mime.Date
import Server.Router
import Server.Api
import Server.Actions
import Server.Drafts
import Server.Send
import Server.Expiry
import Server.Routing
import Server.Health
import Server.Account
import Server.AI
import Server.Clock
import Server.Crypto
import Server.Log

/-! # MCP over HTTP (`POST /mcp`)

The Model Context Protocol's Streamable HTTP transport, stateless: each POST carries one
JSON-RPC 2.0 message and gets one `application/json` answer (no `Mcp-Session-Id`, no event
stream). A notification or a response from the client is `202` with no body; a batch is `400`
`-32600`; an unsupported `MCP-Protocol-Version` header is `400`; on a cookie-authenticated
request an `Origin` other than `PUBLIC_ORIGIN` is `403` (the DNS-rebinding rule; a bearer-token
request may come from any origin, so a browser-based MCP client works). `GET /mcp` is the router's `405`.

The door is `.token "read"` (a bearer token, or a session); each tool then needs its own scope
(`Core.Mcp.Tool.scope`): the reads `read`, the draft writes `draft`, `send_draft` `send`, and
`mark_done`/`snooze`/`mark_to_read` `act`. The tools, their schemas and their result shapes are
the CLI's (`Cli.Mcp`, which runs the same tools against the HTTP API), so `leanmail mcp --proxy`
is a drop-in for `--local`; here they call the route handlers directly. A refusal or failure is a
result with `isError: true` and the JSON error as its text.

Privileged mail (`Server.AI.withheldSql`) is withheld: `get_message` and `get_attachment` answer
`{id, withheld: true}`, the lists carry such entries in place of the message (and a `withheld`
count), and `account_status` reports the counts. Every `tools/call` is logged to `mcp_log`
(time, token id, tool, ok, milliseconds), never its arguments. -/

namespace Server.Mcp

open Lean Server.Db Server.Router Core.Mcp

abbrev ToolM := ExceptT String IO

/-- Fail with a JSON error object as the text. -/
def failJ (j : Json) : ToolM α := throw j.compress
def failMsg (msg : String) (extra : List (String × Json) := []) : ToolM α :=
  failJ (Json.mkObj ([("error", Json.str msg)] ++ extra))

def need (a : Json) (k : String) : ToolM String :=
  match argStr? a k with
  | some s => if s.isEmpty then failMsg s!"argument '{k}' is empty" else pure s
  | none => failMsg s!"missing argument '{k}'"

/-- A route handler's JSON answer, or its error body (with the status) as the failure. -/
def viaRoute (c : Ctx) (h : Ctx → Req → IO Resp) (params : List (String × String) := [])
    (query : List (String × String) := []) (headers : List (String × String) := []) (body : Option Json := none) :
    ToolM Json := do
  let req : Req := { method := "POST", path := [], query, headers := headers.map fun (k, v) => (k.toLower, v),
                     body := (body.map (·.compress.toUTF8)).getD .empty }
  let r ← (h { c with params } req : IO Resp)
  let text := r.bodyText
  let j := (Json.parse text).toOption
  if 200 ≤ r.status && r.status < 300 then
    if r.body.isEmpty then return Json.mkObj [("ok", Json.bool true)]
    match j with
    | some j => return j
    | none => failMsg "the answer is not JSON"
  match j with
  | some (.obj kvs) => failJ (Json.mkObj (kvs.toArray.toList ++ [("status", Json.num r.status)]))
  | _ => failMsg (if text.isEmpty then s!"status {r.status}" else text) [("status", Json.num r.status)]

def setField (j : Json) (k : String) (v : Json) : Json :=
  match j with
  | .obj kvs => Json.mkObj ((kvs.toArray.toList.filter (·.1 != k)) ++ [(k, v)])
  | _ => Json.mkObj [(k, v)]

def dropFields (j : Json) (ks : List String) : Json :=
  match j with
  | .obj kvs => Json.mkObj (kvs.toArray.toList.filter (!ks.contains ·.1))
  | other => other

/-! ## Withholding -/

def withheldJson (id : String) : Json := Json.mkObj [("id", Json.str id), ("withheld", Json.bool true)]

/-- Which of these ids are withheld. -/
def withheldIds (st : State) (ids : Array String) : IO (List String) := do
  if ids.isEmpty then return []
  let marks := ", ".intercalate (ids.toList.map fun _ => "?")
  let rows ← st.db.query s!"SELECT m.id FROM messages m WHERE m.id IN ({marks}) AND {AI.withheldSql}" (ids.map fun i => (.text i : Value))
  return rows.toList.map (·.text "id")

/-- A list answer `{items, next}` with withheld entries masked, plus `withheld` when any were. -/
def listJson (st : State) (rows : Array Row) (next : Option String) : IO Json := do
  let rows ← Api.withTriage st rows
  let held ← withheldIds st (rows.map (·.text "id"))
  let items := rows.map fun r => if held.contains (r.text "id") then withheldJson (r.text "id") else Api.listItem r
  return Json.mkObj ([("items", Json.arr items), ("next", match next with | some n => Json.str n | none => Json.null)] ++
    (if held.isEmpty then [] else [("withheld", Json.num held.length)]))

/-! ## The tools -/

def searchMail (c : Ctx) (a : Json) : ToolM Json := do
  let q ← need a "query"
  let page ← (Search.runQuery c.st (Core.Query.parse q) (clampLimit a 20 100) 0 none : IO _)
  listJson c.st page.rows page.next

def recentMail (c : Ctx) (a : Json) : ToolM Json := do
  let f := (argStr? a "filter").getD "inbox"
  unless filters.contains f do failMsg s!"filter is one of {", ".intercalate filters}"
  let now ← (Clock.nowMs : IO _)
  let some (cs, args) := Api.filterClauses f now | failMsg "bad filter"
  let (cs, args) := if f == "inbox" then (cs.push "m.folder = ?", args.push (.text "inbox")) else (cs, args)
  let page ← (Search.runQuery c.st {} (clampLimit a 20 100) 0 none (cs, args) : IO _)
  listJson c.st page.rows page.next

def getMessage (c : Ctx) (a : Json) : ToolM Json := do
  let id ← need a "id"
  if ← (AI.isWithheld c.st id : IO _) then return withheldJson id
  let m ← viaRoute c Api.msg [("id", id)]
  let m := dropFields m ["raw_key"]
  if !argBool a "include_html" then return m
  let r ← (Api.msgHtml { c with params := [("id", id)] } { method := "GET", path := [] } : IO _)
  return setField m "html" (if r.status == 200 then Json.str r.bodyText else Json.null)

def getAttachment (c : Ctx) (a : Json) : ToolM Json := do
  let id ← need a "id"
  let some idx := argNat? a "idx" | failMsg "missing argument 'idx'"
  let off := (argNat? a "offset").getD 0
  if ← (AI.isWithheld c.st id : IO _) then return withheldJson id
  let some (_, raw) ← (Api.loadRaw c.st id : IO _) | failMsg "not found" [("status", Json.num 404)]
  let some att := (Core.Mime.parse raw).attachments[idx]? | failMsg "not found" [("status", Json.num 404)]
  let size := att.content.size
  if off > size then failMsg s!"offset {off} is past the end ({size} bytes)"
  let hi := Nat.min size (off + chunkMax)
  let chunk := att.content.extract off hi
  return Json.mkObj [("id", Json.str id), ("idx", Json.num idx),
    ("filename", Json.str (Api.sanitizeFilename (att.filename.getD ""))),
    ("mime", Json.str (Api.safeMime att.mimeType)),
    ("size", Json.num size), ("offset", Json.num off), ("length", Json.num chunk.size),
    ("next_offset", if hi < size then Json.num hi else Json.null),
    ("data_b64", Json.str (Core.Base64.encode chunk))]

def expiringSoon (c : Ctx) (a : Json) : ToolM Json := do
  let days := Nat.min 90 ((argNat? a "days").getD 7)
  let j ← (Expiry.expiringJson c.st (← Clock.nowMs) days : IO _)
  let msgs := (j.getObjValAs? (Array Json) "messages").toOption.getD #[]
  let held ← (withheldIds c.st (msgs.filterMap fun m => (m.getObjValAs? String "id").toOption) : IO _)
  if held.isEmpty then return j
  let masked := msgs.map fun m => match (m.getObjValAs? String "id").toOption with
    | some i => if held.contains i then withheldJson i else m
    | none => m
  return setField (setField j "messages" (Json.arr masked)) "withheld" (Json.num held.length)

def accountStatus (c : Ctx) (_ : Json) : ToolM Json := do
  let st := c.st
  let health ← (Health.json st : IO _)
  let acct ← (Account.get c { method := "GET", path := [] } : IO _)
  let acctJ := if acct.status == 200 then (Json.parse acct.bodyText).toOption.getD Json.null else Json.null
  let drafts ← (Drafts.listAll st : IO _)
  let (own, all) ← (AI.withheldCounts st : IO _)
  return Json.mkObj [("server", Json.str st.config.origin), ("health", health), ("account", acctJ),
    ("drafts", Json.num drafts.size), ("client", Json.str s!"leanmail-server/{Core.version}"),
    ("privileged_withheld", Json.mkObj [("privileged", Json.num own), ("withheld", Json.num all)])]

def newDraftIds (_ : Ctx) (a : Json) : ToolM Json := do
  let n := Nat.min 20 (Nat.max 1 ((argNat? a "count").getD 1))
  let mut ids := #[]
  for _ in [0:n] do
    ids := ids.push (Json.str (Core.Ulid.encode (← (Clock.nowMs : IO _)) (← (Crypto.randomBytes 10 : IO _))))
  return Json.mkObj [("ids", Json.arr ids)]

def ifMatch (rev : Option Nat) : List (String × String) := (rev.map fun r => [("If-Match", toString r)]).getD []

def freshUlid : IO String := do return Core.Ulid.encode (← Clock.nowMs) (← Crypto.randomBytes 10)

def saveDraft (c : Ctx) (a : Json) : ToolM Json := do
  let id ← need a "id"
  let mid ← match argStr? a "mutation_id" with
    | some m => if m.isEmpty then (freshUlid : IO _) else pure m
    | none => (freshUlid : IO _)
  let body := setField (dropFields a ["id", "revision"]) "mutation_id" (Json.str mid)
  viaRoute c Drafts.put [("id", id)] (headers := ifMatch (argNat? a "revision")) (body := some body)

def sendDraft (c : Ctx) (a : Json) : ToolM Json := do
  let id ← need a "id"
  let some rev := argNat? a "revision" | failMsg "missing argument 'revision' (the revision you reviewed)"
  let o ← viaRoute c Drafts.sendPost [("id", id)] (headers := ifMatch (some rev)) (body := some (Json.mkObj []))
  if (o.getObjValAs? String "send_status").toOption == some "failed" then failJ (setField o "error" (Json.str "send failed"))
  return o

def action (name : String) (c : Ctx) (a : Json) : ToolM Json := do
  let id ← need a "id"
  viaRoute c Actions.patch [("id", id)] (body := some (Json.mkObj [("action", Json.str name)]))

/-- `1d`, `3d`, `week`, epoch ms or an ISO 8601 date, as the action body. -/
def snoozeBody (w : String) : Except String Json :=
  match w.toLower with
  | "1d" => .ok (Json.mkObj [("action", Json.str "snooze_1d")])
  | "3d" => .ok (Json.mkObj [("action", Json.str "snooze_3d")])
  | "week" => .ok (Json.mkObj [("action", Json.str "snooze_week")])
  | _ =>
    match (match w.toNat? with | some n => some n | none => Core.Mime.Date.parse? w) with
    | some ms => .ok (Json.mkObj [("action", Json.str "snooze"), ("until", Json.num ms)])
    | none => .error s!"snooze wants 1d, 3d, week, epoch ms or a date like 2026-11-02T09:00Z; got '{w}'"

def snooze (c : Ctx) (a : Json) : ToolM Json := do
  let id ← need a "id"
  let u ← need a "until"
  match snoozeBody u with
  | .ok b => viaRoute c Actions.patch [("id", id)] (body := some b)
  | .error e => failMsg e

def run (c : Ctx) (name : String) (a : Json) : ToolM Json :=
  match name with
  | "search_mail" => searchMail c a
  | "get_message" => getMessage c a
  | "get_attachment" => getAttachment c a
  | "recent_mail" => recentMail c a
  | "expiring_soon" => expiringSoon c a
  | "account_status" => accountStatus c a
  | "mail_config" => viaRoute c Routing.configGet
  | "list_drafts" => viaRoute c Drafts.list
  | "get_draft" => do let id ← need a "id"; viaRoute c Drafts.get [("id", id)]
  | "new_draft_ids" => newDraftIds c a
  | "save_draft" => saveDraft c a
  | "delete_draft" => do
    let id ← need a "id"
    viaRoute c Drafts.del [("id", id)] (headers := ifMatch (argNat? a "revision"))
  | "send_draft" => sendDraft c a
  | "mark_done" => action "done" c a
  | "snooze" => snooze c a
  | "mark_to_read" => action "toread" c a
  | n => failMsg s!"unknown tool {n}"

/-! ## Scopes and the log -/

/-- A bearer token needs the scope; a session (or the dev bypass) may do anything. -/
def scopeOk (p : Principal) (scope : String) : Bool :=
  match p with
  | .token _ scopes => scopes.contains scope
  | _ => true

def tokenId? : Principal → Option String
  | .token id _ => some id
  | _ => none

def logCall (st : State) (p : Principal) (tool : String) (ok : Bool) (ms : Nat) : IO Unit := do
  try
    st.db.exec "INSERT INTO mcp_log (at, token_id, tool, ok, ms) VALUES (?, ?, ?, ?, ?)"
      #[← Clock.nowMs, (tokenId? p : Value), tool, ok, ms]
  catch e => Log.warn "mcp.log_failed" [("error", Json.str (toString e))]

/-- `tools/call`: the result, or a JSON-RPC error for an unknown tool. -/
def callTool (c : Ctx) (id : Json) (params : Json) : IO Json := do
  let name := (params.getObjValAs? String "name").toOption.getD ""
  let args := match params.getObjVal? "arguments" with
    | .ok j@(.obj _) => j
    | _ => Json.mkObj []
  let some t := find? name | return rpcError id (-32602) s!"Unknown tool: {name}"
  let t0 ← IO.monoMsNow
  let out : Except String Json ←
    if !scopeOk c.principal t.scope then
      pure (.error (Json.mkObj [("error", Json.str "insufficient scope"), ("scope", Json.str t.scope)]).compress)
    else
      try (run c name args).run
      catch e =>
        Log.error "mcp.tool_failed" [("tool", Json.str name), ("error", Json.str (toString e))]
        pure (.error (Json.mkObj [("error", Json.str "internal error")]).compress)
  let ms := (← IO.monoMsNow) - t0
  let ok := out.isOk
  logCall c.st c.principal name ok ms
  Log.info "mcp.call" [("tool", Json.str name), ("ok", Json.bool ok), ("ms", Json.num ms)]
  return rpcResult id <| match out with
    | .ok j => textResult j.compress false
    | .error e => textResult e true

/-! ## The transport -/

def rpcResp (status : Nat) (j : Json) : Resp := Resp.json status j

def accepted : Resp := { status := 202 }

/-- One JSON-RPC message. -/
def handle (c : Ctx) (msg : Json) : IO Resp := do
  let id? := idOf msg
  let some method := (msg.getObjValAs? String "method").toOption
    | -- a response to us (we send no requests) or nonsense
      if (msg.getObjVal? "result").isOk || (msg.getObjVal? "error").isOk then return accepted
      return rpcResp 400 (rpcError (id?.getD Json.null) (-32600) "Invalid Request")
  let params := (msg.getObjVal? "params").toOption.getD (Json.mkObj [])
  let some id := id? | return accepted  -- notifications (notifications/initialized, cancelled, …)
  match method with
  | "initialize" => return Resp.ok (rpcResult id (initializeResult params Core.version))
  | "ping" => return Resp.ok (rpcResult id (Json.mkObj []))
  | "tools/list" => return Resp.ok (rpcResult id (Json.mkObj [("tools", Json.arr (tools.toArray.map toolJson))]))
  | "tools/call" => return Resp.ok (← callTool c id params)
  | "resources/list" => return Resp.ok (rpcResult id (Json.mkObj [("resources", Json.arr #[])]))
  | "prompts/list" => return Resp.ok (rpcResult id (Json.mkObj [("prompts", Json.arr #[])]))
  | m => return Resp.ok (rpcError id (-32601) s!"Method not found: {m}")

private def trimSlash (s : String) : String := if s.endsWith "/" then String.ofList s.toList.dropLast else s

/-- `POST /mcp`. -/
def post (c : Ctx) (req : Req) : IO Resp := do
  -- the DNS-rebinding rule matters for a request riding the cookie; a bearer token is no ambient credential
  if let some o := (if (req.header? "authorization").isSome then none else req.header? "origin") then
    if trimSlash (Core.Text.Str.trim o) != c.st.config.origin then return Resp.error 403 "origin not allowed"
  if let some v := req.header? "mcp-protocol-version" then
    if !supportedVersions.contains (Core.Text.Str.trim v) then
      return rpcResp 400 (rpcError Json.null (-32600) s!"Unsupported MCP-Protocol-Version: {v}")
  match (String.fromUTF8? req.body).map Json.parse with
  | some (.ok j@(.obj _)) => handle c j
  | some (.ok (.arr _)) => return rpcResp 400 (rpcError Json.null (-32600) "Invalid Request: batches are not supported")
  | some (.ok _) => return rpcResp 400 (rpcError Json.null (-32600) "Invalid Request")
  | _ => return rpcResp 400 (rpcError Json.null (-32700) "Parse error")

def routes : Array Route := #[
  route "POST" "/mcp" (.token "read") post
]

end Server.Mcp
