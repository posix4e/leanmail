import Lean.Data.Json
import Core.Mime
import Core.Ulid
import Server.State
import Server.Router
import Server.Search
import Server.Spool
import Server.Ingest
import Server.Clock
import Server.Crypto
import Server.Log

/-! # The JSON API (M1): list, search, read, raw, attachments, threads, import, wake, tick -/

namespace Server.Api

open Lean Server.Db Server.Router

/-! ## Rows to JSON -/

def optNat (r : Row) (k : String) : Json := match r.nat? k with | some n => Json.num n | none => Json.null
def optText (r : Row) (k : String) : Json := match r.text? k with | some s => Json.str s | none => Json.null
def optStrJ (o : Option String) : Json := match o with | some s => Json.str s | none => Json.null
def jsonCol (r : Row) (k : String) (d : Json := Json.arr #[]) : Json :=
  match r.text? k with
  | some s => (Json.parse s).toOption.getD d
  | none => d

/-- The AI's triage verdict a row carries (`withTriage`), as `{label, priority}`. -/
def triageJson? (r : Row) : Option Json :=
  (r.text? "triage_label").map fun l => Json.mkObj [("label", Json.str l), ("priority", Json.num (r.nat "triage_priority"))]

/-- One entry of a list; `triage` only when the row carries a verdict. -/
def listItem (r : Row) : Json := Json.mkObj <| [
  ("id", Json.str (r.text "id")),
  ("thread_id", optText r "thread_id"),
  ("from", Json.mkObj [("name", Json.str (r.text "from_name")), ("address", Json.str (r.text "from_addr"))]),
  ("subject", Json.str (r.text "subject")),
  ("snippet", Json.str (r.text "snippet")),
  ("received_at", Json.num (r.nat "received_at")),
  ("has_attachments", Json.bool (r.bool "has_attachments")),
  ("has_invite", Json.bool (r.bool "has_invite")),
  ("archived_at", optNat r "archived_at"),
  ("snoozed_until", optNat r "snoozed_until"),
  ("to_read_at", optNat r "to_read_at"),
  ("junk_kind", optText r "junk_kind"),
  ("expires_at", optNat r "expires_at")
] ++ ((triageJson? r).map fun t => [("triage", t)]).getD []

/-- The rows with their `ai_triage` verdict added as `triage_label` and `triage_priority`
columns (one query for the page). -/
def withTriage (st : State) (rows : Array Row) : IO (Array Row) := do
  if rows.isEmpty then return rows
  let ids := rows.map (·.text "id")
  let marks := ", ".intercalate (ids.toList.map fun _ => "?")
  let ts ← st.db.query s!"SELECT message_id, label, priority FROM ai_triage WHERE message_id IN ({marks})"
    (ids.map fun i => (.text i : Value))
  if ts.isEmpty then return rows
  return rows.map fun r =>
    match ts.find? (·.text "message_id" == r.text "id") with
    | some t => { names := r.names ++ #["triage_label", "triage_priority"], cells := r.cells ++ #[t.get "label", t.get "priority"] }
    | none => r

def listJson (rows : Array Row) (next : Option String) : Json :=
  Json.mkObj [("items", Json.arr (rows.map listItem)), ("next", match next with | some n => Json.str n | none => Json.null)]

/-! ## Lists and search -/

/-- The predicates of a list view. `inbox` hides archived, junk and snoozed-for-later mail. -/
def filterClauses (filter : String) (now : Nat) : Option (Array String × Array Value) :=
  match filter with
  | "inbox" => some (#["m.archived_at IS NULL", "m.junk_at IS NULL", "(m.snoozed_until IS NULL OR m.snoozed_until <= ?)"],
                     #[.int now.toInt64])
  | "snoozed" => some (#["m.snoozed_until > ?", "m.junk_at IS NULL"], #[.int now.toInt64])
  | "archived" => some (#["m.archived_at IS NOT NULL", "m.junk_at IS NULL"], #[])
  | "toread" => some (#["m.to_read_at IS NOT NULL", "m.junk_at IS NULL"], #[])
  | "junk" => some (#["m.junk_at IS NOT NULL"], #[])
  | "sent" => some (#["m.folder = 'sent'"], #[])
  | "all" => some (#[], #[])
  | _ => none

def msgs (c : Ctx) (req : Req) : IO Resp := do
  let now ← Clock.nowMs
  let filter := (req.param? "filter").getD "inbox"
  let some (cs, args) := filterClauses filter now | return Resp.error 400 "bad filter"
  let folder := (req.param? "folder").getD (if filter == "inbox" then "inbox" else "")
  let (cs, args) := if folder.isEmpty then (cs, args) else (cs.push "m.folder = ?", args.push (.text folder))
  let page ← Search.runQuery c.st {} (req.natParam "limit" 50) 0 (req.param? "before") (cs, args)
  return Resp.ok (listJson (← withTriage c.st page.rows) page.next)

def search (c : Ctx) (req : Req) : IO Resp := do
  let q := (req.param? "q").getD ""
  let page ← Search.runQuery c.st (Core.Query.parse q) (req.natParam "limit" 50) (req.natParam "offset" 0)
    (req.param? "before")
  return Resp.ok (listJson (← withTriage c.st page.rows) page.next)

/-! ## One message -/

def messageRow (st : State) (id : String) : IO (Option Row) :=
  st.db.queryOne "SELECT * FROM messages WHERE id = ? AND deleted = 0" #[id]

def msg (c : Ctx) (_ : Req) : IO Resp := do
  let id := c.param "id"
  let some r ← messageRow c.st id | return Resp.notFound
  let text := ((← c.st.db.queryOne "SELECT body FROM message_text WHERE message_id = ?" #[id]).map (·.text "body")).getD ""
  let atts ← c.st.db.query "SELECT idx, filename, mime, size, content_id, inline FROM attachments WHERE message_id = ? ORDER BY idx" #[id]
  let attJson := atts.map fun a => Json.mkObj [
    ("idx", Json.num (a.nat "idx")), ("filename", Json.str (a.text "filename")), ("mime", Json.str (a.text "mime")),
    ("size", Json.num (a.nat "size")), ("content_id", optText a "content_id"), ("inline", Json.bool (a.bool "inline"))]
  let base := match listItem r with
    | .obj kvs => kvs.toArray.toList
    | _ => []
  return Resp.ok <| Json.mkObj <| base ++ [
    ("message_id", optText r "message_id"),
    ("source", Json.str (r.text "source")),
    ("folder", Json.str (r.text "folder")),
    ("size", Json.num (r.nat "size")),
    ("to", jsonCol r "to_json"),
    ("cc", jsonCol r "cc_json"),
    ("reply_to", optText r "reply_to"),
    ("list_id", optText r "list_id"),
    ("in_reply_to", optText r "in_reply_to"),
    ("references", jsonCol r "references_json"),
    ("rcpts", jsonCol r "rcpts"),
    ("text", Json.str text),
    ("text_truncated", Json.bool (r.bool "text_truncated")),
    ("attachments", Json.arr attJson),
    ("raw_key", optText r "raw_key")]

/-- The message's raw bytes from the store. -/
def loadRaw (st : State) (id : String) : IO (Option (Row × ByteArray)) := do
  let some r ← messageRow st id | return none
  let some key := r.text? "raw_key" | return none
  let some (raw, _) ← st.store.get key | return none
  return some (r, raw)

def htmlCsp : String := "default-src 'none'; img-src data: cid: https:; style-src 'unsafe-inline'"

/-- The HTML part, for a sandboxed iframe. -/
def msgHtml (c : Ctx) (_ : Req) : IO Resp := do
  let some (_, raw) ← loadRaw c.st (c.param "id") | return Resp.notFound
  let some html := (Core.Mime.parse raw).html | return Resp.error 404 "no html part"
  return { status := 200, body := html.toUTF8, headers := [
    ("Content-Type", "text/html; charset=utf-8"), ("Content-Security-Policy", htmlCsp),
    ("X-Content-Type-Options", "nosniff"), ("Referrer-Policy", "no-referrer")] }

def msgRaw (c : Ctx) (_ : Req) : IO Resp := do
  let some (_, raw) ← loadRaw c.st (c.param "id") | return Resp.notFound
  return { status := 200, body := raw, headers := [("Content-Type", "message/rfc822"), ("X-Content-Type-Options", "nosniff")] }

/-- A filename safe inside `filename="…"`: printable ASCII without quotes, backslashes, slashes
or control characters, at most 120 characters; `attachment` when nothing is left. -/
def sanitizeFilename (s : String) : String :=
  let cs := s.toList.map fun c =>
    if c.toNat < 0x20 || c.toNat ≥ 0x7F || c == '"' || c == '\\' || c == '/' || c == ';' then '_' else c
  let t := Core.Text.Str.trim (String.ofList (cs.take 120))
  let t := String.ofList (t.toList.dropWhile (· == '.'))
  if t.isEmpty then "attachment" else t

/-- A MIME type that is a plain `type/subtype` token, else `application/octet-stream`. -/
def safeMime (m : String) : String :=
  let ok := !m.isEmpty && m.toList.all (fun c => c.isAlphanum || c == '/' || c == '-' || c == '+' || c == '.') &&
    (m.splitOn "/").length == 2
  if ok then m.toLower else "application/octet-stream"

def msgAttachment (c : Ctx) (_ : Req) : IO Resp := do
  let some idx := (c.param "idx").toNat? | return Resp.notFound
  let some (_, raw) ← loadRaw c.st (c.param "id") | return Resp.notFound
  let some a := (Core.Mime.parse raw).attachments[idx]? | return Resp.notFound
  return { status := 200, body := a.content, headers := [
    ("Content-Type", safeMime a.mimeType),
    ("Content-Disposition", s!"attachment; filename=\"{sanitizeFilename (a.filename.getD "")}\""),
    ("Content-Security-Policy", "sandbox"), ("X-Content-Type-Options", "nosniff")] }

def thread (c : Ctx) (_ : Req) : IO Resp := do
  let rows ← c.st.db.query "SELECT * FROM messages WHERE thread_id = ? AND deleted = 0 ORDER BY received_at ASC, id ASC" #[c.param "id"]
  if rows.isEmpty then return Resp.notFound
  return Resp.ok (listJson rows none)

/-! ## Import, wake, tick -/

private def natHeader (req : Req) (k : String) : Option Nat := (req.header? k).bind fun v => (Core.Text.Str.trim v).toNat?

/-- `POST /api/import`: raw MIME in the body; the raw is stored at `raw/<acct>/<id>` first, then
ingested. The message's state comes in headers (all optional; what the old vault's exporter,
`scripts/export-to-leanmail.ts` `importHeaders`, sends):

* `x-id` — the message id (a ULID); a new one when absent, 400 when not a ULID. An id already held
  answers 200 `duplicate`.
* `x-received-at` — epoch ms; now when absent.
* `x-source` — `import` by default (`route`, `gmail`, `sent` also taken; anything else is `route`).
* `x-folder` — `sent`, else `inbox`.
* `x-rcpts` — a JSON array of recipient addresses; the first is `rcpt`, the rest are added.
* `x-expires-at` — epoch ms, or `never` (or `null`) to keep forever; the account's retention when absent.
* `x-archived-at`, `x-snoozed-until`, `x-to-read-at` — epoch ms.
* `x-junk-kind` — files it as junk (`junk_at` set to now) with this kind, e.g. `spam`.

An import never pushes to the phone (`Ingest.shouldPush`). -/
def importRaw (c : Ctx) (req : Req) : IO Resp := do
  if req.body.isEmpty then return Resp.error 400 "empty body"
  let receivedAt ← match natHeader req "x-received-at" with
    | some t => pure t
    | none => Clock.nowMs
  let id ← match req.header? "x-id" with
    | some v =>
      let v := Core.Text.Str.trim v
      if Core.Ulid.isValid v then pure (some (Core.Ulid.normalize v)) else pure none
    | none => pure (some (Core.Ulid.encode receivedAt (← Crypto.randomBytes 10)))
  let some id := id | return Resp.error 400 "bad x-id"
  let rcpts := Ingest.rcptsOf (req.header? "x-rcpts")
  let expires := (req.header? "x-expires-at").map Core.Text.Str.trim
  let keepForever := expires == some "never" || expires == some "null"
  let folder := if (req.header? "x-folder").map (·.toLower) == some "sent" then "sent" else "inbox"
  let source := ((req.header? "x-source").map Core.Text.Str.trim).getD "import"
  let junkKind := (req.header? "x-junk-kind").bind fun v =>
    let t := Core.Text.Str.trim v; if t.isEmpty then none else some t
  let key := Spool.rawKey c.account id
  c.st.store.put key req.body [("source", source), ("received_at", toString receivedAt)]
  let r ← Ingest.ingest c.st {
    raw := req.body, key := some key, rcpt := rcpts.head?, moreRcpts := rcpts.drop 1
    source, receivedAt, folder, forcedId := some id
    archivedAt := natHeader req "x-archived-at", expiresAt := expires.bind (·.toNat?), keepForever
    snoozedUntil := natHeader req "x-snoozed-until", toReadAt := natHeader req "x-to-read-at", junkKind }
  match r with
  | .duplicate dup =>
    if dup != id then c.st.store.delete key
    return Resp.json 200 (Json.mkObj [("status", Json.str "duplicate"), ("id", Json.str dup)])
  | .stored i => return Resp.json 201 (Json.mkObj [("status", Json.str "stored"), ("id", Json.str i)])
  | .blocked i => return Resp.json 201 (Json.mkObj [("status", Json.str "blocked"), ("id", Json.str i)])

def wake (c : Ctx) (_ : Req) : IO Resp := do
  let n ← Spool.drain c.st
  return Resp.ok (Json.mkObj [("drained", Json.num n)])

def tick (_ : Ctx) (_ : Req) : IO Resp := return Resp.ok (Json.mkObj [("ok", Json.bool true)])

def routes : Array Route := #[
  route "GET" "/api/msgs" (.token "read") msgs,
  route "GET" "/api/search" (.token "read") search,
  route "GET" "/api/msg/:id" (.token "read") msg,
  route "GET" "/api/msg/:id/html" (.token "read") msgHtml,
  route "GET" "/api/msg/:id/raw" (.token "read") msgRaw,
  route "GET" "/api/msg/:id/attachments/:idx" (.token "read") msgAttachment,
  route "GET" "/api/thread/:id" (.token "read") thread,
  route "POST" "/api/import" (.token "act") importRaw,
  route "POST" "/internal/wake" .internal wake,
  route "POST" "/internal/tick" .internal tick
]

end Server.Api
