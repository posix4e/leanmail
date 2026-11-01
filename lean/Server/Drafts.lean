import Lean.Data.Json
import Core.Base64
import Core.Ulid
import Core.Text.Str
import Server.State
import Server.Router
import Server.Clock
import Server.Crypto
import Server.Send

/-! # Drafts

One row per draft with its content inline (`drafts`, migration 4): From, the typed recipient
fields (JSON arrays of strings, so an unfinished address survives), subject, text, HTML, the reply
or forward it belongs to, and attachments as `[{filename, mime, size, data_b64}]` (at most 10 and
3 MiB).

Writes are compare-and-swap on `revision`: `PUT /api/drafts/:id` names the revision it edits with
`If-Match: <revision>` or a body `revision`; another value is 409 `{error:"revision", revision}`.
A `mutation_id` the draft already holds is a repeat and answers 200 with the draft unchanged. A new
id creates the draft at revision 1. Every write gives the draft a fresh `send_id`, the
`client_id` its send will use, so a send is idempotent for exactly the revision that was reviewed.
`POST /api/drafts/:id/send` sends the current (or the named) revision through `Server.Send`. -/

namespace Server.Drafts

open Lean Server.Db Server.Router Core.Text

abbrev Fail := Send.Fail

structure StoredAtt where
  filename : String
  mime : String
  size : Nat
  dataB64 : String
  deriving Inhabited

def StoredAtt.json (a : StoredAtt) (withData : Bool) : Json :=
  Json.mkObj ([("filename", Json.str a.filename), ("mime", Json.str a.mime), ("size", Json.num a.size)] ++
    (if withData then [("data_b64", Json.str a.dataB64)] else []))

def StoredAtt.ofAttachment (a : Core.Rfc822.Attachment) : StoredAtt :=
  { filename := a.filename, mime := a.mime, size := a.data.size, dataB64 := Core.Base64.encode a.data }

def StoredAtt.toAttachment (a : StoredAtt) : Core.Rfc822.Attachment :=
  { filename := a.filename, mime := a.mime, data := Core.Base64.decode a.dataB64 }

/-- What a write sets. `attachments := none` keeps the stored ones (the autosave omits them). -/
structure Fields where
  from_ : String := ""
  to : List String := []
  cc : List String := []
  bcc : List String := []
  subject : String := ""
  text : String := ""
  html : Option String := none
  inReplyTo : Option String := none
  references : List String := []
  replyOf : Option String := none
  forwardOf : Option String := none
  attachments : Option (List StoredAtt) := none
  deriving Inhabited

/-! ## JSON -/

def strList (s : String) : List String :=
  match Json.parse s with
  | .ok (.arr xs) => xs.toList.filterMap fun | .str v => some v | _ => none
  | _ => []

def strListJson (xs : List String) : String := (Json.arr (xs.toArray.map Json.str)).compress

def attsOf (s : String) : List StoredAtt :=
  match Json.parse s with
  | .ok (.arr xs) => xs.toList.filterMap fun j => do
    let filename ← jstr? j "filename"
    let dataB64 := (jstr? j "data_b64").getD ""
    pure { filename, mime := (jstr? j "mime").getD "application/octet-stream"
           size := ((j.getObjValAs? Nat "size").toOption).getD 0, dataB64 }
  | _ => []

def attsJson (as : List StoredAtt) : String := (Json.arr (as.toArray.map (·.json true))).compress

private def optJ (o : Option String) : Json := match o with | some s => Json.str s | none => Json.null

def rowJson (r : Row) (withData : Bool) : Json := Json.mkObj [
  ("id", Json.str (r.text "id")), ("revision", Json.num (r.nat "revision")),
  ("mutation_id", Json.str (r.text "mutation_id")), ("send_id", Json.str (r.text "send_id")),
  ("state", Json.str (r.text "state")),
  ("from", Json.str (r.text "from_addr")),
  ("to", Json.arr ((strList (r.text "to_json")).toArray.map Json.str)),
  ("cc", Json.arr ((strList (r.text "cc_json")).toArray.map Json.str)),
  ("bcc", Json.arr ((strList (r.text "bcc_json")).toArray.map Json.str)),
  ("subject", Json.str (r.text "subject")), ("text", Json.str (r.text "body_text")),
  ("html", optJ (r.text? "body_html")),
  ("in_reply_to", optJ (r.text? "in_reply_to")),
  ("references", Json.arr ((strList (r.text "references_json")).toArray.map Json.str)),
  ("reply_of", optJ (r.text? "reply_of")), ("forward_of", optJ (r.text? "forward_of")),
  ("attachments", Json.arr ((attsOf (r.text "attachments")).toArray.map (·.json withData))),
  ("created_at", Json.num (r.nat "created_at")), ("updated_at", Json.num (r.nat "updated_at")),
  ("send_status", optJ (r.text? "send_status")), ("message_id", optJ (r.text? "message_id"))]

def selectSql : String :=
  "SELECT d.*, m.send_status AS send_status, m.id AS message_id FROM drafts d LEFT JOIN messages m ON m.client_id = d.send_id"

def load (st : State) (id : String) : IO (Option Row) :=
  st.db.queryOne s!"{selectSql} WHERE d.id = ? AND d.state != 'deleted'" #[id]

/-- A recipient field: an array of strings or `{name, address}` objects, or one typed string. -/
def recipientField (j : Json) (k : String) : List String :=
  match j.getObjVal? k with
  | .ok (.str s) => Send.splitRecipients s
  | .ok (.arr xs) => xs.toList.filterMap fun
    | .str s => let t := Str.trim s; if t.isEmpty then none else some t
    | x@(.obj _) =>
      let a := ((jstr? x "address").orElse fun _ => jstr? x "email").getD ""
      let n := Send.cleanName ((jstr? x "name").getD "")
      if a.isEmpty then none else some (if n.isEmpty then a else s!"{n} <{a}>")
    | _ => none
  | _ => []

def fieldsOf (j : Json) : Except Fail Fields := do
  let atts ← match j.getObjVal? "attachments" with
    | .error _ => pure none
    | .ok _ => do pure (some ((← Send.attachmentsOf j).map StoredAtt.ofAttachment))
  let references : List String := match j.getObjVal? "references" with
    | .ok (.arr xs) => xs.toList.filterMap fun | .str s => some s | _ => none
    | .ok (.str s) => (s.split Str.isSpaceLike).toList.map (·.toString) |>.filter (· ≠ "")
    | _ => []
  return { from_ := Str.trim ((jstr? j "from").getD ""), to := recipientField j "to", cc := recipientField j "cc"
           bcc := recipientField j "bcc", subject := (jstr? j "subject").getD "", text := (jstr? j "text").getD ""
           html := Send.optStr j "html", inReplyTo := Send.optStr j "in_reply_to", references
           replyOf := Send.optStr j "reply_of", forwardOf := Send.optStr j "forward_of", attachments := atts }

def checkFields (f : Fields) : Except Fail Unit := do
  if f.to.length + f.cc.length + f.bcc.length > 100 then Send.fail 400 "too_many_recipients"
  if f.text.utf8ByteSize > Send.maxTextBytes then Send.fail 413 "text_too_large"
  if (f.html.map (·.utf8ByteSize)).getD 0 > Send.maxHtmlBytes then Send.fail 413 "html_too_large"
  if f.subject.length > Send.subjectMax then Send.fail 400 "subject_too_long"
  if let some as := f.attachments then
    if as.length > Send.maxAttachments then Send.fail 400 "too_many_attachments" [("max", Json.num Send.maxAttachments)]
    if (as.map (·.size)).sum > Send.maxAttachmentBytes then
      Send.fail 413 "attachments_too_large" [("max", Json.num Send.maxAttachmentBytes)]

/-- `If-Match: 3`, `"3"` or `W/"3"`. -/
def ifMatch? (req : Req) : Option Nat := (req.header? "if-match").bind fun v =>
  let t := Str.trim v
  let t := if t.startsWith "W/" then String.ofList (t.toList.drop 2) else t
  (String.ofList (t.toList.filter (· != '"'))).toNat?

def validMutationId (m : String) : Bool :=
  !m.isEmpty && m.length ≤ 64 && m.toList.all fun c => c.isAlphanum || c == '-' || c == '_'

/-! ## Writes -/

private def newUlid : IO String := do return Core.Ulid.encode (← Clock.nowMs) (← Crypto.randomBytes 10)

/-- Create or update with CAS: the status (201 created, 200 updated or repeated) and the row. -/
def save (st : State) (id : String) (expected : Option Nat) (mutationId : String) (f : Fields) :
    IO (Except Fail (Nat × Row)) := do
  if !Core.Ulid.isValid id then return Send.fail 400 "bad_draft_id"
  if !validMutationId mutationId then return Send.fail 400 "bad_mutation_id"
  if let .error e := checkFields f then return .error e
  let id := Core.Ulid.normalize id
  let now ← Clock.nowMs
  let sendId ← newUlid
  let held ← st.db.queryOne "SELECT * FROM drafts WHERE id = ?" #[id]
  match held with
  | some h =>
    if h.text "mutation_id" == mutationId && h.text "state" != "deleted" then
      let some r ← load st id | return Send.fail 404 "draft_not_found"
      return .ok (200, r)
    if h.text "state" != "draft" then
      return Send.fail 409 "state" [("state", Json.str (h.text "state")), ("revision", Json.num (h.nat "revision"))]
    if expected != some (h.nat "revision") then
      return Send.fail 409 "revision" [("revision", Json.num (h.nat "revision"))]
    let atts := f.attachments.getD (attsOf (h.text "attachments"))
    st.db.exec "UPDATE drafts SET revision = revision + 1, mutation_id = ?, send_id = ?, from_addr = ?, to_json = ?,
        cc_json = ?, bcc_json = ?, subject = ?, body_text = ?, body_html = ?, in_reply_to = ?, references_json = ?,
        reply_of = ?, forward_of = ?, attachments = ?, updated_at = ?
      WHERE id = ? AND revision = ? AND state = 'draft'"
      #[mutationId, sendId, f.from_, strListJson f.to, strListJson f.cc, strListJson f.bcc, f.subject, f.text,
        (f.html : Value), (f.inReplyTo : Value), strListJson f.references, (f.replyOf : Value), (f.forwardOf : Value),
        attsJson atts, now, id, h.nat "revision"]
    if (← st.db.changes) == 0 then
      let cur := ((← st.db.queryOne "SELECT revision FROM drafts WHERE id = ?" #[id]).map (·.nat "revision")).getD 0
      return Send.fail 409 "revision" [("revision", Json.num cur)]
    let some r ← load st id | return Send.fail 404 "draft_not_found"
    return .ok (200, r)
  | none =>
    if expected.any (· != 0) then return Send.fail 409 "revision" [("revision", Json.num 0)]
    st.db.exec "INSERT INTO drafts (id, revision, mutation_id, send_id, state, from_addr, to_json, cc_json, bcc_json,
        subject, body_text, body_html, in_reply_to, references_json, reply_of, forward_of, attachments, created_at, updated_at)
      VALUES (?, 1, ?, ?, 'draft', ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      #[id, mutationId, sendId, f.from_, strListJson f.to, strListJson f.cc, strListJson f.bcc, f.subject, f.text,
        (f.html : Value), (f.inReplyTo : Value), strListJson f.references, (f.replyOf : Value), (f.forwardOf : Value),
        attsJson (f.attachments.getD []), now, now]
    let some r ← load st id | return Send.fail 500 "draft_not_saved"
    return .ok (201, r)

/-- Soft delete; with `expected`, only that revision. -/
def remove (st : State) (id : String) (expected : Option Nat) : IO (Except Fail Unit) := do
  let some h ← load st id | return Send.fail 404 "draft_not_found"
  if let some e := expected then
    if e != h.nat "revision" then return Send.fail 409 "revision" [("revision", Json.num (h.nat "revision"))]
  if h.text "state" == "sending" then return Send.fail 409 "state" [("state", Json.str "sending")]
  st.db.exec "UPDATE drafts SET state = 'deleted', attachments = '[]', body_text = '', body_html = NULL,
      revision = revision + 1, updated_at = ? WHERE id = ?" #[← Clock.nowMs, id]
  return .ok ()

/-- The send request a draft row makes: `client_id := send_id`. -/
def toSendRequest (r : Row) (revision : Nat) : Except Fail Send.Request := do
  return {
    clientId := r.text "send_id", from_ := r.text "from_addr"
    to := ← Send.parseRecipientStrings "to" (strList (r.text "to_json"))
    cc := ← Send.parseRecipientStrings "cc" (strList (r.text "cc_json"))
    bcc := ← Send.parseRecipientStrings "bcc" (strList (r.text "bcc_json"))
    subject := r.text "subject", text := r.text "body_text", html := r.text? "body_html"
    inReplyTo := r.text? "in_reply_to", references := strList (r.text "references_json")
    replyOf := r.text? "reply_of", forwardOf := r.text? "forward_of"
    attachments := (attsOf (r.text "attachments")).map StoredAtt.toAttachment
    draftId := some (r.text "id"), draftRevision := some revision }

/-- Send a draft (its current revision unless one is named). -/
def sendDraft (st : State) (principal : Principal) (id : String) (revision : Option Nat) : IO (Except Fail Send.Outcome) := do
  let some r ← load st id | return Send.fail 404 "draft_not_found"
  match toSendRequest r (revision.getD (r.nat "revision")) with
  | .error f => return .error f
  | .ok req => Send.send st principal req

/-! ## Routes -/

def listAll (st : State) : IO (Array Row) :=
  st.db.query s!"{selectSql} WHERE d.state IN ('draft','sending') ORDER BY d.updated_at DESC, d.id DESC LIMIT 200"

def list (c : Ctx) (_ : Req) : IO Resp := do
  let rows ← listAll c.st
  return Resp.ok (Json.mkObj [("items", Json.arr (rows.map (rowJson · false)))])

def get (c : Ctx) (_ : Req) : IO Resp := do
  let some r ← load c.st (c.param "id") | return Resp.notFound
  return Resp.ok (rowJson r true)

def put (c : Ctx) (req : Req) : IO Resp := do
  let j := req.json
  if j.isNull then return Resp.error 400 "bad json"
  let expected := (ifMatch? req) <|> (j.getObjValAs? Nat "revision").toOption
  let some mid := jstr? j "mutation_id" | return Resp.error 400 "bad_mutation_id"
  match fieldsOf j with
  | .error f => return f.resp
  | .ok f =>
    match ← save c.st (c.param "id") expected mid f with
    | .error e => return e.resp
    | .ok (status, r) => return Resp.json status (rowJson r false)

def del (c : Ctx) (req : Req) : IO Resp := do
  let expected := (ifMatch? req) <|> (req.json.getObjValAs? Nat "revision").toOption
  match ← remove c.st (c.param "id") expected with
  | .error e => return e.resp
  | .ok () => return Resp.ok (Json.mkObj [("id", Json.str (c.param "id")), ("deleted", Json.bool true)])

def sendPost (c : Ctx) (req : Req) : IO Resp := do
  let expected := (ifMatch? req) <|> (req.json.getObjValAs? Nat "revision").toOption
  match ← sendDraft c.st c.principal (c.param "id") expected with
  | .error e => return e.resp
  | .ok o => return Resp.ok o.json

def routes : Array Route := #[
  route "GET" "/api/drafts" (.token "read") list,
  route "GET" "/api/drafts/:id" (.token "read") get,
  route "PUT" "/api/drafts/:id" (.token "draft") put,
  route "DELETE" "/api/drafts/:id" (.token "draft") del,
  route "POST" "/api/drafts/:id/send" (.token "send") sendPost
]

end Server.Drafts
