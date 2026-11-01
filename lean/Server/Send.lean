import Lean.Data.Json
import Core.Rfc822
import Core.Mime
import Core.Mime.Address
import Core.Ulid
import Core.Base64
import Core.Text.Str
import Server.State
import Server.Router
import Server.Ingest
import Server.Spool
import Server.Api
import Server.Clock
import Server.Crypto
import Server.Log
import Server.Mail.Build
import Server.Gmail.SendAs

/-! # Sending

`POST /api/send` (token `send`; a session also passes), a port of MailVault's `src/send.ts`:

1. validate the request; the `from` address must be sendable (`sendable`);
2. caps: 20 recipients across To, Cc and Bcc, 10 attachments and 3 MiB of attachment bytes, text
   at most 512 KiB, the built message at most 5 MiB, 200 sends per UTC day;
3. idempotent on `client_id`: a message already holding it is the answer, unless its send
   failed, in which case that copy is tombstoned and this attempt takes the id;
4. `reply_of` fills `In-Reply-To` and `References` from the parent, so the sent copy threads with
   it; `forward_of` carries the original's attachments, re-read from its raw message;
5. `Core.Rfc822` builds the message with our own `Message-ID: <ulid@domain of from>`;
6. the Sent copy is stored FIRST (`raw/<acct>/<id>`, then `Ingest.ingest` with source and folder
   `sent`), marked `send_status = 'sending'` with the `client_id`;
7. the Worker's `POST /internal/email/send {from, to, raw_b64}` (the wire copy has no `Bcc`), or,
   when `from` is the connected Gmail address or one of its aliases, Gmail's
   `users.messages.send` (`Server.Gmail.SendAs`) with the whole message, whose new Gmail id goes
   on the Sent copy's `gm_msgid`;
8. settle: `sent`, or `failed` with `send_error`. The answer is 200 `{id, thread_id, send_status}`
   either way, so a client can show the outcome and retry a failure with the same `client_id`.

A `draft_id` with its `draft_revision` (and `client_id` equal to the draft's `send_id`) locks the
draft for the send (409 on a mismatch); the draft becomes `sent` on success and goes back to
`draft` on failure. `Server.Drafts` builds requests from drafts and calls `send`. -/

namespace Server.Send

open Lean Server.Db Server.Router Core.Text

def maxTextBytes : Nat := 512 * 1024
def maxHtmlBytes : Nat := 1024 * 1024
def maxAttachments : Nat := 10
def maxAttachmentBytes : Nat := 3 * 1024 * 1024
def maxMimeBytes : Nat := 5 * 1024 * 1024
def maxRecipients : Nat := 20
def dailyCap : Nat := 200
def subjectMax : Nat := 998
def dayMs : Nat := 86400000

/-- A refusal: the status, a short error code and any extra fields. -/
structure Fail where
  status : Nat
  error : String
  extra : List (String × Json) := []
  deriving Inhabited

def Fail.resp (f : Fail) : Resp := Resp.json f.status (Json.mkObj ([("error", Json.str f.error)] ++ f.extra))

def fail (status : Nat) (error : String) (extra : List (String × Json) := []) : Except Fail α :=
  .error { status, error, extra }

structure Request where
  clientId : String
  /-- Empty means the account's `default_from`. -/
  from_ : String := ""
  fromName : Option String := none
  to : List Core.Rfc822.Addr := []
  cc : List Core.Rfc822.Addr := []
  bcc : List Core.Rfc822.Addr := []
  replyTo : Option Core.Rfc822.Addr := none
  subject : String := ""
  text : String := ""
  html : Option String := none
  inReplyTo : Option String := none
  references : List String := []
  replyOf : Option String := none
  forwardOf : Option String := none
  attachments : List Core.Rfc822.Attachment := []
  /-- `none`: the account's retention; `some none`: keep forever; `some (some t)`: expire at `t`. -/
  expiresAt : Option (Option Nat) := none
  draftId : Option String := none
  draftRevision : Option Nat := none
  deriving Inhabited

structure Outcome where
  id : String
  threadId : Option String
  sendStatus : String
  messageId : Option String
  idempotent : Bool := false
  sendError : Option String := none

def Outcome.json (o : Outcome) : Json := Json.mkObj [
  ("id", Json.str o.id),
  ("thread_id", match o.threadId with | some t => Json.str t | none => Json.null),
  ("send_status", Json.str o.sendStatus),
  ("message_id", match o.messageId with | some m => Json.str m | none => Json.null),
  ("idempotent", Json.bool o.idempotent),
  ("send_error", match o.sendError with | some e => Json.str e | none => Json.null)]

/-! ## Parsing -/

/-- A display name: no line breaks or `<>"`, trimmed, at most 200 characters. -/
def cleanName (n : String) : String :=
  let n := String.ofList (n.toList.filter fun c => c.toNat ≥ 0x20 && c != '<' && c != '>' && c != '"' && c != ' ' && c != ' ')
  String.ofList ((Str.trim n).toList.take 200)

/-- Split a typed recipient field at `,` or `;` outside quotes and angle brackets. -/
def splitRecipients (s : String) : List String := Id.run do
  let mut out : Array String := #[]
  let mut cur := ""
  let mut quote := false
  let mut angle := false
  for c in s.toList do
    if c == '"' then quote := !quote; cur := cur.push c
    else if !quote && c == '<' then angle := true; cur := cur.push c
    else if !quote && c == '>' then angle := false; cur := cur.push c
    else if !quote && !angle && (c == ',' || c == ';' || c == '\n') then
      out := out.push cur; cur := ""
    else cur := cur.push c
  out := out.push cur
  return out.toList.map Str.trim |>.filter (· ≠ "")

/-- One typed recipient (`Name <a@b>` or `a@b`) as an address, or the text that is not one. -/
def parseRecipient (s : String) : Except String Core.Rfc822.Addr :=
  match Core.Mime.Address.parseFlat s with
  | [.mailbox n a] =>
    match normEmail? a with
    | some e => .ok { name := cleanName n, address := e }
    | none => .error s
  | _ => .error s

/-- Recipients typed as strings (each may hold several), deduplicated. -/
def parseRecipientStrings (field : String) (xs : List String) : Except Fail (List Core.Rfc822.Addr) := do
  let mut out : Array Core.Rfc822.Addr := #[]
  for x in xs do
    for piece in splitRecipients x do
      match parseRecipient piece with
      | .ok a => if !out.any (·.address == a.address) then out := out.push a
      | .error bad => fail 400 "bad_address" [("field", Json.str field), ("value", Json.str bad)]
  return out.toList

/-- A JSON recipient list: strings, or `{address|email, name}` objects. -/
def recipientsOf (j : Json) (field : String) : Except Fail (List Core.Rfc822.Addr) := do
  match j.getObjVal? field with
  | .error _ | .ok .null => return []
  | .ok (.str s) => parseRecipientStrings field [s]
  | .ok (.arr xs) =>
    let mut out : Array Core.Rfc822.Addr := #[]
    for x in xs do
      match x with
      | .str s =>
        for a in ← parseRecipientStrings field [s] do
          if !out.any (·.address == a.address) then out := out.push a
      | .obj _ =>
        let addr := (jstr? x "address").orElse fun _ => jstr? x "email"
        match addr.bind normEmail? with
        | some e =>
          if !out.any (·.address == e) then out := out.push { name := cleanName ((jstr? x "name").getD ""), address := e }
        | none => fail 400 "bad_address" [("field", Json.str field)]
      | _ => fail 400 "bad_address" [("field", Json.str field)]
    return out.toList
  | .ok _ => fail 400 "bad_address" [("field", Json.str field)]

/-- `<id@host>`: brackets added when missing; `none` unless it looks like one message id. -/
def messageId? (s : String) : Option String :=
  let t := Str.trim s
  let inner := if t.startsWith "<" && t.endsWith ">" then String.ofList (t.toList.drop 1 |>.dropLast) else t
  if inner.isEmpty || inner.length > 250 || !(inner.contains '@') ||
     inner.toList.any (fun c => c.toNat ≤ 0x20 || c == '<' || c == '>' || c.toNat ≥ 0x7F) then none
  else some s!"<{inner}>"

def validFilename (f : String) : Bool :=
  !f.isEmpty && f.utf8ByteSize ≤ 255 && !f.toList.any (fun c => c.toNat < 0x20 || c.toNat == 0x7F || c == ' ' || c == ' ')

/-- `[{filename, mime, data_b64}]` (`type`/`content` accepted too, as MailVault named them). -/
def attachmentsOf (j : Json) : Except Fail (List Core.Rfc822.Attachment) := do
  match j.getObjVal? "attachments" with
  | .error _ | .ok .null => return []
  | .ok (.arr xs) =>
    if xs.size > maxAttachments then fail 400 "too_many_attachments" [("max", Json.num maxAttachments)]
    let mut out : Array Core.Rfc822.Attachment := #[]
    for x in xs do
      let filename := Str.trim ((jstr? x "filename").getD "")
      if !validFilename filename then fail 400 "bad_attachment" [("reason", Json.str "filename")]
      let mime := Core.Rfc822.safeMime (((jstr? x "mime").orElse fun _ => jstr? x "type").getD "")
      let some b64 := (jstr? x "data_b64").orElse fun _ => jstr? x "content"
        | fail 400 "bad_attachment" [("reason", Json.str "data_b64")]
      out := out.push { filename, mime, data := Core.Base64.decode b64 }
    return out.toList
  | .ok _ => fail 400 "bad_attachment" [("reason", Json.str "shape")]

def optStr (j : Json) (k : String) : Option String :=
  (jstr? j k).bind fun s => let t := Str.trim s; if t.isEmpty then none else some t

/-- The body of `POST /api/send`. -/
def requestOf (j : Json) : Except Fail Request := do
  let some cid := jstr? j "client_id" | fail 400 "bad_client_id"
  let cid := Str.trim cid
  if !Core.Ulid.isValid cid then fail 400 "bad_client_id"
  let to ← recipientsOf j "to"
  let cc ← recipientsOf j "cc"
  let bcc ← recipientsOf j "bcc"
  let replyTo ← match j.getObjVal? "reply_to" with
    | .error _ | .ok .null => pure none
    | .ok (.str "") => pure none
    | .ok v => do
      let l ← recipientsOf (Json.mkObj [("reply_to", Json.arr #[v])]) "reply_to"
      pure l.head?
  let references : List String := match j.getObjVal? "references" with
    | .ok (.arr xs) => xs.toList.filterMap fun | .str s => some s | _ => none
    | .ok (.str s) => (s.split Str.isSpaceLike).toList.map (·.toString) |>.filter (· ≠ "")
    | _ => []
  let expiresAt : Option (Option Nat) := match j.getObjVal? "expires_at" with
    | .error _ => none
    | .ok .null => some none
    | .ok v => match v.getNat? with | .ok n => some (some n) | .error _ => none
  return {
    clientId := Core.Ulid.normalize cid
    from_ := (jstr? j "from").getD "", fromName := optStr j "from_name"
    to, cc, bcc, replyTo
    subject := (jstr? j "subject").getD "", text := (jstr? j "text").getD ""
    html := optStr j "html"
    inReplyTo := optStr j "in_reply_to", references
    replyOf := optStr j "reply_of", forwardOf := optStr j "forward_of"
    attachments := ← attachmentsOf j
    expiresAt
    draftId := optStr j "draft_id"
    draftRevision := (j.getObjValAs? Nat "draft_revision").toOption }

/-! ## Who may be sent as -/

def vaultModes : List String := ["vault", "vault+forward"]

/-- The From of a bare dev server under the dev bypass (`Account.devAccountJson`). -/
def devFrom : String := "dev@localhost"

def domainOf (addr : String) : String := ((addr.splitOn "@").getLast?).getD ""

def accountRow (st : State) : IO (Option Row) := st.db.queryOne "SELECT * FROM account WHERE id = 1"

/-- The operator's sign-in address (`MAIL_FROM`): never a From for anyone, whatever the routing
or `default_from` says (the Worker sends sign-in mail from it itself, `/internal/email/signin`). -/
def isMailFrom (st : State) (addr : String) : Bool :=
  let m := (Str.trim st.config.mailFrom).toLower
  !m.isEmpty && (Str.trim addr).toLower == m

/-- The port of MailVault's `canSendAs`, with the account's `default_from` allowed (unless
`withDefault` is false) and the dev bypass allowing anything except `MAIL_FROM`: an address row
decides for itself (`can_send = 1` and mail kept in the vault); otherwise the domain must be listed
in some `domains.sending` and its default must keep mail in the vault. `MAIL_FROM` is refused for
everyone. -/
def sendable (st : State) (principal : Principal) (addr : String) (withDefault : Bool := true) : IO Bool := do
  if isMailFrom st addr then return false
  if principal == .dev then return true
  let some a := normEmail? addr | return false
  if withDefault && (((← accountRow st).map (·.text "default_from")).getD "").toLower == a then return true
  if a.startsWith "*@" then return false
  -- the connected Gmail address and its verified aliases, sent through Gmail
  if ← Gmail.SendAs.isIdentity st a then return true
  match ← st.db.queryOne "SELECT mail, can_send FROM addresses WHERE address = ?" #[a] with
  | some r => return r.bool "can_send" && vaultModes.contains (r.text "mail")
  | none =>
    let d := domainOf a
    let listed ← st.db.queryOne
      "SELECT 1 AS x FROM domains, json_each(domains.sending) WHERE json_each.value = ? LIMIT 1" #[d]
    if listed.isNone then return false
    match ← st.db.queryOne "SELECT default_mail FROM domains WHERE name = ?" #[d] with
    | some r => return vaultModes.contains (r.text "default_mail")
    | none => return true

/-- The From choices for compose: `default_from`, then every sendable address row. -/
def fromChoices (st : State) : IO (List String) := do
  let df := (((← accountRow st).map (·.text "default_from")).getD "").toLower
  let rows ← st.db.query "SELECT address FROM addresses WHERE can_send = 1 AND mail IN ('vault','vault+forward') AND address NOT LIKE '*@%' ORDER BY address"
  let gmail ← Gmail.SendAs.identities st
  return ((if df.isEmpty then [] else [df]) ++ rows.toList.map (·.text "address") ++ gmail).eraseDups.filter (!isMailFrom st ·)

/-- Every address that is the account's own (for reply-all). -/
def ownAddresses (st : State) : IO (List String) := do
  let acct ← accountRow st
  let rows ← st.db.query "SELECT address FROM addresses"
  return ([(acct.map (·.text "email")).getD "", (acct.map (·.text "default_from")).getD ""] ++
    rows.toList.map (·.text "address")).map (·.toLower) |>.filter (· ≠ "")

/-! ## Caps -/

def checkCaps (r : Request) : Except Fail Unit := do
  let n := r.to.length + r.cc.length + r.bcc.length
  if n == 0 then fail 400 "no_recipients"
  if n > maxRecipients then fail 400 "too_many_recipients" [("max", Json.num maxRecipients)]
  if r.attachments.length > maxAttachments then fail 400 "too_many_attachments" [("max", Json.num maxAttachments)]
  if (r.attachments.map (·.data.size)).sum > maxAttachmentBytes then
    fail 413 "attachments_too_large" [("max", Json.num maxAttachmentBytes)]
  if r.text.utf8ByteSize > maxTextBytes then fail 413 "text_too_large" [("max", Json.num maxTextBytes)]
  if (r.html.map (·.utf8ByteSize)).getD 0 > maxHtmlBytes then fail 413 "html_too_large" [("max", Json.num maxHtmlBytes)]
  if r.subject.length > subjectMax then fail 400 "subject_too_long"

/-- The newest 8 references whose joined length stays under 1900 bytes. -/
def trimReferences (refs : List String) : List String := Id.run do
  let mut out := refs.drop (refs.length - 8)
  while out.length > 1 && (" ".intercalate out).utf8ByteSize > 1900 do out := out.drop 1
  return out

/-! ## Drafts locked for a send (the table is `Server.Drafts`') -/

/-- Lock the draft for the send: its revision must be `expected` and its `send_id` the request's
`client_id`. A draft already `sending` or `sent` under the same revision is a retry. -/
def lockDraft (st : State) (id : String) (expected : Option Nat) (clientId : String) : IO (Except Fail Unit) := do
  let some row ← st.db.queryOne "SELECT * FROM drafts WHERE id = ? AND state != 'deleted'" #[id]
    | return fail 404 "draft_not_found"
  let some exp := expected | return fail 400 "draft_revision_required"
  if row.nat "revision" != exp then return fail 409 "revision" [("revision", Json.num (row.nat "revision"))]
  if row.text "send_id" != clientId then return fail 409 "draft_conflict" [("reason", Json.str "send_id")]
  if row.text "state" == "draft" then
    let now ← Clock.nowMs
    let n ← st.db.execCount "UPDATE drafts SET state = 'sending', updated_at = ? WHERE id = ? AND revision = ? AND state = 'draft'" #[now, id, exp]
    if n == 0 then return fail 409 "draft_conflict"
  return .ok ()

/-- After the send: `sent` keeps the row as a record without its content; a failure hands the
draft back for editing. -/
def settleDraft (st : State) (id : String) (ok : Bool) : IO Unit := do
  let now ← Clock.nowMs
  if ok then
    st.db.exec "UPDATE drafts SET state = 'sent', attachments = '[]', updated_at = ? WHERE id = ? AND state != 'deleted'" #[now, id]
  else
    st.db.exec "UPDATE drafts SET state = 'draft', updated_at = ? WHERE id = ? AND state = 'sending'" #[now, id]

/-! ## The send -/

private def outcomeOf (r : Row) (idem : Bool) : Outcome :=
  { id := r.text "id", threadId := r.text? "thread_id", sendStatus := (r.text? "send_status").getD "sent"
    messageId := r.text? "message_id", idempotent := idem, sendError := r.text? "send_error" }

/-- Parent headers for a reply: `In-Reply-To` and `References`. -/
def replyHeaders (parent : Row) (irtGiven : Option String) (refs : List String) : Option String × List String :=
  let pmid := (parent.text? "message_id").bind messageId?
  let parentRefs : List String := match Json.parse (parent.text "references_json") with
    | .ok (.arr xs) => xs.toList.filterMap fun | .str s => messageId? s | _ => none
    | _ => []
  let irt := irtGiven.bind messageId? <|> pmid
  let base := if refs.isEmpty then parentRefs ++ pmid.toList else refs
  let all := (base ++ irt.toList).eraseDups
  (irt, trimReferences all)

/-- For an API token, 403 `privileged` when `reply_of` or `forward_of` names withheld mail
(`AI.withheldSql`); a session may. Shared by sends and draft saves (`Drafts.put`). -/
def refuseWithheldRefs (st : State) (principal : Principal) (replyOf forwardOf : Option String) : IO (Except Fail Unit) := do
  let isTok := match principal with | .token _ _ => true | _ => false
  if !isTok then return .ok ()
  for id in replyOf.toList ++ forwardOf.toList do
    if ← AI.isWithheld st id then return fail 403 "privileged" [("id", Json.str id)]
  return .ok ()

private def sendCore (st : State) (principal : Principal) (r0 : Request) : IO (Except Fail Outcome) := do
  let now ← Clock.nowMs
  let acct ← accountRow st
  -- from
  let fromRaw := Str.trim r0.from_
  let fromRaw := if fromRaw.isEmpty then ((acct.map (·.text "default_from")).getD "") else fromRaw
  -- a bare dev server (the dev bypass, no account) sends as dev@localhost, which `normEmail?` refuses
  let fromRaw := if fromRaw.isEmpty && principal == .dev then devFrom else fromRaw
  let fromAddr? := if principal == .dev && fromRaw.toLower == devFrom then some devFrom else normEmail? fromRaw
  let some fromAddr := fromAddr? | return fail 400 "bad_from"
  if !(← sendable st principal fromAddr) then return fail 403 "from_not_allowed" [("from", Json.str fromAddr)]
  -- references and forwarded attachments
  let mut r := { r0 with from_ := fromAddr, references := r0.references.filterMap messageId? }
  -- a token never builds on privileged mail (`Api` withholds it from tokens too)
  if let .error f ← refuseWithheldRefs st principal r.replyOf r.forwardOf then return .error f
  if let some pid := r.replyOf then
    let some parent ← Api.messageRow st pid | return fail 400 "reply_of_not_found"
    let (irt, refs) := replyHeaders parent r.inReplyTo r.references
    r := { r with inReplyTo := irt, references := refs }
  else
    let irt := r.inReplyTo.bind messageId?
    r := { r with inReplyTo := irt, references := trimReferences ((r.references ++ irt.toList).eraseDups) }
  if let some fid := r.forwardOf then
    let some (_, raw) ← Api.loadRaw st fid | return fail 400 "forward_of_not_found"
    let carried := (Core.Mime.parse raw).attachments.toList.filter (!·.related) |>.map fun a =>
      ({ filename := (a.filename.getD "attachment"), mime := Core.Rfc822.safeMime a.mimeType, data := a.content } : Core.Rfc822.Attachment)
    r := { r with attachments := carried ++ r.attachments }
  if let .error f := checkCaps r then return .error f
  if let some (some t) := r.expiresAt then
    if t ≤ now then return fail 400 "bad_expires_at"
  -- the draft
  if let some did := r.draftId then
    if let .error f ← lockDraft st did r.draftRevision r.clientId then return .error f
  -- idempotency
  if let some ex ← st.db.queryOne "SELECT * FROM messages WHERE client_id = ?" #[r.clientId] then
    if ex.text "send_status" != "failed" then
      if let some did := r.draftId then settleDraft st did (ex.text "send_status" == "sent")
      return .ok (outcomeOf ex true)
    -- a failed attempt gives its client id to this one
    st.db.exec "UPDATE messages SET client_id = NULL, deleted = 1, deleted_at = ?1, updated_at = ?1 WHERE id = ?2" #[now, ex.text "id"]
  -- the day's cap
  let dayStart := now / dayMs * dayMs
  let sentToday := ((← st.db.queryOne
    "SELECT count(*) AS n FROM messages WHERE source = 'sent' AND received_at >= ? AND NOT (deleted = 1 AND send_status = 'failed')"
    #[dayStart]).map (·.nat "n")).getD 0
  if sentToday ≥ dailyCap then
    if let some did := r.draftId then settleDraft st did false
    return fail 429 "send_cap" [("cap", Json.num dailyCap)]
  -- build
  let id := Core.Ulid.encode now (← Crypto.randomBytes 10)
  let messageId := s!"<{id.toLower}@{domainOf fromAddr}>"
  let fromName := r.fromName.map cleanName <|> (acct.bind fun a => let n := cleanName (a.text "display_name"); if n.isEmpty then none else some n)
  let msg : Core.Rfc822.Message := {
    from_ := { name := fromName.getD "", address := fromAddr }
    to := r.to, cc := r.cc, bcc := r.bcc, replyTo := r.replyTo
    subject := r.subject, text := r.text, html := r.html
    inReplyTo := r.inReplyTo, references := if r.references.isEmpty then none else some (" ".intercalate r.references)
    attachments := r.attachments, date := now, messageId }
  let seed := Crypto.toHex (← Crypto.randomBytes 12)
  let archive := Mail.Build.message msg seed
  let wire := Mail.Build.message { msg with includeBcc := false } seed
  if archive.size > maxMimeBytes then
    if let some did := r.draftId then settleDraft st did false
    return fail 413 "message_too_large" [("max", Json.num maxMimeBytes)]
  -- the Sent copy first
  let key := Spool.rawKey st.config.accountId id
  st.store.put key archive [("source", "sent"), ("received_at", toString now), ("size", toString archive.size)]
  let (expiresAt, keepForever) := match r.expiresAt with
    | none => (none, false)
    | some none => (none, true)
    | some (some t) => (some t, false)
  let res ← Ingest.ingest st { raw := archive, key := some key, rcpt := some fromAddr, source := "sent", folder := "sent",
                               receivedAt := now, forcedId := some id, expiresAt, keepForever }
  if let .duplicate dup := res then
    if dup != id then st.store.delete key
    if let some did := r.draftId then settleDraft st did false
    return fail 409 "duplicate_message_id"
  st.db.exec "UPDATE messages SET send_status = 'sending', client_id = ?, updated_at = ? WHERE id = ?" #[r.clientId, now, id]
  -- the wire: Gmail for its own addresses (the whole message: Gmail reads and strips Bcc), else
  -- the Worker's Email Sending
  let rcpts := (r.to ++ r.cc ++ r.bcc).map (·.address) |>.eraseDups
  let viaGmail ← Gmail.SendAs.isIdentity st fromAddr
  let (ok, gmId, err) ← if viaGmail then do
      match ← Gmail.SendAs.send st archive r.replyOf with
      | .ok gid => pure (true, some gid, "")
      | .error code => pure (false, none, s!"gmail_{code}")
    else do
      let reply ← st.worker.call "POST" "/internal/email/send" <| Json.mkObj [
        ("from", Json.str fromAddr), ("to", Json.arr (rcpts.toArray.map Json.str)),
        ("raw_b64", Json.str (Core.Base64.encode wire))]
      let ok := match reply with
        | some j => (j.getObjValAs? Bool "ok").toOption != some false
        | none => false
      pure (ok, none, "worker_failed")
  let fin ← Clock.nowMs
  if ok then
    st.db.exec "UPDATE messages SET send_status = 'sent', send_error = NULL, updated_at = ? WHERE id = ?" #[fin, id]
    -- Gmail's copy in Sent is then this row's twin; another row may hold the id already (unique)
    if let some g := gmId then
      st.db.exec "UPDATE OR IGNORE messages SET gm_msgid = ? WHERE id = ?" #[g, id]
  else
    st.db.exec "UPDATE messages SET send_status = 'failed', send_error = ?, updated_at = ? WHERE id = ?" #[err, fin, id]
  if let some did := r.draftId then settleDraft st did ok
  Log.info "send.done" [("ok", Json.bool ok), ("via", Json.str (if viaGmail then "gmail" else "worker")),
    ("recipients", Json.num rcpts.length), ("reply", Json.bool r.inReplyTo.isSome),
    ("attachments", Json.num r.attachments.length), ("bytes", Json.num archive.size)]
  let some row ← Api.messageRow st id | return fail 500 "store_failed"
  return .ok (outcomeOf row false)

/-- Send `r` as `principal`. A thrown error hands a locked draft back before it propagates. -/
def send (st : State) (principal : Principal) (r : Request) : IO (Except Fail Outcome) := do
  try sendCore st principal r
  catch e =>
    if let some did := r.draftId then settleDraft st did false
    throw e

/-- `POST /api/send`. -/
def handler (c : Ctx) (req : Req) : IO Resp := do
  let j := req.json
  if j.isNull then return Resp.error 400 "bad json"
  match requestOf j with
  | .error f => return f.resp
  | .ok r =>
    match ← send c.st c.principal r with
    | .error f => return f.resp
    | .ok o => return Resp.ok o.json

def routes : Array Route := #[
  route "POST" "/api/send" (.token "send") handler
]

end Server.Send
