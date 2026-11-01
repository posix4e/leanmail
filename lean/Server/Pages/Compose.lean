import Core.Mime.Header
import Core.Ulid
import Server.Pages.Common
import Server.Pages.Inbox
import Server.Pages.Message
import Server.Api
import Server.Send
import Server.Drafts

/-! # Pages: compose, reply, forward, drafts

`/compose` is one form (`multipart/form-data`, so files can come along) that posts to
`/compose/send` or, with the Save button, `/compose/save`. Both call the same functions as the
API (`Server.Send.send`, `Server.Drafts.save`, `Server.Drafts.sendDraft`):

* `?reply=<id>` (with `&all=1`, reply all), `?forward=<id>` and `?draft=<id>` prefill it;
* the page carries a fresh `draft_id` (a ULID) and `client_id`, so a draft the script autosaves
  and a send the browser repeats are both the same object;
* when a draft row exists, a send first saves the form into it (CAS on the hidden `revision`)
  and then sends the draft; otherwise it sends with the page's `client_id`.

The multipart body is parsed here (`parseMultipart`): the boundary from `Content-Type`, parts by
`Content-Disposition: form-data; name=…; filename=…`. `/drafts` lists drafts with open and
delete. -/

namespace Server.Pages

open Lean Core.Html Server.Db Server.Router Core.Text

/-! ## multipart/form-data -/

structure FormPart where
  name : String
  filename : Option String := none
  contentType : Option String := none
  data : ByteArray := .empty
  deriving Inhabited

/-- The first index ≥ `from_` where `needle` occurs in `hay`. -/
def bytesIndexOf (hay needle : ByteArray) (from_ : Nat := 0) : Option Nat := Id.run do
  if needle.size == 0 then return some from_
  if hay.size < needle.size then return none
  let first := needle[0]!
  let mut i := from_
  while i + needle.size ≤ hay.size do
    if hay[i]! == first then
      let mut ok := true
      let mut k := 1
      while ok && k < needle.size do
        if hay[i + k]! != needle[k]! then ok := false
        k := k + 1
      if ok then return some i
    i := i + 1
  return none

/-- Bytes as text: UTF-8, else Latin-1. -/
def bytesText (b : ByteArray) : String :=
  match String.fromUTF8? b with
  | some s => s
  | none => String.ofList (b.toList.map fun x => Char.ofNat x.toNat)

/-- The `boundary` of a `multipart/form-data` Content-Type. -/
def formBoundary? (contentType : String) : Option String :=
  let s := Core.Mime.Header.parseStructured contentType
  if s.value != "multipart/form-data" then none else
  match s.param? "boundary" with
  | some b => if b.isEmpty || b.length > 200 then none else some b
  | none => none

private def partOf (raw : ByteArray) : Option FormPart := do
  -- headers end at the first blank line
  let (hEnd, bStart) ← match bytesIndexOf raw "\r\n\r\n".toUTF8 with
    | some i => some (i, i + 4)
    | none => (bytesIndexOf raw "\n\n".toUTF8).map fun i => (i, i + 2)
  let hdrText := bytesText (raw.extract 0 hEnd)
  let lines := ((hdrText.replace "\r\n" "\n").splitOn "\n").toArray
  let fields := Core.Mime.Header.parseLines lines
  let disp ← (fields.find? (·.key == "content-disposition")).map (·.value)
  let d := Core.Mime.Header.parseStructured disp
  let name ← d.param? "name"
  pure { name, filename := d.param? "filename"
         contentType := (fields.find? (·.key == "content-type")).map (·.value)
         data := raw.extract bStart raw.size }

/-- Parse a `multipart/form-data` body; `none` when the type or the framing is wrong. -/
def parseMultipart (contentType : String) (body : ByteArray) : Option (Array FormPart) := do
  let b ← formBoundary? contentType
  let delim := ("--" ++ b).toUTF8
  let next := ("\r\n--" ++ b).toUTF8
  let start ← bytesIndexOf body delim
  let mut parts : Array FormPart := #[]
  let mut pos := start + delim.size
  let mut fuel := 1000
  while fuel > 0 do
    fuel := fuel - 1
    -- `--` after a delimiter closes the body
    if pos + 1 < body.size && body[pos]! == 45 && body[pos + 1]! == 45 then break
    -- skip the rest of the delimiter line
    let some eol := bytesIndexOf body "\n".toUTF8 pos | break
    let contentStart := eol + 1
    let some e := bytesIndexOf body next contentStart | break
    if let some p := partOf (body.extract contentStart e) then parts := parts.push p
    pos := e + next.size
  return parts

/-- The text value of a field (CRLF made LF); `""` when absent. -/
def partText (ps : Array FormPart) (name : String) : String :=
  match ps.find? (fun p => p.name == name && p.filename.isNone) with
  | some p => (bytesText p.data).replace "\r\n" "\n"
  | none => ""

def partTexts (ps : Array FormPart) (name : String) : List String :=
  (ps.filter (fun p => p.name == name && p.filename.isNone)).toList.map (bytesText ·.data)

/-- The uploaded files (empty file inputs are skipped). -/
def partFiles (ps : Array FormPart) : List Core.Rfc822.Attachment :=
  ps.toList.filterMap fun p =>
    match p.filename with
    | some f =>
      let f := Str.trim f
      -- some browsers send a full path
      let f := ((f.splitOn "\\").getLast?.getD f)
      let f := ((f.splitOn "/").getLast?.getD f)
      if f.isEmpty && p.data.size == 0 then none
      else some { filename := (if f.isEmpty then "attachment" else f)
                  mime := Core.Rfc822.safeMime (((p.contentType.getD "").splitOn ";").headD "" |> Str.trim)
                  data := p.data }
    | none => none

/-- The form fields of a compose post: multipart, or urlencoded when the script posts that. -/
def composeParts (req : Req) : Option (Array FormPart) :=
  let ct := (req.header? "content-type").getD ""
  if ct.toLower.startsWith "application/x-www-form-urlencoded" then
    some ((parseForm req.body).toArray.map fun (k, v) => { name := k, data := v.toUTF8 })
  else parseMultipart ct req.body

/-! ## The form -/

structure ComposeForm where
  from_ : String := ""
  to : String := ""
  cc : String := ""
  bcc : String := ""
  subject : String := ""
  body : String := ""
  draftId : String := ""
  revision : Nat := 0
  clientId : String := ""
  replyOf : String := ""
  forwardOf : String := ""
  inReplyTo : String := ""
  references : String := ""
  /-- Stored on the draft: (filename, size). -/
  stored : List (String × Nat) := []
  /-- Carried from the forwarded message: (filename, size). -/
  carried : List (String × Nat) := []
  deriving Inhabited

private def freshUlid : IO String := do return Core.Ulid.encode (← Clock.nowMs) (← Crypto.randomBytes 10)

/-- `> ` before every line. -/
def quoteLines (s : String) : String :=
  let ls := (Core.Rfc822.normalizeNewlines (Str.trimRight s)).splitOn "\n"
  "\n".intercalate (ls.map fun l => if l.isEmpty then ">" else "> " ++ l)

def mailboxStr (name addr : String) : String :=
  if name.isEmpty then addr else if addr.isEmpty then name else
    let n := if Core.Rfc822.needsQuote name then Core.Rfc822.quoteString name else name
    s!"{n} <{addr}>"

def mailboxesOf (s : String) : List (String × String) :=
  match Json.parse s with
  | .ok (.arr xs) => xs.toList.filterMap fun j =>
    let a := (j.getObjValAs? String "address").toOption.getD ""
    if a.isEmpty then none else some ((j.getObjValAs? String "name").toOption.getD "", a)
  | _ => []

def prefixed (pre subj : String) : String :=
  let t := Str.trim subj
  if t.toLower.startsWith (pre.toLower ++ ":") then t else s!"{pre}: {t}"

def bodyTextOf (st : State) (id : String) : IO String := do
  return ((← st.db.queryOne "SELECT body FROM message_text WHERE message_id = ?" #[id]).map (·.text "body")).getD ""

/-- The From the reply should use: the first address the parent was sent to that we may send as. -/
def replyFrom (st : State) (p : Page) (parent : Row) : IO String := do
  let rcpts := Drafts.strList (parent.text "rcpts") ++ (mailboxesOf (parent.text "to_json") ++ mailboxesOf (parent.text "cc_json")).map (·.2)
  for r in rcpts do
    if (← Send.fromChoices st).contains r.toLower then return r.toLower
  let _ := p
  return ""

def replyForm (st : State) (p : Page) (parent : Row) (all : Bool) : IO ComposeForm := do
  let id := parent.text "id"
  let fromA := parent.text "from_addr"
  let fromN := parent.text "from_name"
  let replyTo := (parent.text? "reply_to").getD ""
  let to := if replyTo.isEmpty then mailboxStr fromN fromA else replyTo
  let own ← Send.ownAddresses st
  let ownFrom ← replyFrom st p parent
  let toAddr := (to.splitOn "<").getLast?.getD to |>.replace ">" "" |> Str.trim |>.toLower
  let ccs := if all then
      (mailboxesOf (parent.text "to_json") ++ mailboxesOf (parent.text "cc_json")).filter fun (_, a) =>
        let a := a.toLower
        !own.contains a && a != toAddr && a != ownFrom && a != fromA.toLower
    else []
  let ts := parent.nat "received_at"
  let text ← bodyTextOf st id
  let who := mailboxStr fromN fromA
  return { to, cc := ", ".intercalate (ccs.eraseDups.map fun (n, a) => mailboxStr n a)
           subject := prefixed "Re" (parent.text "subject"), from_ := ownFrom
           body := s!"\n\nOn {fullDate ts}, {who} wrote:\n{quoteLines text}\n", replyOf := id }

def forwardForm (st : State) (p : Page) (parent : Row) : IO ComposeForm := do
  let id := parent.text "id"
  let ts := parent.nat "received_at"
  let text ← bodyTextOf st id
  let atts ← st.db.query "SELECT filename, size FROM attachments WHERE message_id = ? AND inline = 0 ORDER BY idx" #[id]
  let to := mailboxes (parent.text "to_json")
  let hdr := s!"---------- Forwarded message ----------\nFrom: {mailboxStr (parent.text "from_name") (parent.text "from_addr")}\nDate: {fullDate ts}\nSubject: {parent.text "subject"}\n" ++
    (if to.isEmpty then "" else s!"To: {", ".intercalate to}\n")
  return { subject := prefixed "Fwd" (parent.text "subject"), from_ := ← replyFrom st p parent
           body := s!"\n\n{hdr}\n{quoteLines text}\n", forwardOf := id
           carried := atts.toList.map fun a => (a.text "filename", a.nat "size") }

def draftForm (r : Row) : ComposeForm :=
  { from_ := r.text "from_addr", to := ", ".intercalate (Drafts.strList (r.text "to_json"))
    cc := ", ".intercalate (Drafts.strList (r.text "cc_json")), bcc := ", ".intercalate (Drafts.strList (r.text "bcc_json"))
    subject := r.text "subject", body := r.text "body_text", draftId := r.text "id", revision := r.nat "revision"
    clientId := r.text "send_id", replyOf := (r.text? "reply_of").getD "", forwardOf := (r.text? "forward_of").getD ""
    inReplyTo := (r.text? "in_reply_to").getD "", references := " ".intercalate (Drafts.strList (r.text "references_json"))
    stored := (Drafts.attsOf (r.text "attachments")).map fun a => (a.filename, a.size) }

def hiddenInput (k v : String) : Node := el "input" [("type", "hidden"), ("name", k), ("value", v)]

def textField (label name value : String) (extra : List (String × String) := []) (ty : String := "text") : Node :=
  el "label" [("class", "field")] [elt "span" [] label,
    el "input" ([("type", ty), ("name", name), ("value", value), ("autocomplete", "off")] ++ extra)]

def composeDoc (p : Page) (f : ComposeForm) (choices : List String) (msg : Option (String × String)) (status : Nat := 200) : Resp :=
  let choices := if f.from_.isEmpty || choices.contains f.from_.toLower then choices else f.from_ :: choices
  let selected := if f.from_.isEmpty then choices.headD "" else f.from_.toLower
  let fromField : Node :=
    if choices.isEmpty then textField "From" "from" f.from_ (ty := "email")
    else el "label" [("class", "field")] [elt "span" [] "From",
      el "select" [("name", "from")] (choices.map fun a =>
        elt "option" ([("value", a)] ++ (if a == selected then [("selected", "selected")] else [])) a)]
  let attList (title : String) (xs : List (String × Nat)) (drop : Bool) : Node :=
    if xs.isEmpty then empty else
    el "div" [("class", "field")] [elt "span" [] title,
      el "ul" [("class", "atts")] (xs.zipIdx.map fun ((n, s), i) =>
        el "li" [] ([txt n, elt "span" [("class", "att-meta")] s!" {humanSize s}"] ++
          (if drop then [el "label" [("class", "check")] [
             el "input" [("type", "checkbox"), ("name", "drop_att"), ("value", toString i)], txt "Remove"]] else [])))]
  let title := if !f.replyOf.isEmpty then "Reply" else if !f.forwardOf.isEmpty then "Forward" else "Compose"
  page p title (pageKind := "compose") (status := status) [
    elt "h1" [] title,
    (match msg with | some (k, m) => flash k m | none => empty),
    el "form" [("method", "post"), ("action", "/compose/send"), ("enctype", "multipart/form-data"),
               ("class", "card stack compose"), ("id", "compose")] [
      hiddenInput "draft_id" f.draftId, hiddenInput "revision" (toString f.revision),
      hiddenInput "client_id" f.clientId, hiddenInput "reply_of" f.replyOf, hiddenInput "forward_of" f.forwardOf,
      hiddenInput "in_reply_to" f.inReplyTo, hiddenInput "references" f.references,
      fromField,
      textField "To" "to" f.to [("autofocus", "autofocus")],
      textField "Cc" "cc" f.cc,
      textField "Bcc" "bcc" f.bcc,
      textField "Subject" "subject" f.subject,
      el "label" [("class", "field")] [elt "span" [] "Message",
        elt "textarea" [("name", "body"), ("rows", "16"), ("class", "body-edit")] f.body],
      attList "Attached" f.stored true,
      attList "From the original" f.carried false,
      el "label" [("class", "field")] [elt "span" [] "Attach files (up to 10, 3 MB in all)",
        el "input" [("type", "file"), ("name", "attachment"), ("multiple", "multiple")]],
      el "div" [("class", "row-buttons")] [
        elt "button" [("type", "submit"), ("class", "primary"), ("id", "send-btn")] "Send",
        elt "button" [("type", "submit"), ("formaction", "/compose/save"), ("id", "save-btn")] "Save draft",
        elt "span" [("class", "hint"), ("id", "draft-state")] ""]]]

/-! ## Handlers -/

def composeGet (p : Page) : IO Resp := do
  let st := p.c.st
  let req := p.req
  let mut f : ComposeForm := {}
  if let some id := req.param? "draft" then
    let some r ← Drafts.load st id | return notFoundPage p
    if r.text "state" != "draft" then return redirect "/drafts"
    f := draftForm r
  else if let some id := req.param? "reply" then
    let some parent ← Api.messageRow st id | return notFoundPage p
    f ← replyForm st p parent (req.param? "all" == some "1")
  else if let some id := req.param? "forward" then
    let some parent ← Api.messageRow st id | return notFoundPage p
    f ← forwardForm st p parent
  if f.draftId.isEmpty then f := { f with draftId := ← freshUlid }
  if f.clientId.isEmpty then f := { f with clientId := ← freshUlid }
  let choices ← composeChoices st p
  let msg := if req.param? "saved" == some "1" then some ("ok", "Draft saved.") else none
  return composeDoc p f choices msg
where
  composeChoices (st : State) (p : Page) : IO (List String) := do
    let cs ← Send.fromChoices st
    -- nothing configured yet (a fresh or dev account): offer the account's own address
    if cs.isEmpty && !p.me.email.isEmpty then return [p.me.email.toLower] else return cs

/-- The form as posted, back into the form record (for re-rendering after a refusal). -/
def formOf (ps : Array FormPart) : ComposeForm :=
  { from_ := partText ps "from", to := partText ps "to", cc := partText ps "cc", bcc := partText ps "bcc"
    subject := partText ps "subject", body := partText ps "body", draftId := Str.trim (partText ps "draft_id")
    revision := (Str.trim (partText ps "revision")).toNat?.getD 0, clientId := Str.trim (partText ps "client_id")
    replyOf := Str.trim (partText ps "reply_of"), forwardOf := Str.trim (partText ps "forward_of")
    inReplyTo := Str.trim (partText ps "in_reply_to"), references := Str.trim (partText ps "references") }

def nonEmpty? (s : String) : Option String := if s.isEmpty then none else some s

/-- The draft fields a post gives: the stored attachments it keeps plus the new files. -/
def fieldsOfForm (f : ComposeForm) (ps : Array FormPart) (held : Option Row) : Drafts.Fields :=
  let drops := (partTexts ps "drop_att").filterMap (·.toNat?)
  let kept := match held with
    | some h => (Drafts.attsOf (h.text "attachments")).zipIdx.filter (!drops.contains ·.2) |>.map (·.1)
    | none => []
  { from_ := Str.trim f.from_, to := Send.splitRecipients f.to, cc := Send.splitRecipients f.cc
    bcc := Send.splitRecipients f.bcc, subject := f.subject, text := f.body
    inReplyTo := nonEmpty? f.inReplyTo
    references := (f.references.split Str.isSpaceLike).toList.map (·.toString) |>.filter (· ≠ "")
    replyOf := nonEmpty? f.replyOf, forwardOf := nonEmpty? f.forwardOf
    attachments := some (kept ++ (partFiles ps).map Drafts.StoredAtt.ofAttachment) }

def refusal (p : Page) (f : ComposeForm) (e : Send.Fail) : IO Resp := do
  let msg := match e.error with
    | "from_not_allowed" => "You cannot send from that address."
    | "too_many_recipients" => s!"At most {Send.maxRecipients} recipients."
    | "no_recipients" => "Add at least one recipient."
    | "too_many_attachments" => s!"At most {Send.maxAttachments} attachments."
    | "attachments_too_large" => "Attachments can total up to 3 MB."
    | "bad_address" => "One of the addresses is not valid."
    | "send_cap" => "The daily sending limit is reached."
    | "revision" => "This draft was changed elsewhere. Reload it to see the latest version."
    | other => s!"Could not send: {other}."
  let held ← if f.draftId.isEmpty then pure none else Drafts.load p.c.st f.draftId
  let f := match held with
    | some h => { f with revision := h.nat "revision", stored := (Drafts.attsOf (h.text "attachments")).map fun (a : Drafts.StoredAtt) => (a.filename, a.size) }
    | none => f
  let cid ← freshUlid
  let f := if f.clientId.isEmpty then { f with clientId := cid } else f
  let cs ← Send.fromChoices p.c.st
  let cs := if cs.isEmpty && !p.me.email.isEmpty then [p.me.email.toLower] else cs
  return composeDoc p f cs (some ("err", msg)) (if e.status < 400 then 400 else e.status)

def sentLocation (o : Send.Outcome) : String := s!"/m/{o.id}?sent={o.sendStatus}"

/-- `POST /compose/send`. -/
def composeSend (p : Page) : IO Resp := do
  let st := p.c.st
  let some ps := composeParts p.req | return errorPage 400 (some p.me)
  let f := formOf ps
  let held ← if Core.Ulid.isValid f.draftId then Drafts.load st f.draftId else pure none
  match held with
  | some h =>
    let mid ← freshUlid
    match ← Drafts.save st f.draftId (some f.revision) mid (fieldsOfForm f ps (some h)) with
    | .error e => refusal p f e
    | .ok (_, r) =>
      match ← Drafts.sendDraft st p.c.principal f.draftId (some (r.nat "revision")) with
      | .error e => refusal p f e
      | .ok o => return redirect (sentLocation o) 303
  | none =>
    let fs := fieldsOfForm f ps none
    let req : Except Send.Fail Send.Request := do
      return {
        clientId := if Core.Ulid.isValid f.clientId then Core.Ulid.normalize f.clientId else ""
        from_ := fs.from_
        to := ← Send.parseRecipientStrings "to" fs.to, cc := ← Send.parseRecipientStrings "cc" fs.cc
        bcc := ← Send.parseRecipientStrings "bcc" fs.bcc
        subject := fs.subject, text := fs.text, inReplyTo := fs.inReplyTo, references := fs.references
        replyOf := fs.replyOf, forwardOf := fs.forwardOf, attachments := partFiles ps }
    match req with
    | .error e => refusal p f e
    | .ok r =>
      let cid ← freshUlid
      let r := if r.clientId.isEmpty then { r with clientId := cid } else r
      match ← Send.send st p.c.principal r with
      | .error e => refusal p f e
      | .ok o => return redirect (sentLocation o) 303

/-- `POST /compose/save`. -/
def composeSave (p : Page) : IO Resp := do
  let st := p.c.st
  let some ps := composeParts p.req | return errorPage 400 (some p.me)
  let f := formOf ps
  let did ← freshUlid
  let f := if Core.Ulid.isValid f.draftId then f else { f with draftId := did }
  let held ← Drafts.load st f.draftId
  let expected := if held.isSome then some f.revision else none
  match ← Drafts.save st f.draftId expected (← freshUlid) (fieldsOfForm f ps held) with
  | .error e => refusal p f e
  | .ok (_, r) => return redirect s!"/compose?draft={r.text "id"}&saved=1" 303

/-- `/drafts`. -/
def draftsPage (p : Page) : IO Resp := do
  let now ← Clock.nowMs
  let rows ← Drafts.listAll p.c.st
  let item (r : Row) : Node :=
    let id := r.text "id"
    let subj := r.text "subject"
    let to := ", ".intercalate (Drafts.strList (r.text "to_json"))
    let ts := r.nat "updated_at"
    el "li" [("class", "item")] [
      el "div" [] [
        elt "a" [("href", s!"/compose?draft={id}"), ("class", "tname")] (if subj.isEmpty then "(no subject)" else subj),
        elt "span" [("class", "meta")] (if to.isEmpty then "No recipients yet" else s!"To {to}"),
        el "span" [("class", "meta")] [txt "Edited ", timeEl ts (shortTime ts now),
          txt (if r.text "state" == "sending" then " · sending" else "")]],
      el "div" [("class", "row-buttons")] [
        elt "a" [("href", s!"/compose?draft={id}")] "Open",
        postButton s!"/drafts/{id}/delete" "Delete" "danger"]]
  return page p "Drafts" (pageKind := "drafts") [
    elt "h1" [] "Drafts",
    el "p" [] [elt "a" [("href", "/compose")] "New message"],
    (match p.req.param? "deleted" with | some _ => flash "ok" "Draft deleted." | none => empty),
    if rows.isEmpty then emptyNote "No drafts." else el "ul" [("class", "items")] (rows.toList.map item)]

def draftDeletePost (p : Page) : IO Resp := do
  match ← Drafts.remove p.c.st (p.c.param "id") none with
  | .error e => if e.status == 404 then return notFoundPage p else return errorPage 400 (some p.me)
  | .ok () => return redirect "/drafts?deleted=1" 303

def composeRoutes : Array Route := #[
  route "GET" "/compose" .«public» (guarded false composeGet),
  route "POST" "/compose/send" .«public» (guarded true composeSend),
  route "POST" "/compose/save" .«public» (guarded true composeSave),
  route "GET" "/drafts" .«public» (guarded false draftsPage),
  route "POST" "/drafts/:id/delete" .«public» (guarded true draftDeletePost)
]

end Server.Pages
