import Core.Text.Str
import Core.Text.Charset
import Core.Text.Html
import Core.Mime.Header
import Core.Mime.Rfc2047
import Core.Mime.Parse
import Core.Mime.Address
import Core.Mime.Date

/-! # A parsed message

`Core.Mime.parse` turns raw RFC 5322 bytes into the text, HTML and attachments postal-mime 3.0
would find in them, rule for rule: which parts are text, how sibling text parts are merged and
converted, when a `message/rfc822` part is parsed inline, and what an attachment carries, plus the
typed header fields of postal-mime's result (`from`, `to`, `subject`, `date`, ...), parsed by
`Core.Mime.Address` and `Core.Mime.Date`. The raw header values stay reachable too. -/

namespace Core.Mime

open Core.Text Str

structure Attachment where
  filename : Option String := none
  mimeType : String
  disposition : Option String := none
  /-- Under `multipart/related` with a `Content-ID`: a resource the HTML refers to. -/
  related : Bool := false
  /-- The raw `Content-ID` value, angle brackets included. -/
  contentId : Option String := none
  description : Option String := none
  /-- `METHOD` of a calendar part, upper-cased. -/
  method : Option String := none
  content : ByteArray := .empty
  /-- An inline `message/rfc822` that was not parsed because it was nested too deep. -/
  rfc822DepthExceeded : Bool := false
  deriving Inhabited

structure Message where
  headers : Array Header.Field := #[]
  /-- Absent when no part gave plain text. -/
  text : Option String := none
  html : Option String := none
  attachments : Array Attachment := #[]
  /-- The first address of the first `From` header (postal-mime's `message.from`). -/
  from? : Option Addr := none
  sender? : Option Addr := none
  /-- The address of the first `Delivered-To` / `Return-Path` header's first entry, when not empty. -/
  deliveredTo? : Option String := none
  returnPath? : Option String := none
  /-- Every `To` (`Cc`, ...) header parsed, concatenated in order. -/
  to : List Addr := []
  cc : List Addr := []
  bcc : List Addr := []
  replyTo : List Addr := []
  /-- The first header of that name with encoded words decoded, when its raw value is not empty. -/
  subject? : Option String := none
  messageId? : Option String := none
  inReplyTo? : Option String := none
  references? : Option String := none
  /-- The raw value of the first `Date` header. -/
  dateRaw? : Option String := none
  /-- `dateRaw?` as epoch milliseconds, when it parses. -/
  dateMs? : Option Nat := none
  /-- postal-mime's `message.date`: ISO 8601 when the date parses, else the raw value. -/
  dateIso? : Option String := none
  deriving Inhabited

/-- The value of the first header named `key` (lower-case). -/
def Message.rawHeader? (m : Message) (key : String) : Option String :=
  (m.headers.find? (·.key == key)).map (·.value)

/-- Every value of the headers named `key`, in order. -/
def Message.rawHeaders (m : Message) (key : String) : Array String :=
  (m.headers.filter (·.key == key)).map (·.value)

/-- postal-mime's treatment of `subject`, `message-id`, `in-reply-to` and `references`: the first
header of that name, when its value is not empty, with encoded words decoded. -/
private def Message.decoded? (m : Message) (key : String) : Option String :=
  match m.rawHeader? key with
  | some v => if v.isEmpty then none else some (Rfc2047.decodeWords v)
  | none => none

/-- The first address of the first header named `key`, when its value is not empty. -/
private def Message.firstAddr? (m : Message) (key : String) : Option Addr :=
  match m.rawHeader? key with
  | some v => if v.isEmpty then none else (Address.parse v).head?
  | none => none

/-- `delivered-to` / `return-path`: the first entry's address, when it has a non-empty one. -/
private def Message.firstAddress? (m : Message) (key : String) : Option String :=
  match m.firstAddr? key with
  | some (.mailbox _ a) => if a.isEmpty then none else some a
  | _ => none

/-- Every non-empty header named `key`, parsed and concatenated. -/
private def Message.allAddrs (m : Message) (key : String) : List Addr :=
  (m.rawHeaders key).toList.filter (!·.isEmpty) |>.flatMap Address.parse

/-- Fill the typed fields from the headers, as postal-mime's `parse` does after the tree walk. -/
def Message.withTypedFields (m : Message) : Message :=
  let dateRaw := m.rawHeader? "date"
  let dateMs := dateRaw.bind Date.parse?
  { m with
    from? := m.firstAddr? "from", sender? := m.firstAddr? "sender"
    deliveredTo? := m.firstAddress? "delivered-to", returnPath? := m.firstAddress? "return-path"
    to := m.allAddrs "to", cc := m.allAddrs "cc", bcc := m.allAddrs "bcc", replyTo := m.allAddrs "reply-to"
    subject? := m.decoded? "subject", messageId? := m.decoded? "message-id"
    inReplyTo? := m.decoded? "in-reply-to", references? := m.decoded? "references"
    dateRaw? := dateRaw, dateMs? := dateMs
    dateIso? := (dateMs.map Date.formatIso) <|> dateRaw }

def Message.subject (m : Message) : Option String := m.subject?
def Message.messageId (m : Message) : Option String := m.messageId?
def Message.inReplyTo (m : Message) : Option String := m.inReplyTo?
def Message.references (m : Message) : Option String := m.references?
/-- The raw `Date` value; `dateIso?` is postal-mime's `message.date`. -/
def Message.date (m : Message) : Option String := m.dateRaw?

/-! ## The header block of an inlined message

postal-mime's `formatTextHeader` / `formatHtmlHeader` (the patched `text-format.js`): a row is shown
when JavaScript would find the field truthy, so `From` is shown even when it formats to nothing. -/

/-- postal-mime's `formatDate` of `message.date`: the `en-US`/UTC `Intl.DateTimeFormat` rendering
when the date parses, else the raw value. -/
def Message.dateShown? (m : Message) : Option String :=
  match m.dateIso? with
  | some s => if s.isEmpty then none else some ((m.dateMs?.map Date.formatIntl).getD s)
  | none => none

private def nonEmpty? (o : Option String) : Option String :=
  o.bind fun s => if s.isEmpty then none else some s

private def nonEmptyList? (as : List Addr) : Option (List Addr) := if as.isEmpty then none else some as

/-- The text rows: `From`, `Subject`, `Date`, `To`, `Cc`, `Bcc`, each when present. -/
def headerRows (m : Message) : List (String × String) :=
  let addrs (k : String) (as : Option (List Addr)) := as.map fun as => (k, Address.formatList as)
  [m.from?.map fun f => ("From", Address.formatList [f]),
   (nonEmpty? m.subject?).map ("Subject", ·),
   m.dateShown?.map ("Date", ·),
   addrs "To" (nonEmptyList? m.to), addrs "Cc" (nonEmptyList? m.cc), addrs "Bcc" (nonEmptyList? m.bcc)].filterMap id

/-- postal-mime's `foldLines(str, lineLength, afterSpace = true)`. -/
def foldLines (s : String) (lineLength : Nat := 76) : String := Id.run do
  let a := s.toList.toArray
  let len := a.size
  let mut pos := 0
  let mut result := ""
  while pos < len do
    let mut line := a.extract pos (pos + lineLength)
    if line.size < lineLength then
      result := result ++ String.ofList line.toList
      break
    -- a line break inside: take through it
    match line.findIdx? (fun c => c == '\n' || c == '\r') with
    | some k =>
      let k' := if line[k]! == '\r' && k + 1 < line.size && line[k + 1]! == '\n' then k + 2 else k + 1
      result := result ++ String.ofList (line.extract 0 k').toList
      pos := pos + k'
      continue
    | none => pure ()
    -- break before the last word, keeping the space on this line
    let lastSpace := (List.range line.size).reverse.find? fun i => isJsWhitespace line[i]!
    match lastSpace with
    | some sp => line := line.extract 0 (sp + 1)
    | none =>
      -- no space: run on to the end of the word and the space after it
      let rest := a.extract (pos + line.size) len
      let word := rest.toList.takeWhile (!isJsWhitespace ·)
      if !word.isEmpty then
        let sp := (rest.toList.drop word.length).takeWhile isJsWhitespace
        line := line ++ (word ++ sp).toArray
    result := result ++ String.ofList line.toList
    pos := pos + line.size
    if pos < len then result := result ++ "\r\n"
  return result

def formatTextHeader (m : Message) : String := Id.run do
  let rows := headerRows m
  let maxKey := rows.foldl (fun acc (k, _) => max acc k.length) 0
  let mut lines : Array String := #[]
  for (k, v) in rows do
    let sep := maxKey - k.length
    let pre := k ++ ": " ++ repeatChar ' ' sep
    let emptyPre := repeatChar ' ' (k.length + 1) ++ " " ++ repeatChar ' ' sep
    let folded := (Str.lines (foldLines v 80)).map trimJs
    let mut first := true
    for l in folded do
      lines := lines.push ((if first then pre else emptyPre) ++ l)
      first := false
  let maxLine := lines.foldl (fun acc l => max acc l.length) 0
  let marker := repeatChar '-' maxLine
  return "\n" ++ marker ++ "\n" ++ "\n".intercalate lines.toList ++ "\n" ++ marker ++ "\n"

def formatHtmlHeader (m : Message) : String :=
  let row (k v : String) (cls : String := "") (attrs : String := "") :=
    s!"<div class=\"postal-email-header-key\">{k}</div><div class=\"postal-email-header-value{cls}\"{attrs}>{v}</div>"
  let addrs (k : String) (as : List Addr) :=
    if as.isEmpty then none else some (row k (Address.formatHtmlList as))
  let date := m.dateShown?.map fun shown =>
    row "Date" (Html.escape shown) " postal-email-header-date" s!" data-date=\"{Html.escape (m.dateIso?.getD "")}\""
  let rows := [m.from?.map fun f => row "From" (Address.formatHtmlList [f]),
    (nonEmpty? m.subject?).map fun s => row "Subject" (Html.escape s) " postal-email-header-subject",
    date, addrs "To" m.to, addrs "Cc" m.cc, addrs "Bcc" m.bcc].filterMap id
  let inner := if rows.isEmpty then "" else
    "<div class=\"postal-email-header-row\">" ++ "</div>\n<div class=\"postal-email-header-row\">".intercalate rows ++ "</div>"
  "<div class=\"postal-email-header\">" ++ inner ++ "</div>"

/-! ## From the tree to the message -/

/-- An entry of the text map: a part's text, or an inlined message (shown by its header block). -/
inductive TextItem where
  | text (s : String)
  | sub (plainHeader htmlHeader : String)
  deriving Inhabited

structure TextEntry where
  plain : Array TextItem := #[]
  html : Array TextItem := #[]
  deriving Inhabited

def TextEntry.get (e : TextEntry) (ty : String) : Array TextItem :=
  if ty == "plain" then e.plain else if ty == "html" then e.html else #[]

def TextEntry.push (e : TextEntry) (ty : String) (x : TextItem) : TextEntry :=
  if ty == "plain" then { e with plain := e.plain.push x }
  else if ty == "html" then { e with html := e.html.push x } else e

/-- Insertion-ordered, keyed by node: `[i]` for this message's node `i`, `i :: k` for key `k` of the
message inlined at node `i`. -/
abbrev TextMap := Array (List Nat × TextEntry)

private def TextMap.modifyKey (m : TextMap) (k : List Nat) (f : TextEntry → TextEntry) : TextMap :=
  match m.findIdx? (·.1 == k) with
  | some i => m.modify i fun (k, e) => (k, f e)
  | none => m.push (k, f {})

private def TextMap.set (m : TextMap) (k : List Nat) (e : TextEntry) : TextMap :=
  m.modifyKey k fun _ => e

def maxRfc822Depth : Nat := 10

private def isInlineText (n : Parse.Node) : Bool :=
  n.disposition.value != "attachment" &&
    (n.contentType.value == "text/html" || n.contentType.value == "text/plain")

private def isInlineRfc822 (n : Parse.Node) : Bool :=
  n.contentType.value == "message/rfc822" &&
    (if n.disposition.value.isEmpty then "inline" else n.disposition.value) == "inline"

/-- A delivery or feedback report keeps its `message/rfc822` parts as attachments. -/
private def forceRfc822Attachments (t : Parse.Tree) : Bool :=
  t.nodes.any fun n => n.multipart.isNone &&
    (n.contentType.value == "message/delivery-status" || n.contentType.value == "message/feedback-report")

/-- Trailing LF run → exactly one LF. -/
private def oneTrailingNewline (s : String) : String :=
  String.ofList (s.toList.reverse.dropWhile (· == '\n') |>.reverse) ++ "\n"

private def attachmentOf (n : Parse.Node) (related exceeded : Bool) : Attachment :=
  let nonEmpty (o : Option String) := o.bind fun s => if s.isEmpty then none else some s
  let filename := nonEmpty (n.disposition.param? "filename") <|> nonEmpty (n.contentType.param? "name")
  let isCal := n.contentType.value == "text/calendar" || n.contentType.value == "application/ics"
  let content :=
    if isCal then
      oneTrailingNewline ((Parse.textContent n).replace "\r\n" "\n") |>.toUTF8
    else n.content.getD .empty
  { filename := filename.map Rfc2047.decodeWords
    mimeType := n.contentType.value
    disposition := if n.disposition.value.isEmpty then none else some n.disposition.value
    related := related && (nonEmpty n.contentId).isSome && !exceeded
    contentId := nonEmpty n.contentId
    description := (nonEmpty n.contentDescription).map Rfc2047.decodeWords
    method := if isCal then (nonEmpty (n.contentType.param? "method")).map fun m => trimJs m.toUpper else none
    content
    rfc822DepthExceeded := exceeded }

structure Walk where
  textMap : TextMap := #[]
  textTypes : Array String := #[]
  attachments : Array Attachment := #[]

private def Walk.addType (w : Walk) (ty : String) : Walk :=
  if w.textTypes.contains ty then w else { w with textTypes := w.textTypes.push ty }

private def join (xs : Array String) : String := "\n".intercalate xs.toList

/-- Render the text map: every text type seen, from every entry, converting from the other type
where an entry lacks one. -/
private def render (w : Walk) : List (String × String) :=
  w.textTypes.toList.map fun ty =>
    let alt := if ty == "html" then "plain" else "html"
    let out := w.textMap.foldl (init := #[]) fun out (_, e) =>
      let own := e.get ty
      if !own.isEmpty then
        own.foldl (init := out) fun out x =>
          match x with
          | .text s => out.push s
          | .sub p h => out.push (if ty == "html" then h else p)
      else
        (e.get alt).foldl (init := out) fun out x =>
          match x with
          | .text s => out.push (if ty == "html" then Html.textToHtml s else Html.toText s)
          | .sub p h => out.push (if ty == "html" then h else p)
    (ty, join out)

mutual
  /-- The message and its text map (which a parent merges into its own). -/
  partial def parseAt (raw : ByteArray) (depth : Nat) : Message × TextMap :=
    match Parse.parseTree raw with
    | none => ({}, #[])
    | some t =>
      let force := forceRfc822Attachments t
      let w := walk t depth force 0 none false {}
      let texts := render w
      let pick (ty : String) := (texts.lookup ty).bind fun s => if s.isEmpty then none else some s
      (Message.withTypedFields
        { headers := (t.node 0).headers, text := pick "plain", html := pick "html", attachments := w.attachments },
       w.textMap)

  partial def walk (t : Parse.Tree) (depth : Nat) (force : Bool) (i : Nat)
      (alternative : Option Nat) (related : Bool) (w : Walk) : Walk := Id.run do
    let n := t.node i
    let mut w := w
    let mut alternative := alternative
    let mut related := related
    match n.multipart with
    | none =>
      let inlineRfc := isInlineRfc822 n && !force
      let exceeded := inlineRfc && depth ≥ maxRfc822Depth
      if inlineRfc && !exceeded then
        let (sub, subMap) := parseAt (n.content.getD .empty) (depth + 1)
        let item := TextItem.sub (formatTextHeader sub) (formatHtmlHeader sub)
        if sub.text.isSome || sub.html.isNone then
          w := { w with textMap := w.textMap.modifyKey [i] (·.push "plain" item) }
          w := w.addType "plain"
        if sub.html.isSome then
          w := { w with textMap := w.textMap.modifyKey [i] (·.push "html" item) }
          w := w.addType "html"
        for (k, e) in subMap do
          w := { w with textMap := w.textMap.set (i :: k) e }
        w := { w with attachments := w.attachments ++ sub.attachments }
      else if isInlineText n then
        let ty := String.ofList (n.contentType.value.toList.dropWhile (· != '/') |>.drop 1)
        let sel := alternative.getD i
        w := { w with textMap := w.textMap.modifyKey [sel] (·.push ty (.text (Parse.textContent n))) }
        w := w.addType ty
      else if n.content.isSome then
        w := { w with attachments := w.attachments.push (attachmentOf n related exceeded) }
    | some "alternative" => alternative := some i
    | some "related" => related := true
    | some _ => pure ()
    for c in n.children do
      w := walk t depth force c alternative related w
    return w
end

/-- Parse a raw message the way postal-mime 3.0 does. -/
def parse (raw : ByteArray) : Message := (parseAt raw 0).1

end Core.Mime
