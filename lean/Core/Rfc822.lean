import Core.Base64
import Core.Mime.Date
import Core.Text.Str

/-! # Building an RFC 5322 message

The one builder for mail the server writes: sends, replies, forwards, and its own sign-in mail.
Pure: the caller supplies the date, the Message-ID and a random `boundarySeed`.

* CRLF line ends throughout; header fields folded at whitespace to stay within 78 characters
  where the content allows (a single long word is never split).
* Header values lose CR, LF, NUL, U+2028 and U+2029 (no header injection).
* A non-ASCII subject or display name becomes RFC 2047 B-words (`=?UTF-8?B?…?=`), each at
  most 39 bytes of UTF-8 so a word fits a line, split only between characters.
* Addresses are `Name <addr>`; a name with specials is quoted, `"` and `\` escaped.
* Text only is a `text/plain; charset=utf-8` quoted-printable body; text and HTML are a
  `multipart/alternative`; with attachments a `multipart/mixed` wraps the text (or the
  alternative) and one base64 part per attachment, 76-character lines, with
  `filename="…"` and, for a non-ASCII name, RFC 2231 `filename*=UTF-8''…`.
* Boundaries start with `=_`, which quoted-printable never writes literally and base64 never
  contains, so no body line can be mistaken for one.

Port of MailVault's `src/mime.ts`, plus HTML and the alternative part. -/

namespace Core.Rfc822

structure Addr where
  name : String := ""
  address : String
  deriving Repr, BEq, Inhabited

structure Attachment where
  filename : String
  mime : String
  data : ByteArray
  deriving Inhabited

/-- How a text body is written. `eightBit` is for the server's own short notices, where a link
must stay one unbroken line (lines must then be at most 998 bytes). -/
inductive TextEncoding where
  | quotedPrintable
  | eightBit
  deriving Repr, BEq, Inhabited

structure Message where
  from_ : Addr
  to : List Addr := []
  cc : List Addr := []
  bcc : List Addr := []
  replyTo : Option Addr := none
  subject : String := ""
  text : String := ""
  html : Option String := none
  inReplyTo : Option String := none
  /-- Space-separated `<id>` list. -/
  references : Option String := none
  attachments : List Attachment := []
  /-- Epoch milliseconds. -/
  date : Nat
  /-- With or without the angle brackets. -/
  messageId : String
  /-- `false` for the copy that goes on the wire: no `Bcc` header. -/
  includeBcc : Bool := true
  textEncoding : TextEncoding := .quotedPrintable
  deriving Inhabited

/-! ## Header pieces -/

def isAscii (s : String) : Bool := s.toList.all (·.toNat < 128)

/-- Printable ASCII and space/tab only. -/
def isPlainAscii (s : String) : Bool := s.toList.all fun c => (c.toNat ≥ 0x20 && c.toNat < 0x7F) || c == '\t'

/-- A header value without line breaks, NUL or the Unicode line separators. -/
def headerSafe (s : String) : String :=
  String.ofList (s.toList.filter fun c => c != '\r' && c != '\n' && c != '\x00' && c != ' ' && c != ' ')

/-- Split UTF-8 into runs of at most `n` bytes, never inside a character. -/
def utf8Chunks (s : String) (n : Nat) : List ByteArray := Id.run do
  let mut out : Array ByteArray := #[]
  let mut cur : ByteArray := .empty
  for c in s.toList do
    let b := c.toString.toUTF8
    if cur.size + b.size > n && cur.size > 0 then
      out := out.push cur
      cur := .empty
    cur := cur ++ b
  if cur.size > 0 then out := out.push cur
  return out.toList

/-- RFC 2047 B-words for `s`, space-separated (the space between two words is not part of the
decoded text). ASCII without `=?` is returned unchanged. -/
def encodeWords (s : String) : String :=
  if isPlainAscii s && (s.splitOn "=?").length ≤ 1 then s
  else " ".intercalate ((utf8Chunks s 39).map fun b => s!"=?UTF-8?B?{Base64.encode b}?=")

/-- Whether a display name needs quoting (RFC 5322 specials). -/
def needsQuote (s : String) : Bool := s.toList.any fun c => "()<>[]:;@\\,.\"".toList.contains c

def quoteString (s : String) : String :=
  "\"" ++ String.join (s.toList.map fun c => if c == '"' || c == '\\' then "\\" ++ c.toString else c.toString) ++ "\""

def displayName (n : String) : String :=
  if !isPlainAscii n || (n.splitOn "=?").length > 1 then encodeWords n
  else if needsQuote n then quoteString n else n

def formatAddr (a : Addr) : String :=
  let addr := headerSafe a.address
  let name := Text.Str.trim (headerSafe a.name)
  if name.isEmpty then addr else s!"{displayName name} <{addr}>"

def formatList (as : List Addr) : String := ", ".intercalate (as.map formatAddr)

/-- `<id>`: the brackets added when missing. -/
def bracketed (id : String) : String :=
  let t := Text.Str.trim (headerSafe id)
  if t.startsWith "<" then t else s!"<{t}>"

/-- Fold `Name: value` at spaces so lines stay within `limit` characters where they can; the
fold is a CRLF inserted before an existing space, so unfolding gives the value back. -/
def fold (line : String) (limit : Nat := 78) : String := Id.run do
  let cs := line.toList.toArray
  -- never break inside the field name or right after the colon
  let minBreak := (match cs.toList.findIdx? (· == ':') with | some i => i + 2 | none => 1)
  let mut out := ""
  let mut start := 0
  while cs.size - start > limit do
    -- the last space within the limit, else the first one after it
    let mut brk : Option Nat := none
    let mut j := start + limit
    while j > start && brk.isNone do
      if cs[j]! == ' ' && j ≥ minBreak && j > start then brk := some j
      j := j - 1
    if brk.isNone then
      let mut k := start + limit + 1
      while k < cs.size && brk.isNone do
        if cs[k]! == ' ' && k ≥ minBreak then brk := some k
        k := k + 1
    match brk with
    | some b =>
      out := out ++ String.ofList (cs.extract start b).toList ++ "\r\n"
      start := b
    | none => break
  return out ++ String.ofList (cs.extract start cs.size).toList

def header (name value : String) : String := fold s!"{name}: {value}" ++ "\r\n"

/-! ## Bodies -/

private def hexUpper (n : Nat) : String :=
  let d (k : Nat) : Char := "0123456789ABCDEF".toList.getD k '0'
  String.ofList [d (n / 16), d (n % 16)]

/-- LF, CRLF or a lone CR → LF. -/
def normalizeNewlines (s : String) : String :=
  let s := s.replace "\r\n" "\n"
  s.replace "\r" "\n"

/-- Quoted-printable (RFC 2045 §6.7) of UTF-8 text, CRLF line ends, every line at most 76
characters: soft breaks (`=` at the end) never split an escape; `=`, control bytes and
non-ASCII are escaped; a space or tab ending a line is escaped. The result ends with CRLF. -/
def quotedPrintable (text : String) : String := Id.run do
  let lines := (normalizeNewlines text).splitOn "\n"
  -- one trailing newline is the end of the last line, not an empty line after it
  let lines := if lines.length > 1 && lines.getLast? == some "" then lines.dropLast else lines
  let mut out := ""
  for l in lines do
    let bytes := l.toUTF8
    let mut toks : Array String := #[]
    for h : i in [:bytes.size] do
      let b := bytes[i]
      let last := i + 1 == bytes.size
      let tok :=
        if (b == 32 || b == 9) then (if last then "=" ++ hexUpper b.toNat else (Char.ofNat b.toNat).toString)
        else if b ≥ 33 && b ≤ 126 && b != 61 then (Char.ofNat b.toNat).toString
        else "=" ++ hexUpper b.toNat
      toks := toks.push tok
    let mut line := ""
    for tok in toks do
      if line.length + tok.length > 75 then
        out := out ++ line ++ "=\r\n"
        line := ""
      line := line ++ tok
    out := out ++ line ++ "\r\n"
  return out

/-- Base64 in 76-character lines, each ending CRLF. -/
def base64Lines (b : ByteArray) : String := Id.run do
  let s := (Base64.encode b).toList.toArray
  let mut out := ""
  let mut i := 0
  while i < s.size do
    out := out ++ String.ofList (s.extract i (i + 76)).toList ++ "\r\n"
    i := i + 76
  return out

/-- CRLF line ends, ending with one. -/
def crlfText (s : String) : String :=
  let body := "\r\n".intercalate ((normalizeNewlines s).splitOn "\n")
  if body.endsWith "\r\n" then body else body ++ "\r\n"

/-- RFC 2231 `attr-char`s pass, every other byte is `%XX`. -/
def percentEncode (s : String) : String :=
  String.join <| s.toUTF8.toList.map fun b =>
    let c := Char.ofNat b.toNat
    if c.isAlphanum && b < 128 || "!#$&+.^_`|~-".toList.contains c then c.toString else "%" ++ hexUpper b.toNat

/-- A readable ASCII stand-in: non-ASCII and controls become `_`. -/
def asciiFallback (s : String) : String :=
  String.ofList (s.toList.map fun c => if c.toNat < 0x20 || c.toNat ≥ 0x7F then '_' else c)

/-- `name="…"`, and for a non-ASCII value also `name*=UTF-8''…` (as `name*0*=`, `name*1*=`…
sections of at most 48 characters when long). -/
def param (name value : String) : String :=
  let value := headerSafe value
  let plain := s!"{name}={quoteString (asciiFallback value)}"
  if isPlainAscii value then plain else
  let enc := (percentEncode value).toList.toArray
  if enc.size + 10 ≤ 60 then s!"{plain}; {name}*=UTF-8''{String.ofList enc.toList}" else Id.run do
    -- sections never split a %XX escape
    let mut parts : Array String := #[]
    let mut i := 0
    while i < enc.size do
      let mut j := min enc.size (i + 48)
      if j < enc.size then
        if j ≥ 1 && enc[j - 1]! == '%' then j := j - 1
        else if j ≥ 2 && enc[j - 2]! == '%' then j := j - 2
      parts := parts.push (String.ofList (enc.extract i j).toList)
      i := j
    let sections := parts.toList.zipIdx.map fun (p, k) =>
      if k == 0 then s!"{name}*0*=UTF-8''{p}" else s!"{name}*{k}*={p}"
    return plain ++ "; " ++ "; ".intercalate sections

/-- A `type/subtype` token, else `application/octet-stream`. -/
def safeMime (m : String) : String :=
  let ok := !m.isEmpty && m.length ≤ 127 && (m.splitOn "/").length == 2 &&
    m.toList.all fun c => c.isAlphanum || "!#$%&'*+.^_`|~-/".toList.contains c
  if ok then m.toLower else "application/octet-stream"

def textPart (subtype body : String) (enc : TextEncoding) : String :=
  match enc with
  | .quotedPrintable =>
    header "Content-Type" s!"text/{subtype}; charset=utf-8" ++
      header "Content-Transfer-Encoding" "quoted-printable" ++ "\r\n" ++ quotedPrintable body
  | .eightBit =>
    header "Content-Type" s!"text/{subtype}; charset=utf-8" ++
      header "Content-Transfer-Encoding" (if isAscii body then "7bit" else "8bit") ++ "\r\n" ++ crlfText body

def attachmentPart (a : Attachment) : String :=
  header "Content-Type" s!"{safeMime a.mime}; {param "name" a.filename}" ++
    header "Content-Disposition" s!"attachment; {param "filename" a.filename}" ++
    header "Content-Transfer-Encoding" "base64" ++ "\r\n" ++ base64Lines a.data

/-- `--b` parts `--b--`. -/
def multipart (boundary : String) (parts : List String) : String :=
  String.join (parts.map fun p => s!"--{boundary}\r\n{p}") ++ s!"--{boundary}--\r\n"

/-- The boundary seed as letters and digits, at most 40. -/
def seedClean (s : String) : String :=
  let t := String.ofList ((s.toList.filter Char.isAlphanum).take 40)
  if t.isEmpty then "0" else t

/-- The body's own headers (Content-Type, Content-Transfer-Encoding) and the body. -/
def bodyOf (m : Message) (seed : String) : String :=
  let seed := seedClean seed
  let textOnly := textPart "plain" m.text m.textEncoding
  let main := match m.html with
    | some h =>
      let b := s!"=_lm_alt_{seed}"
      header "Content-Type" s!"multipart/alternative; boundary=\"{b}\"" ++ "\r\n" ++
        multipart b [textOnly, textPart "html" h m.textEncoding]
    | none => textOnly
  if m.attachments.isEmpty then main else
    let b := s!"=_lm_mix_{seed}"
    header "Content-Type" s!"multipart/mixed; boundary=\"{b}\"" ++ "\r\n" ++
      multipart b (main :: m.attachments.map attachmentPart)

def headers (m : Message) : String :=
  header "From" (formatAddr m.from_) ++
  (if m.to.isEmpty then "" else header "To" (formatList m.to)) ++
  (if m.cc.isEmpty then "" else header "Cc" (formatList m.cc)) ++
  (if m.bcc.isEmpty || !m.includeBcc then "" else header "Bcc" (formatList m.bcc)) ++
  (match m.replyTo with | some r => header "Reply-To" (formatAddr r) | none => "") ++
  header "Subject" (encodeWords (headerSafe m.subject)) ++
  header "Date" (Mime.Date.formatRfc5322 m.date) ++
  header "Message-ID" (bracketed m.messageId) ++
  (match m.inReplyTo with
   | some r => if (Text.Str.trim (headerSafe r)).isEmpty then "" else header "In-Reply-To" (bracketed r)
   | none => "") ++
  (match m.references with
   | some r => let r := Text.Str.trim (headerSafe r); if r.isEmpty then "" else header "References" r
   | none => "") ++
  header "MIME-Version" "1.0"

/-- The message as text. -/
def render (m : Message) (boundarySeed : String) : String := headers m ++ bodyOf m boundarySeed

/-- The message as bytes. -/
def build (m : Message) (boundarySeed : String) : ByteArray := (render m boundarySeed).toUTF8

end Core.Rfc822
