import Core.Text.Str
import Core.Mime.Rfc2047
import Core.Text.Html

/-! # Address fields: `From`, `To`, `Cc`, `Bcc`, `Reply-To`, `Sender`

A faithful port of postal-mime's `address-parser.js` (postal-mime 3.0.0): the same tokenizer
(operators `"` `(` `<` `,` `:` `;` and their closers, backslash escapes inside quotes, `noBreak` on a
closer glued to the next character, newlines as spaces, control bytes dropped, token values
trimmed), the same per-address state machine (text / address / comment / group, quoted-text
tracking, Apple Mail's `<` truncation, nested groups flattened, an address recovered from plain
text but never from quoted text, the encoded-words-only branch) and the same comma/semicolon
split. JavaScript's `trim` and `\s` are Unicode-aware, so this module uses `jsSpace` for both. -/

namespace Core.Mime

/-- One entry of an address field: a mailbox, or an RFC 5322 group of mailboxes. -/
inductive Addr where
  | mailbox (name : String) (address : String)
  | group (name : String) (members : List Addr)
  deriving Repr, BEq, Inhabited

/-- The display name (decoded); the group name for a group. Empty when there is none. -/
def Addr.name : Addr → String
  | .mailbox n _ => n
  | .group n _ => n

/-- The address of a mailbox (may be empty when only a name was given); `none` for a group. -/
def Addr.address? : Addr → Option String
  | .mailbox _ a => some a
  | .group _ _ => none

/-- Text rendering as postal-mime's `formatTextAddress`: `name <address>`, or just the address. -/
def Addr.formatMailbox (name address : String) : String :=
  if name.isEmpty then address else name ++ " <" ++ address ++ ">"

/-- Text rendering as postal-mime's `formatTextAddresses` for one entry: groups as `name:a, b;`. -/
partial def Addr.format : Addr → String
  | .mailbox n a => Addr.formatMailbox n a
  | .group n ms => n ++ ":" ++ ", ".intercalate (ms.map Addr.format) ++ ";"

namespace Address

open Core.Text Str

/-- JavaScript's `\s` (and the set `String.prototype.trim` removes). -/
def jsSpace (c : Char) : Bool :=
  c == ' ' || c == '\t' || c == '\n' || c == '\x0B' || c == '\x0C' || c == '\r' ||
  c == ' ' || c == ' ' || (' ' ≤ c && c ≤ ' ') || c == ' ' ||
  c == ' ' || c == ' ' || c == ' ' || c == '　' || c == '﻿'

/-- `String.prototype.trim`. -/
def jsTrim (s : String) : String := trimBy jsSpace s

private def jsTrimRight (s : String) : String :=
  String.ofList (s.toList.reverse.dropWhile jsSpace |>.reverse)

private def jsTrimLeft (s : String) : String := String.ofList (s.toList.dropWhile jsSpace)

/-- `postal-mime`'s `formatTextAddresses`: entries joined with `, `. -/
def formatList (as : List Addr) : String := ", ".intercalate (as.map Addr.format)

/-- One mailbox as postal-mime's `formatHtmlAddress`: a `mailto:` link showing the name, or the
address in angle brackets when there is no name. -/
def formatHtmlMailbox (name address : String) : String :=
  let shown := if name.isEmpty then "<" ++ address ++ ">" else name
  s!"<a href=\"mailto:{Html.escape address}\" class=\"postal-email-address\">{Html.escape shown}</a>"

mutual
  /-- The pieces `formatHtmlAddresses` pushes for one entry. -/
  private partial def htmlPieces : Addr → List String
    | .mailbox n a => [formatHtmlMailbox n a]
    | .group n ms =>
      [s!"<span class=\"postal-email-address-group\">{Html.escape n}:</span>"] ++ htmlPiecesList ms ++
        ["<span class=\"postal-email-address-group\">;</span>"]

  /-- Entries with a separator piece between them (`forEach`'s index restarts inside a group). -/
  private partial def htmlPiecesList (as : List Addr) : List String :=
    (as.zipIdx.map fun (a, i) =>
      (if i > 0 then ["<span class=\"postal-email-address-separator\">, </span>"] else []) ++ htmlPieces a).flatten
end

/-- postal-mime's `formatHtmlAddresses`: the pieces joined with a space. -/
def formatHtmlList (as : List Addr) : String := " ".intercalate (htmlPiecesList as)

structure Token where
  isOp : Bool
  value : String
  noBreak : Bool := false
  deriving Repr, Inhabited

/-- The closer each operator expects; `,` and `;` close nothing. -/
private def closerOf : Char → Option Char
  | '"' => some '"'
  | '(' => some ')'
  | '<' => some '>'
  | ',' => some '\x00'
  | ':' => some ';'
  | ';' => some '\x00'
  | _ => none

/-- The `Tokenizer` class. `'\x00'` stands for JavaScript's empty `operatorExpecting`. -/
def tokenize (s : String) : List Token := Id.run do
  let a := s.toList.toArray
  let n := a.size
  -- the list under construction; the open text node, if any, is the last entry with `node = true`
  let mut list : Array Token := #[]
  let mut node := false
  let mut expecting : Char := '\x00'
  let mut escaped := false
  for i in [0 : n] do
    let chr := a[i]!
    let next? := a[i+1]?
    let mut handled := false
    if escaped then
      pure ()
    else if expecting != '\x00' && chr == expecting then
      let nb := match next? with
        | some nc => !(nc == ' ' || nc == '\t' || nc == '\r' || nc == '\n' || nc == ',' || nc == ';')
        | none => false
      list := list.push { isOp := true, value := chr.toString, noBreak := nb }
      node := false
      expecting := '\x00'
      escaped := false
      handled := true
    else if expecting == '\x00' && (closerOf chr).isSome then
      list := list.push { isOp := true, value := chr.toString }
      node := false
      expecting := (closerOf chr).getD '\x00'
      escaped := false
      handled := true
    else if expecting == '"' && chr == '\\' then
      escaped := true
      handled := true
    if !handled then
      if !node then
        list := list.push { isOp := false, value := "" }
        node := true
      let c := if chr == '\n' then ' ' else chr
      if c.toNat ≥ 0x21 || c == ' ' || c == '\t' then
        let last := list.back!
        list := list.pop.push { last with value := last.value.push c }
      escaped := false
  return (list.toList.map fun t => { t with value := jsTrim t.value }).filter (!·.value.isEmpty)

/-- `findAddressInText`: the first whitespace-delimited token holding `\b[^@\s]+@[^\s]+\b`.
Returns `(index, length, value)` in characters. -/
def findAddressInText (text : String) : Option (Nat × Nat × String) := Id.run do
  let a := text.toList.toArray
  let n := a.size
  let mut i := 0
  while i < n do
    if jsSpace a[i]! then
      i := i + 1
      continue
    let s := i
    let mut e := i
    while e < n && !jsSpace a[e]! do e := e + 1
    i := e
    let tok := a.extract s e
    match tok.findIdx? (· == '@') with
    | none => continue
    | some at_ =>
      let mut start := 0
      while start < at_ && !isWordChar tok[start]! do start := start + 1
      if start ≥ at_ then continue
      let mut stop := tok.size
      while stop > at_ + 1 && !isWordChar tok[stop - 1]! do stop := stop - 1
      if stop ≤ at_ + 1 then continue
      return some (s + start, stop - start, slice tok start stop)
  return none

/-- `^[^@\s]+@[^@\s]+$` -/
private def isPlainAddress (s : String) : Bool :=
  let cs := s.toList
  !cs.any jsSpace && cs.count '@' == 1 && cs.head? != some '@' && cs.getLast? != some '@'

/-- `/<[^<>]+@[^<>]+>/.test(s)` -/
private def hasAngleAddress (s : String) : Bool := Id.run do
  let a := s.toList.toArray
  for i in [0 : a.size] do
    if a[i]! == '<' then
      let mut j := i + 1
      while j < a.size && a[j]! != '<' && a[j]! != '>' do j := j + 1
      if j < a.size && a[j]! == '>' then
        -- the segment is a[i+1 .. j); it needs an '@' with a character on each side
        for k in [i + 2 : j - 1] do
          if a[k]! == '@' then return true
  return false

/-- Apple Mail truncates everything between an unexpected `<` and an address: `^[^<]*<\s*`. -/
private def dropToAngle (s : String) : String :=
  let cs := s.toList
  if cs.contains '<' then String.ofList ((cs.dropWhile (· != '<')).drop 1 |>.dropWhile jsSpace) else s

private inductive St | text | address | comment | group
  deriving BEq

private structure Data where
  address : Array String := #[]
  comment : Array String := #[]
  group : Array String := #[]
  text : Array String := #[]
  quoted : Array Bool := #[]

private def Data.get (d : Data) : St → Array String
  | .text => d.text | .address => d.address | .comment => d.comment | .group => d.group

private def Data.set (d : Data) (st : St) (v : Array String) : Data :=
  match st with
  | .text => { d with text := v } | .address => { d with address := v }
  | .comment => { d with comment := v } | .group => { d with group := v }

/-- `MAX_NESTED_GROUP_DEPTH` -/
def maxDepth : Nat := 50

private def decode (s : String) : String := Rfc2047.decodeWords s

private def orElse (a b : String) : String := if a.isEmpty then b else a

/-- Split a token run into its data buckets (the first loop of `_handleAddress`). -/
private def collect (tokens : Array Token) : Data × Bool := Id.run do
  let mut isGroup := false
  let mut st := St.text
  let mut d : Data := {}
  let mut insideQuotes := false
  for i in [0 : tokens.size] do
    let token := tokens[i]!
    if token.isOp then
      match token.value with
      | "<" => st := .address; insideQuotes := false
      | "(" => st := .comment; insideQuotes := false
      | ":" => st := .group; isGroup := true; insideQuotes := false
      | "\"" => insideQuotes := !insideQuotes; st := .text
      | _ => st := .text; insideQuotes := false
    else if !token.value.isEmpty then
      let value := if st == .address then dropToAngle token.value else token.value
      let cur := d.get st
      let prevNoBreak := i > 0 && tokens[i-1]!.noBreak
      if prevNoBreak && !cur.isEmpty then
        d := d.set st (cur.set! (cur.size - 1) (cur.back! ++ value))
        if st == .text && insideQuotes then
          d := { d with quoted := d.quoted.set! (d.quoted.size - 1) true }
      else
        d := d.set st (cur.push value)
        if st == .text then d := { d with quoted := d.quoted.push insideQuotes }
  return (d, isGroup)

/-- Recover an address from plain text when no `<...>` was given. -/
private def detect (d : Data) : Data := Id.run do
  let mut d := d
  let wasQuoted (d : Data) (i : Nat) := d.quoted[i]?.getD false
  let mut i := d.text.size
  while i > 0 do
    i := i - 1
    if !wasQuoted d i && isPlainAddress d.text[i]! then
      d := { d with address := #[d.text[i]!], text := d.text.eraseIdx! i,
                    quoted := if i < d.quoted.size then d.quoted.eraseIdx! i else d.quoted }
      return d
  -- still no address: an address with more than one '@'
  i := d.text.size
  while i > 0 do
    i := i - 1
    if !wasQuoted d i then
      let t := d.text[i]!
      match findAddressInText t with
      | some (idx, len, value) =>
        let a := t.toList.toArray
        let before := jsTrimRight (slice a 0 idx)
        let after := jsTrimLeft (slice a (idx + len) a.size)
        return { d with address := #[value], text := d.text.set! i (jsTrim (before ++ " " ++ after)) }
      | none => d := { d with text := d.text.set! i (jsTrim t) }
  return d

mutual

/-- `_handleAddress` -/
partial def handleAddress (tokens : Array Token) (depth : Nat) : List Addr :=
  let (d, isGroup) := collect tokens
  -- no text but a comment: the comment is the name
  let d := if d.text.isEmpty && !d.comment.isEmpty then { d with text := d.comment, comment := #[] } else d
  if isGroup then
    let text := " ".intercalate d.text.toList
    let members :=
      if d.group.isEmpty then []
      else (parseAt (",".intercalate d.group.toList) (depth + 1)).flatMap fun
        | .group _ ms => ms
        | m => [m]
    [.group (decode text) members]
  else
    let d := if d.address.isEmpty && !d.text.isEmpty then detect d else d
    let d := if d.text.isEmpty && !d.comment.isEmpty then { d with text := d.comment, comment := #[] } else d
    -- keep the first address, the rest becomes text
    let d := if d.address.size > 1 then { d with text := d.text ++ d.address.extract 1 d.address.size,
                                                  address := #[d.address[0]!] } else d
    let text := " ".intercalate d.text.toList
    let address := " ".intercalate d.address.toList
    if address.isEmpty && Rfc2047.isEncodedWordsOnly (jsTrim text) then
      let decoded := decode text
      -- only re-parse when the decoded text holds an angle-bracket address: a bare encoded
      -- address must not become a recipient
      let sub := if hasAngleAddress decoded then parseAt decoded 0 else []
      if !sub.isEmpty then sub else [.mailbox decoded ""]
    else
      let addr := orElse address text
      let name := decode (orElse text address)
      if addr == name then
        if addr.toList.contains '@' then [.mailbox "" addr] else [.mailbox name ""]
      else [.mailbox name addr]

/-- `addressParser(str, {_depth})` -/
partial def parseAt (s : String) (depth : Nat) : List Addr :=
  if depth > maxDepth then [] else
  let tokens := tokenize s
  let (groups, cur) := tokens.foldl (init := (#[], #[])) fun (acc : Array (Array Token) × Array Token) t =>
    let (gs, cur) := acc
    if t.isOp && (t.value == "," || t.value == ";") then
      (if cur.isEmpty then gs else gs.push cur, #[])
    else (gs, cur.push t)
  let groups := if cur.isEmpty then groups else groups.push cur
  groups.toList.flatMap fun g => handleAddress g depth

end

/-- Parse an address field (`addressParser(str)`). -/
def parse (s : String) : List Addr := parseAt s 0

/-- Groups replaced by their members, recursively (`addressParser(str, {flatten: true})`). -/
partial def flatten : List Addr → List Addr
  | [] => []
  | .group _ ms :: rest => flatten ms ++ flatten rest
  | m :: rest => m :: flatten rest

/-- The mailboxes of a field with groups flattened. -/
def parseFlat (s : String) : List Addr := flatten (parse s)

end Address
end Core.Mime
