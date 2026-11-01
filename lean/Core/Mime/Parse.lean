import Core.Base64
import Core.Text.Str
import Core.Text.Charset
import Core.Mime.Header

namespace Core.Mime.Parse

/-! # The MIME tree

A port of postal-mime's line reader and `MimeNode`: the message is cut into lines on LF (each
line loses its trailing CR/LF run), a line starting with `--` is checked against the registered
boundaries (the most recently registered first), and every other line is fed to the current node,
first as a header line and, after the empty line, to the node's transfer decoder. Nodes live in one
array and refer to each other by index; node 0 is the root.

Two caps keep a hostile message linear (postal-mime has neither): multipart nesting stops at
`maxDepth` and the tree at `maxParts` nodes. A multipart part past the depth, or the part that
would pass the count, becomes one opaque `application/octet-stream` leaf that takes the rest of
its content (`Node.sealed`); `Core.Mime` makes neither text nor an attachment of it. Since only
the open ancestors' boundaries are registered, a `--` line is checked against at most `maxDepth`
of them. Nothing here throws. -/

open Core.Text Str

inductive State | header | body | finished
  deriving BEq, Inhabited

/-- Multipart levels kept; a multipart part deeper than this is opaque. -/
def maxDepth : Nat := 32
/-- Nodes kept in one tree; past this the rest of the message is one opaque node. -/
def maxParts : Nat := 1000

/-- The transfer decoders: base64, quoted-printable, and pass-through for everything else. -/
inductive Decoder | base64 | qp | pass
  deriving BEq, Inhabited

structure Node where
  parent : Option Nat := none
  children : Array Nat := #[]
  /-- Children before this index are already finalized. -/
  finalizedChildCount : Nat := 0
  depth : Nat := 0
  state : State := .header
  headerLines : Array String := #[]
  /-- All header fields, in order. -/
  headers : Array Header.Field := #[]
  /-- The raw `Content-Type` value (the first one), or the default. -/
  contentTypeRaw : String := "text/plain"
  contentType : Header.Structured := {}
  /-- The subtype when the type is `multipart/*`. -/
  multipart : Option String := none
  transferEncodingRaw : String := "8bit"
  /-- The first `[\w-]+` token of the transfer encoding. -/
  transferEncoding : String := ""
  dispositionRaw : String := ""
  disposition : Header.Structured := {}
  contentId : Option String := none
  contentDescription : Option String := none
  /-- True once `processHeaders` ran. -/
  processed : Bool := false
  decoder : Decoder := .pass
  /-- Decoded body bytes so far. -/
  body : ByteArray := .empty
  /-- Base64 characters not yet decoded (fewer than four, or a unit waiting for its end). -/
  pending : ByteArray := .empty
  /-- The decoded body, set when the node is finalized. -/
  content : Option ByteArray := none
  /-- Past a cap (`maxDepth`, `maxParts`): the rest of the content, kept undecoded and unparsed. -/
  sealed : Bool := false
  deriving Inhabited

structure Tree where
  nodes : Array Node := #[{}]
  /-- Registered boundaries (UTF-8 bytes) and the multipart node they belong to. -/
  boundaries : Array (ByteArray × Nat) := #[]
  current : Nat := 0
  /-- `maxParts` was reached: no more boundaries are matched, every line goes to the current node. -/
  capped : Bool := false
  deriving Inhabited

namespace Tree

def node (t : Tree) (i : Nat) : Node := t.nodes[i]!

@[inline] def modifyNode (t : Tree) (i : Nat) (f : Node → Node) : Tree :=
  { t with nodes := t.nodes.modify i f }

/-- Folded header lines, the five `Content-*` headers (first occurrence wins), the structured
parse of type and disposition, the boundary, and the transfer decoder. -/
def processHeaders (t : Tree) (i : Nat) : Tree := Id.run do
  let n := t.node i
  let headers := Header.parseLines n.headerLines
  let mut ct := n.contentTypeRaw
  let mut cte := n.transferEncodingRaw
  let mut cd := n.dispositionRaw
  let mut cid := n.contentId
  let mut desc := n.contentDescription
  let mut seen : List String := []
  for h in headers do
    let k := h.key
    if (k == "content-type" || k == "content-transfer-encoding" || k == "content-disposition" ||
        k == "content-id" || k == "content-description") && !seen.contains k then
      seen := k :: seen
      if k == "content-type" then ct := h.value
      else if k == "content-transfer-encoding" then cte := h.value
      else if k == "content-disposition" then cd := h.value
      else if k == "content-id" then cid := some h.value
      else desc := some h.value
  let mut parsed := Header.parseStructured ct
  let mut multipart :=
    if parsed.value.toLower.startsWith "multipart/" then
      some (String.ofList (parsed.value.toList.drop 10))
    else none
  -- too deep, or too many parts: an opaque leaf, its boundary never registered
  let sealed := multipart.isSome && (n.depth ≥ maxDepth || t.nodes.size ≥ maxParts)
  if sealed then
    ct := "application/octet-stream"
    parsed := Header.parseStructured ct
    multipart := none
  let encoding := Header.encodingToken cte
  let lower := encoding.toLower
  let decoder :=
    if sealed then Decoder.pass
    else if (lower.splitOn "base64").length > 1 then Decoder.base64
    else if (lower.splitOn "quoted-printable").length > 1 then Decoder.qp
    else Decoder.pass
  let mut t := t.modifyNode i fun n => { n with
    headers, contentTypeRaw := ct, contentType := parsed, multipart,
    transferEncodingRaw := cte, transferEncoding := encoding,
    dispositionRaw := cd, disposition := Header.parseStructured cd,
    contentId := cid, contentDescription := desc,
    processed := true, decoder, headerLines := #[], sealed }
  if multipart.isSome then
    let b := parsed.param "boundary"
    if !b.isEmpty then
      t := { t with boundaries := t.boundaries.push (b.toUTF8, i) }
  return t

private def isB64 (x : UInt8) : Bool :=
  (65 ≤ x && x ≤ 90) || (97 ≤ x && x ≤ 122) || (48 ≤ x && x ≤ 57) || x == 43 || x == 47

private def hexNibble (x : UInt8) : Option Nat :=
  if 48 ≤ x && x ≤ 57 then some (x.toNat - 48)
  else if 97 ≤ x && x ≤ 102 then some (x.toNat - 97 + 10)
  else if 65 ≤ x && x ≤ 70 then some (x.toNat - 65 + 10)
  else none

private def b64Value (x : UInt8) : UInt32 :=
  if 65 ≤ x && x ≤ 90 then x.toUInt32 - 65
  else if 97 ≤ x && x ≤ 122 then x.toUInt32 - 71
  else if 48 ≤ x && x ≤ 57 then x.toUInt32 + 4
  else if x == 43 then 62 else 63

/-- Base64: keep `[A-Za-z0-9+/=]`; every `=` closes the open unit, which is decoded on its own
(some mailers pad every line). Whole groups of four decode as they arrive, which gives the same
bytes as decoding the unit at once. -/
private def base64Update (body pending : ByteArray) (line : ByteArray) : ByteArray × ByteArray := Id.run do
  let mut body := body
  let mut pending := pending
  for x in line do
    if x == 61 then
      if !pending.isEmpty then
        body := body ++ Base64.decodePostal pending
        pending := .empty
    else if isB64 x then
      pending := pending.push x
      if pending.size == 4 then
        let n := (b64Value pending[0]! <<< 18) ||| (b64Value pending[1]! <<< 12) |||
          (b64Value pending[2]! <<< 6) ||| b64Value pending[3]!
        body := body.push (n >>> 16).toUInt8 |>.push (n >>> 8).toUInt8 |>.push n.toUInt8
        pending := .empty
  return (body, pending)

/-- Quoted-printable, one line: `=XX` is a byte, a final `=` is a soft break, otherwise the line
ends with LF. -/
private def qpUpdate (body : ByteArray) (line : ByteArray) : ByteArray := Id.run do
  let mut len := line.size
  let soft := len > 0 && line[len - 1]! == 61
  if soft then len := len - 1
  let mut body := body
  let mut i := 0
  while i < len do
    let x := line[i]!
    if x == 61 && i + 2 < len then
      match hexNibble line[i + 1]!, hexNibble line[i + 2]! with
      | some h, some l =>
        body := body.push (h * 16 + l).toUInt8
        i := i + 3
        continue
      | _, _ => pure ()
    body := body.push x
    i := i + 1
  if !soft then body := body.push 10
  return body

def feed (t : Tree) (line : ByteArray) : Tree :=
  let i := t.current
  let n := t.node i
  match n.state with
  | .header =>
    if line.isEmpty then
      (t.modifyNode i fun n => { n with state := .body }).processHeaders i
    else
      t.modifyNode i fun n => { n with headerLines := n.headerLines.push (Charset.utf8Lossy line) }
  | .body =>
    t.modifyNode i fun n =>
      -- take the buffers out of the node first, so they are updated in place
      let body := n.body
      let pending := n.pending
      let n := { n with body := .empty, pending := .empty }
      match n.decoder with
      | .base64 =>
        let (body, pending) := base64Update body pending line
        { n with body, pending }
      | .qp => { n with body := qpUpdate body line }
      | .pass => { n with body := (body ++ line).push 10 }
  | .finished => t

mutual
  partial def finalize (t : Tree) (i : Nat) : Tree := Id.run do
    let n := t.node i
    if n.state == .finished then return t
    let mut t := if n.state == .header then t.processHeaders i else t
    -- remove this node's boundary (the last registration of it)
    match (t.boundaries.toList.reverse.findIdx? (·.2 == i)) with
    | some r =>
      let idx := t.boundaries.size - 1 - r
      t := { t with boundaries := t.boundaries.eraseIdx! idx }
    | none => pure ()
    t := finalizeChildNodes t i
    t := t.modifyNode i fun n =>
      let body := if n.decoder == .base64 && !n.pending.isEmpty then n.body ++ Base64.decodePostal n.pending else n.body
      { n with content := some body, body := .empty, pending := .empty, state := .finished }
    return t

  partial def finalizeChildNodes (t : Tree) (i : Nat) : Tree :=
    let n := t.node i
    if n.finalizedChildCount < n.children.size then
      let c := n.children[n.finalizedChildCount]!
      let t := t.modifyNode i fun n => { n with finalizedChildCount := n.finalizedChildCount + 1 }
      finalizeChildNodes (finalize t c) i
    else t
end

/-- Is `line` a delimiter of `b`: `--b` or `--b--`, then only SP/HTAB? -/
private def delimiter? (line b : ByteArray) : Option Bool := Id.run do
  if line.size < b.size + 2 then return none
  for j in [0 : b.size] do
    if line[j + 2]! != b[j]! then return none
  let mut stop := b.size + 2
  let mut term := false
  if line.size ≥ b.size + 4 && line[b.size + 2]! == 45 && line[b.size + 3]! == 45 then
    term := true
    stop := b.size + 4
  for j in [stop : line.size] do
    if line[j]! != 32 && line[j]! != 9 then return none
  return some term

def processLine (t : Tree) (line : ByteArray) : Tree := Id.run do
  if !t.capped && !t.boundaries.isEmpty && line.size > 2 && line[0]! == 45 && line[1]! == 45 then
    let mut k := t.boundaries.size
    while k > 0 do
      k := k - 1
      let (b, owner) := t.boundaries[k]!
      match delimiter? line b with
      | none => continue
      | some true =>
        let t := t.finalize owner
        return { t with current := (t.node owner).parent.getD 0 }
      | some false =>
        let t := t.finalizeChildNodes owner
        let o := t.node owner
        if t.nodes.size + 1 ≥ maxParts then
          -- the last node this tree gets: everything after this line, unparsed
          let child : Node := {
            parent := some owner, depth := o.depth + 1, state := .body, processed := true, sealed := true,
            contentTypeRaw := "application/octet-stream", contentType := Header.parseStructured "application/octet-stream" }
          let idx := t.nodes.size
          let t := { t with nodes := t.nodes.push child, capped := true }
          let t := t.modifyNode owner fun o => { o with children := o.children.push idx }
          return { t with current := idx }
        let parentType := o.multipart
        let child : Node := {
          parent := some owner, depth := o.depth + 1,
          contentTypeRaw := if parentType == some "digest" then "message/rfc822" else "text/plain" }
        let idx := t.nodes.size
        let t := { t with nodes := t.nodes.push child }
        let t := t.modifyNode owner fun o => { o with children := o.children.push idx }
        return { t with current := idx }
  return t.feed line

end Tree

/-- The index of the next LF at or after `pos`, or the end. -/
private partial def nextLf (raw : ByteArray) (pos : Nat) : Nat :=
  if pos < raw.size && raw[pos]! != 10 then nextLf raw (pos + 1) else pos

/-- Read `raw` line by line into a finalized tree: split on LF, each line without its trailing CRs
(postal-mime's `readLine`). `none` for empty input, where postal-mime never finalizes (or even
reads the headers of) its root. -/
def parseTree (raw : ByteArray) : Option Tree := Id.run do
  if raw.isEmpty then return none
  let mut t : Tree := {}
  let mut pos := 0
  while pos < raw.size do
    let lf := nextLf raw pos
    let mut stop := lf
    while stop > pos && raw[stop - 1]! == 13 do stop := stop - 1
    t := t.processLine (raw.extract pos stop)
    pos := lf + 1
  return some (t.finalize 0)

/-! ## Text content -/

/-- RFC 3676 `format=flowed`: unstuff, then join soft-broken lines (a line ending in a space,
except the signature separator); with `delsp=yes` the trailing space goes too. -/
def decodeFlowed (s : String) (delSp : Bool) : String := Id.run do
  let ls := (Str.lines s).map fun l => if l.startsWith " " then String.ofList (l.toList.drop 1) else l
  match ls with
  | [] => return ""
  | first :: rest =>
    let mut acc := first
    for cur in rest do
      if acc.endsWith " " && acc != "-- " && !acc.endsWith "\n-- " then
        acc := (if delSp then String.ofList acc.toList.dropLast else acc) ++ cur
      else
        acc := acc ++ "\n" ++ cur
    return acc

private def eqCI (a b : String) : Bool := a.toLower == b

/-- postal-mime's `getTextContent`: the content in the part's charset, unflowed if flowed. -/
def textContent (n : Node) : String :=
  match n.content with
  | none => ""
  | some bytes =>
    let s := Charset.decode (n.contentType.param "charset") bytes
    if eqCI (n.contentType.param "format") "flowed" then
      decodeFlowed s (eqCI (n.contentType.param "delsp") "yes")
    else s

end Core.Mime.Parse
