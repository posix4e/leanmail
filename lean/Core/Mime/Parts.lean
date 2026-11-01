import Core.Base64
import Core.Mime

/-! # Parts: the body of a message as one record

A port of MailVault's `shared/parts.js` `buildParts`: the text, the HTML and every attachment with
its metadata and its bytes as base64. Past `attachmentsMax` bytes of attachments the data is left
out and `attachmentsOmitted` is set; the metadata stays. -/

namespace Core.Mime

structure PartAttachment where
  name : String
  mime : String
  size : Nat
  cid : String
  inline : Bool
  method : Option String := none
  /-- Standard base64 with padding; `none` when the record omits attachment data. -/
  data : Option String := none
  deriving Inhabited, Repr

structure Parts where
  v : Nat := 1
  text : String
  html : Option String
  attachments : Array PartAttachment
  attachmentsOmitted : Bool := false
  deriving Inhabited, Repr

def attachmentsMax : Nat := 8 * 1024 * 1024

/-- `String.prototype.slice(0, n)`: at most `n` UTF-16 code units (a character that would be cut
in half is left out). -/
def sliceUtf16 (s : String) (n : Nat) : String := Id.run do
  let mut out := ""
  let mut units := 0
  for c in s.toList do
    let w := if c.toNat > 0xFFFF then 2 else 1
    if units + w > n then break
    out := out.push c
    units := units + w
  return out

def buildParts (m : Message) : Parts := Id.run do
  let mut atts : Array PartAttachment := #[]
  let mut total := 0
  for a in m.attachments do
    total := total + a.content.size
    let name := a.filename.getD ""
    let mime := if a.mimeType.isEmpty then "application/octet-stream" else a.mimeType
    atts := atts.push {
      name := sliceUtf16 name 128
      mime := sliceUtf16 mime 80
      size := a.content.size
      cid := sliceUtf16 (a.contentId.getD "") 200
      inline := a.disposition == some "inline" || a.related
      method := (a.method.bind fun s => if s.isEmpty then none else some s).map (sliceUtf16 · 20)
      data := some (Base64.encode a.content) }
  let omitted := total > attachmentsMax
  if omitted then atts := atts.map ({ · with data := none })
  return { text := m.text.getD "", html := m.html, attachments := atts, attachmentsOmitted := omitted }

end Core.Mime
