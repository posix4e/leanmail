import Core.Base64
import Core.Rfc822
import Core.Mime.Address

/-! # Mail the server writes

Every message is built by `Core.Rfc822`; this module adapts the server's two shapes to it.

* `textMessage`: the server's own notices (sign-in links, welcome mail): From, To, Subject, Date,
  Message-ID, MIME-Version, `text/plain; charset=utf-8` sent as 7bit/8bit so a link stays one
  unbroken line, CRLF line ends.
* `message`: anything else (`Server.Send`): the full `Core.Rfc822.Message`, quoted-printable text,
  optional HTML and attachments, with a random boundary seed from the caller. -/

namespace Server.Mail.Build

structure Text where
  sender : String
  recipient : String
  subject : String
  body : String
  /-- Epoch milliseconds. -/
  date : Nat
  /-- With the angle brackets: `<ulid@host>`. -/
  messageId : String

def isAscii (s : String) : Bool := Core.Rfc822.isAscii s

def headerValue (s : String) : String := Core.Rfc822.headerSafe s

def encodeWord (s : String) : String := Core.Rfc822.encodeWords s

/-- LF or CRLF in, CRLF out. -/
def crlf (s : String) : String :=
  "\r\n".intercalate ((s.splitOn "\n").map fun l => if l.endsWith "\r" then String.ofList l.toList.dropLast else l)

/-- One address as the builder takes it: `Name <a@b>` or a bare `a@b`. -/
def addrOf (s : String) : Core.Rfc822.Addr :=
  match Core.Mime.Address.parseFlat s with
  | .mailbox n a :: _ => if a.isEmpty then { address := headerValue s } else { name := n, address := a }
  | _ => { address := headerValue s }

def textMessage (m : Text) : String :=
  Core.Rfc822.render {
    from_ := addrOf m.sender, to := [addrOf m.recipient], subject := m.subject, text := m.body,
    date := m.date, messageId := m.messageId, textEncoding := .eightBit } ""

/-- A full message as bytes (`Core.Rfc822.build`). -/
def message (m : Core.Rfc822.Message) (boundarySeed : String) : ByteArray := Core.Rfc822.build m boundarySeed

end Server.Mail.Build
