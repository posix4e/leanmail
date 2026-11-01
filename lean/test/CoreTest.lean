import Core
import Lean.Data.Json

/-! Core tests: pure, vector-driven. One executable, one line per check, exit 1 on any failure.
`docs/SPEC.md` is generated from the check names, so write them as sentences. -/

structure Tally where
  passed : Nat := 0
  failed : Nat := 0

def check (t : IO.Ref Tally) (name : String) (ok : Bool) : IO Unit := do
  if ok then
    IO.println s!"ok   {name}"
    t.modify fun x => { x with passed := x.passed + 1 }
  else
    IO.println s!"FAIL {name}"
    t.modify fun x => { x with failed := x.failed + 1 }

/-! ## MIME vectors: postal-mime + `buildParts` output for each raw message -/

def esc (s : String) : String := (s.replace "\\" "\\\\").replace "\n" "\\n"

/-- A value for a failure line: whole when short, else ~120 characters around `at`. -/
def clipAt (s : String) (at_ : Nat) : String :=
  let cs := s.toList
  if cs.length ≤ 120 then esc s else
  let start := at_ - min at_ 40
  (if start > 0 then "…" else "") ++ esc (String.ofList ((cs.drop start).take 120)) ++
    (if start + 120 < cs.length then "…" else "")

/-- Where two strings first differ. -/
def firstDiff (a b : String) : Nat :=
  ((a.toList.zip b.toList).findIdx? (fun (x, y) => x != y)).getD (min a.length b.length)

def showPair (expected got : Option String) : String :=
  let at_ := match expected, got with | some a, some b => firstDiff a b | _, _ => 0
  let one : Option String → String
    | some s => "\"" ++ clipAt s at_ ++ "\""
    | none => "null"
  s!"expected {one expected} got {one got}"

/-- The first difference between the expected `parts` JSON and ours, if any. -/
def partsDiff (exp : Lean.Json) (got : Core.Mime.Parts) : Option String := Id.run do
  let str (j : Lean.Json) (k : String) : Option String := (j.getObjValAs? String k).toOption
  let expText := (str exp "text").getD ""
  if expText != got.text then
    return some s!"text {showPair (some expText) (some got.text)}"
  let expHtml := str exp "html"
  if expHtml != got.html then
    return some s!"html {showPair expHtml got.html}"
  let expAtts := ((exp.getObjValAs? (Array Lean.Json) "attachments").toOption).getD #[]
  if expAtts.size != got.attachments.size then
    return some s!"attachments expected {expAtts.size} got {got.attachments.size}"
  for h : i in [0 : expAtts.size] do
    let e := expAtts[i]
    let g := got.attachments[i]!
    let field (name : String) (expected got : Option String) : Option String :=
      if expected == got then none else some s!"attachment {i} {name} {showPair expected got}"
    let size := (e.getObjValAs? Nat "size").toOption
    let inline := (e.getObjValAs? Bool "inline").toOption
    let checks := [
      field "name" (str e "name") (some g.name),
      field "mime" (str e "mime") (some g.mime),
      field "size" (size.map toString) (some (toString g.size)),
      field "cid" (str e "cid") (some g.cid),
      field "inline" (inline.map toString) (some (toString g.inline)),
      field "method" (str e "method") g.method,
      field "data" (str e "data") g.data]
    if let some d := checks.findSome? id then return some d
  return none

def mimeVectors (t : IO.Ref Tally) : IO Unit := do
  -- run from `lean/` (as `lake exe` does) or from the repository root
  let path := if ← System.FilePath.pathExists "test/fixtures/mime-vectors.json" then "test/fixtures/mime-vectors.json"
    else "lean/test/fixtures/mime-vectors.json"
  let raw ← IO.FS.readFile path
  match Lean.Json.parse raw with
  | .error e => check t s!"mime-vectors.json parses ({e})" false
  | .ok j =>
    let cases := ((j.getObjValAs? (Array Lean.Json) "cases").toOption).getD #[]
    check t "mime-vectors.json has cases" (!cases.isEmpty)
    for c in cases do
      let name := ((c.getObjValAs? String "name").toOption).getD "?"
      let rawB64 := ((c.getObjValAs? String "raw").toOption).getD ""
      let exp := ((c.getObjVal? "parts").toOption).getD .null
      let got := Core.Mime.buildParts (Core.Mime.parse (Core.Base64.decode rawB64))
      match partsDiff exp got with
      | none => check t s!"mime vector {name} parses to postal-mime's parts" true
      | some d =>
        IO.println s!"FAIL {name}: {d}"
        t.modify fun x => { x with failed := x.failed + 1 }

/-! ## Typed header fields and the header block of an inlined message -/

/-- A check on an optional string, printing the first difference on failure. -/
def checkStr (t : IO.Ref Tally) (name : String) (expected got : Option String) : IO Unit := do
  if expected == got then check t name true
  else
    IO.println s!"FAIL {name}: {showPair expected got}"
    t.modify fun x => { x with failed := x.failed + 1 }

/-- Messages with an inline `message/rfc822` part, and the `text` / `html` postal-mime 3.0.0 (the
patched `text-format.js`) gives for them under `TZ=UTC`, `LANG=en_US.UTF-8` (node 25):
`(name, raw, text, html)`. -/
def rfc822Cases : List (String × String × Option String × Option String) := [
  ("basic",
   "From: outer@example.com\nTo: alex@wuli.nu\nSubject: Fwd\nMIME-Version: 1.0\nContent-Type: multipart/mixed; boundary=\"B\"\n\n--B\nContent-Type: text/plain; charset=utf-8\n\nSee below.\n\n--B\nContent-Type: message/rfc822\n\nFrom: Maya Lindqvist <maya@nordic-press.example>\nTo: alex@wuli.nu, Bo <bo@example.com>\nCc: cc1@example.com\nSubject: Proof pages\nDate: Sun, 13 Sep 2026 14:32:12 +0000\n\nThe original.\n\n--B--\n",
   some "See below.\n\n\n\n---------------------------------------------------\nFrom:    Maya Lindqvist <maya@nordic-press.example>\nSubject: Proof pages\nDate:    9/13/2026, 14:32:12\nTo:      alex@wuli.nu, Bo <bo@example.com>\nCc:      cc1@example.com\n---------------------------------------------------\n\nThe original.\n\n",
   none),
  ("group-and-encoded",
   "From: outer@example.com\nTo: alex@wuli.nu\nSubject: Fwd\nMIME-Version: 1.0\nContent-Type: multipart/mixed; boundary=\"B\"\n\n--B\nContent-Type: text/plain; charset=utf-8\n\nSee below.\n\n--B\nContent-Type: message/rfc822\n\nFrom: =?UTF-8?Q?J=C3=BCrgen_M=C3=BCller?= <jm@example.de>\nTo: Team: a@example.com, \"B, Person\" <b@example.com>;, c@example.com\nCc: undisclosed-recipients:;\nBcc: hidden@example.com\nSubject: =?UTF-8?B?w4RwZmVs?= & <pears>\nDate: Mon, 5 Jan 2026 03:04:05 -0800\n\nBody.\n\n--B--\n",
   some "See below.\n\n\n\n----------------------------------------------------------------------\nFrom:    Jürgen Müller <jm@example.de>\nSubject: Äpfel & <pears>\nDate:    1/5/2026, 11:04:05\nTo:      Team:a@example.com, B, Person <b@example.com>;, c@example.com\nCc:      undisclosed-recipients:;\nBcc:     hidden@example.com\n----------------------------------------------------------------------\n\nBody.\n\n",
   none),
  ("bad-date",
   "From: outer@example.com\nTo: alex@wuli.nu\nSubject: Fwd\nMIME-Version: 1.0\nContent-Type: multipart/mixed; boundary=\"B\"\n\n--B\nContent-Type: text/plain; charset=utf-8\n\nSee below.\n\n--B\nContent-Type: message/rfc822\n\nFrom: x@example.com\nTo: y@example.com\nSubject: When?\nDate: not a date at all\n\nBody.\n\n--B--\n",
   some "See below.\n\n\n\n--------------------------\nFrom:    x@example.com\nSubject: When?\nDate:    not a date at all\nTo:      y@example.com\n--------------------------\n\nBody.\n\n",
   none),
  ("midnight-html",
   "From: outer@example.com\nTo: alex@wuli.nu\nSubject: Fwd\nMIME-Version: 1.0\nContent-Type: multipart/mixed; boundary=\"B\"\n\n--B\nContent-Type: text/html; charset=utf-8\n\n<p>See below.</p>\n\n--B\nContent-Type: message/rfc822\n\nFrom: \"O'Brien & Co\" <ob@example.com>\nTo: Ann <ann@example.com>, ben@example.com\nSubject: Tea \"time\" ?\nDate: Fri, 01 Jan 2027 00:00:07 +0000\nContent-Type: text/html; charset=utf-8\n\n<p>Inner html.</p>\n\n--B--\n",
   none,
   some "<p>See below.</p>\n\n\n<div class=\"postal-email-header\"><div class=\"postal-email-header-row\"><div class=\"postal-email-header-key\">From</div><div class=\"postal-email-header-value\"><a href=\"mailto:ob@example.com\" class=\"postal-email-address\">O&#x27;Brien &#x26; Co</a></div></div>\n<div class=\"postal-email-header-row\"><div class=\"postal-email-header-key\">Subject</div><div class=\"postal-email-header-value postal-email-header-subject\">Tea &#x22;time&#x22; &#x3F;</div></div>\n<div class=\"postal-email-header-row\"><div class=\"postal-email-header-key\">Date</div><div class=\"postal-email-header-value postal-email-header-date\" data-date=\"2027-01-01T00:00:07.000Z\">1/1/2027, 00:00:07</div></div>\n<div class=\"postal-email-header-row\"><div class=\"postal-email-header-key\">To</div><div class=\"postal-email-header-value\"><a href=\"mailto:ann@example.com\" class=\"postal-email-address\">Ann</a> <span class=\"postal-email-address-separator\">, </span> <a href=\"mailto:ben@example.com\" class=\"postal-email-address\">&#x3C;ben@example.com&#x3E;</a></div></div></div>\n<p>Inner html.</p>\n\n"),
  ("long-to",
   "From: outer@example.com\nTo: alex@wuli.nu\nSubject: Fwd\nMIME-Version: 1.0\nContent-Type: multipart/mixed; boundary=\"B\"\n\n--B\nContent-Type: text/plain; charset=utf-8\n\nSee below.\n\n--B\nContent-Type: message/rfc822\n\nFrom: sender@example.com\nTo: Alpha Person <alpha.person@example.com>, Beta Person <beta.person@example.com>, Gamma Person <gamma.person@example.com>\nTo: delta@example.com\nDate: Tue, 9 Feb 2027 21:15:00 +0100\n\nBody.\n\n--B--\n",
   some "See below.\n\n\n\n-------------------------------------------------------------------------------------\nFrom: sender@example.com\nDate: 2/9/2027, 20:15:00\nTo:   Alpha Person <alpha.person@example.com>, Beta Person <beta.person@example.com>,\n      Gamma Person <gamma.person@example.com>, delta@example.com\n-------------------------------------------------------------------------------------\n\nBody.\n\n",
   none),
  ("empty-and-odd",
   "From: outer@example.com\nTo: alex@wuli.nu\nSubject: Fwd\nMIME-Version: 1.0\nContent-Type: multipart/mixed; boundary=\"B\"\n\n--B\nContent-Type: text/plain; charset=utf-8\n\nSee below.\n\n--B\nContent-Type: message/rfc822\n\nFrom: <>\nTo: Just A Name\nSubject:\nDate:\n\nBody.\n\n--B--\n",
   some "See below.\n\n\n\n--------------------\nFrom: \nTo:   Just A Name <>\n--------------------\n\nBody.\n\n",
   none),
  ("mixed-both",
   "From: outer@example.com\nTo: alex@wuli.nu\nSubject: Fwd\nMIME-Version: 1.0\nContent-Type: multipart/mixed; boundary=\"B\"\n\n--B\nContent-Type: text/html; charset=utf-8\n\n<p>See below.</p>\n\n--B\nContent-Type: message/rfc822\n\nFrom: Plain Sender <plain@example.com>\nTo: r@example.com\nSubject: Alt inner\nDate: 2026-03-04T05:06:07Z\nContent-Type: multipart/alternative; boundary=\"A\"\n\n--A\nContent-Type: text/plain\n\nplain inner\n--A\nContent-Type: text/html\n\n<b>html inner</b>\n--A--\n\n--B--\n",
   some "\nSee below.\n\n\n-----------------------------------------\nFrom:    Plain Sender <plain@example.com>\nSubject: Alt inner\nDate:    3/4/2026, 05:06:07\nTo:      r@example.com\n-----------------------------------------\n\nplain inner\n",
   some "<p>See below.</p>\n\n\n<div class=\"postal-email-header\"><div class=\"postal-email-header-row\"><div class=\"postal-email-header-key\">From</div><div class=\"postal-email-header-value\"><a href=\"mailto:plain@example.com\" class=\"postal-email-address\">Plain Sender</a></div></div>\n<div class=\"postal-email-header-row\"><div class=\"postal-email-header-key\">Subject</div><div class=\"postal-email-header-value postal-email-header-subject\">Alt inner</div></div>\n<div class=\"postal-email-header-row\"><div class=\"postal-email-header-key\">Date</div><div class=\"postal-email-header-value postal-email-header-date\" data-date=\"2026-03-04T05:06:07.000Z\">3/4/2026, 05:06:07</div></div>\n<div class=\"postal-email-header-row\"><div class=\"postal-email-header-key\">To</div><div class=\"postal-email-header-value\"><a href=\"mailto:r@example.com\" class=\"postal-email-address\">&#x3C;r@example.com&#x3E;</a></div></div></div>\n<b>html inner</b>\n"),
  ("group-html",
   "From: outer@example.com\nTo: alex@wuli.nu\nSubject: Fwd\nMIME-Version: 1.0\nContent-Type: multipart/mixed; boundary=\"B\"\n\n--B\nContent-Type: text/html; charset=utf-8\n\n<p>See below.</p>\n\n--B\nContent-Type: message/rfc822\n\nFrom: =?UTF-8?Q?J=C3=BCrgen?= <jm@example.de>\nTo: Team: a@example.com, \"B, P\" <b@example.com>;, c@example.com\nCc: undisclosed-recipients:;\nBcc: x@example.com\nSubject: =?UTF-8?B?w4RwZmVs?=\nDate: soon & <later>\nContent-Type: text/html; charset=utf-8\n\n<p>Grouped.</p>\n\n--B--\n",
   none,
   some "<p>See below.</p>\n\n\n<div class=\"postal-email-header\"><div class=\"postal-email-header-row\"><div class=\"postal-email-header-key\">From</div><div class=\"postal-email-header-value\"><a href=\"mailto:jm@example.de\" class=\"postal-email-address\">Jürgen</a></div></div>\n<div class=\"postal-email-header-row\"><div class=\"postal-email-header-key\">Subject</div><div class=\"postal-email-header-value postal-email-header-subject\">Äpfel</div></div>\n<div class=\"postal-email-header-row\"><div class=\"postal-email-header-key\">Date</div><div class=\"postal-email-header-value postal-email-header-date\" data-date=\"soon &#x26; &#x3C;later&#x3E;\">soon &#x26; &#x3C;later&#x3E;</div></div>\n<div class=\"postal-email-header-row\"><div class=\"postal-email-header-key\">To</div><div class=\"postal-email-header-value\"><span class=\"postal-email-address-group\">Team:</span> <a href=\"mailto:a@example.com\" class=\"postal-email-address\">&#x3C;a@example.com&#x3E;</a> <span class=\"postal-email-address-separator\">, </span> <a href=\"mailto:b@example.com\" class=\"postal-email-address\">B, P</a> <span class=\"postal-email-address-group\">;</span> <span class=\"postal-email-address-separator\">, </span> <a href=\"mailto:c@example.com\" class=\"postal-email-address\">&#x3C;c@example.com&#x3E;</a></div></div>\n<div class=\"postal-email-header-row\"><div class=\"postal-email-header-key\">Cc</div><div class=\"postal-email-header-value\"><span class=\"postal-email-address-group\">undisclosed-recipients:</span> <span class=\"postal-email-address-group\">;</span></div></div>\n<div class=\"postal-email-header-row\"><div class=\"postal-email-header-key\">Bcc</div><div class=\"postal-email-header-value\"><a href=\"mailto:x@example.com\" class=\"postal-email-address\">&#x3C;x@example.com&#x3E;</a></div></div></div>\n<p>Grouped.</p>\n\n")]

def rfc822Blocks (t : IO.Ref Tally) : IO Unit := do
  for (name, raw, text, html) in rfc822Cases do
    let m := Core.Mime.parse raw.toUTF8
    checkStr t s!"an inlined message ({name}) gets postal-mime's text header block" text m.text
    checkStr t s!"an inlined message ({name}) gets postal-mime's html header block" html m.html

def typedFields (t : IO.Ref Tally) : IO Unit := do
  let path := if ← System.FilePath.pathExists "test/fixtures/eml/01.eml" then "test/fixtures/eml/01.eml"
    else "lean/test/fixtures/eml/01.eml"
  let m := Core.Mime.parse (← IO.FS.readBinFile path)
  check t "a message's from is its first From address, name and address" <|
    m.from? == some (.mailbox "Maya Lindqvist" "maya@nordic-press.example")
  check t "a message's to lists the parsed To addresses" (m.to == [.mailbox "" "alex@wuli.nu"])
  check t "a message without Cc, Bcc or Reply-To has empty lists for them" <|
    m.cc.isEmpty && m.bcc.isEmpty && m.replyTo.isEmpty && m.sender?.isNone
  check t "a message's subject is its decoded Subject" (m.subject? == some "Proof pages for the autumn catalogue")
  check t "a message's messageId is its Message-ID" (m.messageId? == some "<01@example.test>")
  check t "a message's date is parsed to epoch milliseconds" (m.dateMs? == some 1789309932000)
  check t "a message's ISO date is postal-mime's message.date" (m.dateIso? == some "2026-09-13T14:32:12.000Z")
  check t "the old accessors still read the same fields" <|
    m.subject == m.subject? && m.messageId == m.messageId? && m.date == some "Sun, 13 Sep 2026 14:32:12 +0000"

def main (_ : List String) : IO UInt32 := do
  let t ← IO.mkRef ({} : Tally)
  check t "a header line splits into a trimmed name and value" <|
    Core.Http.splitHeaderLine "Content-Type:  text/plain" == some ("Content-Type", "text/plain")
  check t "a header value keeps colons after the first" <|
    Core.Http.splitHeaderLine "Date: Fri, 31 Oct 2026 10:00:00 GMT" == some ("Date", "Fri, 31 Oct 2026 10:00:00 GMT")
  check t "a response header is found regardless of case" <|
    ({ status := 200, headers := [("Content-Type", "x")], body := .empty } : Core.Http.Response).header? "content-type" == some "x"
  -- Base64
  check t "base64 encodes the RFC 4648 vectors" <|
    Core.Base64.encode "".toUTF8 == "" && Core.Base64.encode "f".toUTF8 == "Zg==" &&
    Core.Base64.encode "fo".toUTF8 == "Zm8=" && Core.Base64.encode "foobar".toUTF8 == "Zm9vYmFy"
  check t "base64 decodes the RFC 4648 vectors" <|
    Core.Base64.decode "Zg==" == "f".toUTF8 && Core.Base64.decode "Zm8=" == "fo".toUTF8 &&
    Core.Base64.decode "Zm9vYmFy" == "foobar".toUTF8
  check t "base64 decode skips line breaks and tolerates missing padding" <|
    Core.Base64.decode "Zm9v\r\nYmFy" == "foobar".toUTF8 && Core.Base64.decode "Zg" == "f".toUTF8
  check t "base64url round-trips all byte values" <|
    let bytes := ByteArray.mk ((Array.range 256).map (·.toUInt8))
    Core.Base64.decode (Core.Base64.encodeUrl bytes) == bytes && Core.Base64.decode (Core.Base64.encode bytes) == bytes
  -- ULID
  let zeros := ByteArray.mk (Array.replicate 10 0)
  let ones := ByteArray.mk (Array.replicate 10 255)
  check t "a ulid of time zero and zero randomness is all zeros" (Core.Ulid.encode 0 zeros == "00000000000000000000000000")
  check t "a ulid is 26 characters and valid" <|
    let u := Core.Ulid.encode 1730000000000 ones
    u.length == 26 && Core.Ulid.isValid u
  check t "a ulid carries its timestamp" (Core.Ulid.timestamp? (Core.Ulid.encode 1730000000000 ones) == some 1730000000000)
  check t "the largest 48-bit time encodes as 7ZZZZZZZZZ" ((Core.Ulid.encode (2^48 - 1) zeros).take 10 == "7ZZZZZZZZZ")
  check t "ulids order by time as strings" (Core.Ulid.encode 1 ones < Core.Ulid.encode 2 zeros)
  check t "a ulid with a leading 8 is rejected" (!Core.Ulid.isValid "8ZZZZZZZZZZZZZZZZZZZZZZZZZ")
  check t "confusable letters normalize to digits" (Core.Ulid.normalize "0lo1" == "0101")
  -- MIME
  mimeVectors t
  typedFields t
  rfc822Blocks t
  let r ← t.get
  IO.println s!"{r.passed} passed, {r.failed} failed"
  return if r.failed == 0 then 0 else 1
