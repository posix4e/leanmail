import Core.Mime.Address
import Core.Mime.Date

/-! Address and date tests. Expected values for the address vectors and the date vectors were
produced by postal-mime 3.0.0's `addressParser` and by `Date.parse` under `TZ=UTC` (node 25).
One line per check, exit 1 on any failure. -/

open Core.Mime

structure Tally where
  passed : Nat := 0
  failed : Nat := 0

def check (t : IO.Ref Tally) (name : String) (ok : Bool) (detail : String := "") : IO Unit := do
  if ok then
    IO.println s!"ok   {name}"
    t.modify fun x => { x with passed := x.passed + 1 }
  else
    IO.println s!"FAIL {name}{if detail.isEmpty then "" else s!" (got {detail})"}"
    t.modify fun x => { x with failed := x.failed + 1 }

def mb (n a : String) : Addr := .mailbox n a

/-- Check one address vector against postal-mime's output. -/
def checkAddr (t : IO.Ref Tally) (name input : String) (expected : List Addr) : IO Unit := do
  let got := Address.parse input
  check t name (got == expected) (reprStr got)

def checkDate (t : IO.Ref Tally) (name input : String) (expected : Option Nat) : IO Unit := do
  let got := Date.parse? input
  check t name (got == expected) (reprStr got)

/-- The unfolded value of every header named `name` (case-insensitive) in a raw message. -/
def headerValues (raw : String) (name : String) : List String := Id.run do
  let lines := Core.Text.Str.lines raw
  let header := lines.takeWhile (!·.isEmpty)
  -- unfold: a line starting with SP or HTAB continues the previous one
  let mut unfolded : Array String := #[]
  for l in header do
    if (l.startsWith " " || l.startsWith "\t") && !unfolded.isEmpty then
      unfolded := unfolded.set! (unfolded.size - 1) (unfolded.back! ++ " " ++ Core.Text.Str.trim l)
    else unfolded := unfolded.push l
  let mut out := []
  for l in unfolded do
    match l.splitOn ":" with
    | n :: rest =>
      if (Core.Text.Str.trim n).toLower == name.toLower then
        out := out ++ [Core.Text.Str.trim (":".intercalate rest)]
    | [] => pure ()
  return out

def main (_ : List String) : IO UInt32 := do
  let t ← IO.mkRef ({} : Tally)
  -- Addresses (expected = postal-mime addressParser)
  checkAddr t "a plain address has no name" "alex@wuli.nu" [mb "" "alex@wuli.nu"]
  checkAddr t "a name before an angle address is the display name"
    "Maya Lindqvist <maya@nordic-press.example>" [mb "Maya Lindqvist" "maya@nordic-press.example"]
  checkAddr t "a comma inside a quoted name does not split the address"
    "\"Doe, Jane\" <jane@x.com>" [mb "Doe, Jane" "jane@x.com"]
  checkAddr t "a group keeps its name and members"
    "Friends: a@x.com, b@y.com;" [.group "Friends" [mb "" "a@x.com", mb "" "b@y.com"]]
  check t "flattening a group yields its members" <|
    Address.flatten (Address.parse "Friends: a@x.com, b@y.com;") == [mb "" "a@x.com", mb "" "b@y.com"]
  checkAddr t "a comment after a bare address becomes the name" "a@x.com (Alex)" [mb "Alex" "a@x.com"]
  checkAddr t "an encoded-word name is decoded" "=?UTF-8?B?Wm/Dqw==?= <zoe@x.com>" [mb "Zoë" "zoe@x.com"]
  checkAddr t "semicolons separate addresses like commas"
    "a@x.com; b@y.com; \"C\" <c@z.com>" [mb "" "a@x.com", mb "" "b@y.com", mb "C" "c@z.com"]
  checkAddr t "a bare encoded word with no address is a name only" "=?UTF-8?Q?Alex_Rose?=" [mb "Alex Rose" ""]
  checkAddr t "a bare encoded address is never made a recipient"
    "=?utf-8?B?dGVzdEBldmlsLmNv?=" [mb "test@evil.co" ""]
  checkAddr t "an encoded word holding an angle address is re-parsed"
    "\"=?utf-8?Q?Name_<a@x.com>?=\"" [mb "Name" "a@x.com"]
  checkAddr t "text with two at-signs yields the address token and the rest as name"
    "Alex a@b@c.com" [mb "Alex" "a@b@c.com"]
  checkAddr t "a lone address with two at-signs is kept whole" "foo@bar@baz.com" [mb "" "foo@bar@baz.com"]
  checkAddr t "Apple Mail truncation drops everything before an unexpected <"
    "Joe <Smith <joe@x.com>" [mb "Joe" "joe@x.com"]
  checkAddr t "a quoted local part glued to a domain stays one address"
    "\"user@domain\"@example.com" [mb "" "user@domain@example.com"]
  checkAddr t "a nested group is flattened into its parent"
    "G: H: a@x.com; b@y.com;" [.group "G" [mb "" "a@x.com"], mb "" "b@y.com"]
  checkAddr t "a quoted name glued to more text joins into one word" "\"Jane\"Doe <j@x.com>" [mb "JaneDoe" "j@x.com"]
  checkAddr t "escaped quotes inside a quoted name are kept"
    "\"Jane \\\"JJ\\\" Doe\" <j@x.com>" [mb "Jane \"JJ\" Doe" "j@x.com"]
  checkAddr t "a word with no at-sign is a name with no address" "Alex" [mb "Alex" ""]
  checkAddr t "an angle address alone has no name" "<a@x.com>" [mb "" "a@x.com"]
  checkAddr t "an empty field has no addresses" "" []
  checkAddr t "an empty group is kept with no members" "Undisclosed recipients:;" [.group "Undisclosed recipients" []]
  check t "flattening drops an empty group" (Address.parseFlat "Undisclosed recipients:;" == [])
  checkAddr t "non-breaking spaces around a name are trimmed" "  Alex  <a@x.com>" [mb "Alex" "a@x.com"]
  checkAddr t "an empty entry between commas is skipped" "a@x.com, , b@y.com" [mb "" "a@x.com", mb "" "b@y.com"]
  checkAddr t "a folded line break inside a field is whitespace" "Name\r\n <a@x.com>" [mb "Name" "a@x.com"]
  checkAddr t "an empty quoted name gives no name" "\"\" <a@x.com>" [mb "" "a@x.com"]
  checkAddr t "only the first angle address is the address" "a <b> c <d@e.com>" [mb "a c d@e.com" "b"]
  checkAddr t "an address-looking name before an angle address stays the name"
    "x@y.com <z@w.com>" [mb "x@y.com" "z@w.com"]
  -- Formatting (text-format.js)
  check t "a mailbox formats as name <address>" ((mb "Maya" "m@x.com").format == "Maya <m@x.com>")
  check t "a mailbox with no name formats as the bare address" ((mb "" "m@x.com").format == "m@x.com")
  check t "a group formats as name:members;" <|
    Address.formatList (Address.parse "Friends: a@x.com, Bo <b@y.com>; c@z.com") == "Friends:a@x.com, Bo <b@y.com>;, c@z.com"
  check t "name and address accessors read a mailbox and a group" <|
    (mb "N" "a@x").name == "N" && (mb "N" "a@x").address? == some "a@x" &&
    (Addr.group "G" []).name == "G" && (Addr.group "G" []).address? == none
  -- Dates (expected = Date.parse under TZ=UTC)
  checkDate t "an RFC 5322 date with +0000 parses" "Tue, 13 Sep 2026 14:32:12 +0000" (some 1789309932000)
  checkDate t "a date without day name or seconds parses as UTC" "13 Sep 2026 14:32" (some 1789309920000)
  checkDate t "an ISO 8601 Z date parses" "2026-09-13T14:32:12Z" (some 1789309932000)
  checkDate t "an ISO 8601 date with milliseconds and offset parses"
    "2026-09-13T14:32:12.123+02:00" (some 1789302732123)
  checkDate t "an ISO date alone is midnight UTC" "2026-09-13" (some 1789257600000)
  checkDate t "a trailing zone comment is ignored" "Sun, 13 Sep 2026 14:32:12 -0700 (PDT)" (some 1789335132000)
  checkDate t "extra whitespace and GMT are tolerated" "  Sun,  13   Sep  2026  14:32:12   GMT " (some 1789309932000)
  checkDate t "a two-digit year below 50 is 20xx and EST is -0500" "13 Sep 26 14:32:12 EST" (some 1789327932000)
  checkDate t "a two-digit year of 50 or more is 19xx" "1 Jan 99 00:00:00 UT" (some 915148800000)
  checkDate t "a one-digit day and a half-hour offset parse" "Mon, 1 Mar 2027 01:02:03 +0530" (some 1803843123000)
  checkDate t "the JavaScript toString form parses"
    "Sun Sep 13 2026 14:32:12 GMT+0200 (Central European Summer Time)" (some 1789302732000)
  checkDate t "month-first with a comma and PM parses" "Sep 13, 2026 2:32 PM" (some 1789309920000)
  checkDate t "asctime order parses" "Sun Sep 13 14:32:12 2026" (some 1789309932000)
  checkDate t "a military zone letter reads as +0000" "29 Feb 2028 12:00:00 Z" (some 1835438400000)
  checkDate t "PDT is -0700" "13 Sep 2026 14:32:12 PDT" (some 1789335132000)
  checkDate t "CDT is -0500" "13 Sep 2026 14:32:12 CDT" (some 1789327932000)
  checkDate t "Sept is a month name" "13 Sept 2026 10:00 +0100" (some 1789290000000)
  checkDate t "a sentence is not a date" "not a date" none
  -- JavaScript rolls 31 Feb over into March; we reject the impossible day instead.
  checkDate t "an impossible day of month is rejected" "31 Feb 2026 10:00:00 +0000" none
  check t "the epoch formats as RFC 5322 and ISO" <|
    Date.formatRfc5322 0 == "Thu, 01 Jan 1970 00:00:00 +0000" && Date.formatIso 0 == "1970-01-01T00:00:00.000Z"
  check t "a date formats as RFC 5322 like the fixtures" (Date.formatRfc5322 1789309932000 == "Sun, 13 Sep 2026 14:32:12 +0000")
  check t "a date formats as ISO like toISOString" <|
    Date.formatIso 1789223532000 == "2026-09-12T14:32:12.000Z" && Date.formatIso 4102444799999 == "2099-12-31T23:59:59.999Z" &&
    Date.formatIso 951782400000 == "2000-02-29T00:00:00.000Z"
  check t "dates round-trip through both formats" <|
    [0, 951782400000, 1789309932000, 1803843123000, 4102444799000].all fun ms =>
      Date.parse? (Date.formatRfc5322 ms) == some ms && Date.parse? (Date.formatIso ms) == some ms
  check t "milliseconds round-trip through ISO" (Date.parse? (Date.formatIso 1789302732123) == some 1789302732123)
  check t "days-from-civil inverts civil-from-days across four centuries" <|
    (List.range 1461).all fun k =>
      let z : Int := (k : Int) * 101 - 73000
      let (y, m, d) := Date.civilFromDays z
      Date.daysFromCivil y m d == z
  -- Fixtures: every From/To/Cc and Date header of test/fixtures/eml
  let dir : System.FilePath := "test/fixtures/eml"
  let entries ← dir.readDir
  let files := (entries.map (·.path)).filter (·.extension == some "eml") |>.qsort (·.toString < ·.toString)
  for f in files do
    let raw ← IO.FS.readFile f
    let fname := f.fileName.getD f.toString
    for h in ["From", "To", "Cc"] do
      for v in headerValues raw h do
        let as := Address.parseFlat v
        check t s!"{fname} {h} has at least one address with an at-sign"
          (!as.isEmpty && as.all fun a => (a.address?.getD "").toList.contains '@') (reprStr as)
    let dates := headerValues raw "Date"
    check t s!"{fname} has a Date header that parses" (!dates.isEmpty && dates.all (Date.parse? · |>.isSome)) (reprStr dates)
  let r ← t.get
  IO.println s!"{r.passed} passed, {r.failed} failed"
  return if r.failed == 0 then 0 else 1
