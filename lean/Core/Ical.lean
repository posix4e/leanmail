import Core.RRule
import Core.Base64
import Core.Text.Charset
import Core.Text.Str

/-! # iCalendar (RFC 5545 / iTIP RFC 5546): port of MailVault `shared/ical.js`

Produces the record MailVault seals into `events.env`, field for field (`Event`), and expands it
(`expandOccurrences`, `lastOccurrenceEnd`, from `shared/rrule.js`). Tolerant of what real
producers emit (Google, Outlook/Exchange, Apple, Zimbra): folded lines, quoted parameter values,
Windows time-zone ids, floating times, missing UIDs, DTEND before DTSTART in file order. Only
VEVENT (and VTIMEZONE, for the fixed-offset fallback) are kept; VTODO, VJOURNAL and VFREEBUSY are
ignored and VALARM stays only in `ics`. Never fails: problems land in `Calendar.warnings`.

Text lengths (`SHORT_TEXT_MAX`, `DESCRIPTION_MAX`, the clamps) are counted in UTF-16 code units
like the JavaScript, so the same input clamps at the same place. -/

namespace Core.Ical

open Core.Text Core.RRule

def recordV : Nat := 1
def calendarPartMax : Nat := 1 <<< 20
def icsMax : Nat := 32 * 1024
def descriptionMax : Nat := 16 * 1024
def shortTextMax : Nat := 1024
def maxAttendees : Nat := 500
def maxCategories : Nat := 50
def maxDateList : Nat := 1000

/-! ## SHA-256 (pure; Core links no libcrypto) -/

private def sha256K : Array UInt32 := #[
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
  0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
  0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
  0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
  0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2]

private def rotr (x : UInt32) (n : UInt32) : UInt32 := (x >>> n) ||| (x <<< (32 - n))

/-- SHA-256 of a byte string. -/
def sha256 (msg : ByteArray) : ByteArray := Id.run do
  let ml := msg.size
  let mut b := msg.push 0x80
  while b.size % 64 != 56 do b := b.push 0
  let bits := ml * 8
  for i in [0:8] do b := b.push ((bits >>> (8 * (7 - i))) % 256).toUInt8
  let mut h : Array UInt32 := #[0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19]
  for blk in [0:b.size / 64] do
    let mut w : Array UInt32 := Array.replicate 64 0
    for t in [0:16] do
      let o := blk * 64 + t * 4
      w := w.set! t ((b[o]!.toUInt32 <<< 24) ||| (b[o+1]!.toUInt32 <<< 16) ||| (b[o+2]!.toUInt32 <<< 8) ||| b[o+3]!.toUInt32)
    for t in [16:64] do
      let s0 := rotr w[t-15]! 7 ^^^ rotr w[t-15]! 18 ^^^ (w[t-15]! >>> 3)
      let s1 := rotr w[t-2]! 17 ^^^ rotr w[t-2]! 19 ^^^ (w[t-2]! >>> 10)
      w := w.set! t (w[t-16]! + s0 + w[t-7]! + s1)
    let mut a := h[0]!
    let mut bb := h[1]!
    let mut c := h[2]!
    let mut d := h[3]!
    let mut e := h[4]!
    let mut f := h[5]!
    let mut g := h[6]!
    let mut hh := h[7]!
    for t in [0:64] do
      let S1 := rotr e 6 ^^^ rotr e 11 ^^^ rotr e 25
      let ch := (e &&& f) ^^^ ((~~~e) &&& g)
      let t1 := hh + S1 + ch + sha256K[t]! + w[t]!
      let S0 := rotr a 2 ^^^ rotr a 13 ^^^ rotr a 22
      let maj := (a &&& bb) ^^^ (a &&& c) ^^^ (bb &&& c)
      let t2 := S0 + maj
      hh := g; g := f; f := e; e := d + t1; d := c; c := bb; bb := a; a := t1 + t2
    h := #[h[0]! + a, h[1]! + bb, h[2]! + c, h[3]! + d, h[4]! + e, h[5]! + f, h[6]! + g, h[7]! + hh]
  let mut out := ByteArray.empty
  for x in h do
    out := out.push (x >>> 24).toUInt8 |>.push (x >>> 16).toUInt8 |>.push (x >>> 8).toUInt8 |>.push x.toUInt8
  return out

def hex (b : ByteArray) : String := Id.run do
  let digits := "0123456789abcdef".toList.toArray
  let mut s := ""
  for x in b.toList do
    s := s.push digits[x.toNat / 16]! |>.push digits[x.toNat % 16]!
  return s

/-! ## Record -/

structure RecurrenceId extends DateTime where
  /-- 'THISANDFUTURE' or none -/
  range : Option String := none
  deriving Repr, BEq, Inhabited

structure Attendee where
  email : String
  name : String
  partstat : String
  role : String
  rsvp : Bool
  cutype : String
  deriving Repr, BEq, Inhabited

structure Organizer where
  email : String
  name : String
  deriving Repr, BEq, Inhabited

/-- `VEventRecord` of `shared/ical.js`. -/
structure Event where
  v : Nat := recordV
  uid : String
  recurrenceId : Option RecurrenceId := none
  sequence : Nat := 0
  /-- DTSTAMP, epoch seconds (0 when absent) -/
  stamp : Int := 0
  method : Option String := none
  status : Option String := none
  summary : String := ""
  location : String := ""
  description : String := ""
  url : Option String := none
  conference : Option String := none
  start : DateTime
  end_ : DateTime
  durationMs : Int := 0
  rrule : Option RRule := none
  rruleRaw : Option String := none
  rdates : List DateTime := []
  exdates : List DateTime := []
  organizer : Option Organizer := none
  attendees : List Attendee := []
  categories : List String := []
  /-- the original VEVENT block, CRLF-joined, at most 32 KiB of UTF-8 -/
  ics : String := ""
  deriving Repr, BEq, Inhabited

structure Calendar where
  method : Option String
  calname : Option String
  tzid : Option String
  events : List Event
  vtimezones : VTimezones
  warnings : List String
  deriving Repr, Inhabited

/-! ## JSON (the JavaScript `JSON.stringify` of the record: same keys, same order) -/

def jsonString (s : String) : String := Id.run do
  let digits := "0123456789abcdef".toList.toArray
  let mut out := "\""
  for c in s.toList do
    match c with
    | '"' => out := out ++ "\\\""
    | '\\' => out := out ++ "\\\\"
    | '\n' => out := out ++ "\\n"
    | '\r' => out := out ++ "\\r"
    | '\t' => out := out ++ "\\t"
    | c =>
      if c.toNat == 8 then out := out ++ "\\b"
      else if c.toNat == 12 then out := out ++ "\\f"
      else if c.toNat < 0x20 then
        out := out ++ "\\u00" |>.push digits[c.toNat / 16]! |>.push digits[c.toNat % 16]!
      else out := out.push c
  return out.push '"'

private def jOpt (f : α → String) : Option α → String
  | some x => f x
  | none => "null"
private def jStr (s : Option String) : String := jOpt jsonString s
private def jBool (b : Bool) : String := if b then "true" else "false"
private def jArr (f : α → String) (l : List α) : String := "[" ++ ",".intercalate (l.map f) ++ "]"
private def jObj (kvs : List (String × String)) : String :=
  "{" ++ ",".intercalate (kvs.map fun (k, v) => jsonString k ++ ":" ++ v) ++ "}"

private def dateTimeFields (d : DateTime) : List (String × String) :=
  [("utc", jOpt toString d.utc), ("local", jsonString d.loc), ("date", jStr d.date), ("tzid", jStr d.tzid),
   ("tzidRaw", jStr d.tzidRaw), ("allDay", jBool d.allDay), ("floating", jBool d.floating)]

def _root_.Core.RRule.DateTime.toJson (d : DateTime) : String := jObj (dateTimeFields d)

def _root_.Core.RRule.RRule.toJson (r : RRule) : String :=
  let ints (l : List Int) := jArr toString l
  jObj ([("freq", jsonString r.freq.toString), ("interval", toString r.interval), ("wkst", jsonString r.wkst),
    ("unsupported", jArr jsonString r.unsupported)] ++
    (r.count.map fun c => [("count", toString c)]).getD [] ++
    (r.until_.map fun u => [("until", jObj [("utc", jOpt toString u.utc), ("date", jStr u.date)])]).getD [] ++
    (r.byday.map fun l => [("byday", jArr (fun b => jObj [("ord", toString b.ord), ("day", jsonString b.day)]) l)]).getD [] ++
    (r.bymonthday.map fun l => [("bymonthday", ints l)]).getD [] ++
    (r.bymonth.map fun l => [("bymonth", ints l)]).getD [] ++
    (r.bysetpos.map fun l => [("bysetpos", ints l)]).getD [])

/-- The record as JSON; `withIcs := false` leaves out the source block (as the vectors do). -/
def Event.toJson (e : Event) (withIcs : Bool := true) : String :=
  jObj ([("v", toString e.v), ("uid", jsonString e.uid),
    ("recurrenceId", jOpt (fun (r : RecurrenceId) => jObj (dateTimeFields r.toDateTime ++ [("range", jStr r.range)])) e.recurrenceId),
    ("sequence", toString e.sequence), ("stamp", toString e.stamp), ("method", jStr e.method),
    ("status", jStr e.status), ("summary", jsonString e.summary), ("location", jsonString e.location),
    ("description", jsonString e.description), ("url", jStr e.url), ("conference", jStr e.conference),
    ("start", e.start.toJson), ("end", e.end_.toJson), ("durationMs", toString e.durationMs),
    ("rrule", jOpt RRule.toJson e.rrule), ("rruleRaw", jStr e.rruleRaw),
    ("rdates", jArr DateTime.toJson e.rdates), ("exdates", jArr DateTime.toJson e.exdates),
    ("organizer", jOpt (fun (o : Organizer) => jObj [("email", jsonString o.email), ("name", jsonString o.name)]) e.organizer),
    ("attendees", jArr (fun (a : Attendee) => jObj [("email", jsonString a.email), ("name", jsonString a.name),
      ("partstat", jsonString a.partstat), ("role", jsonString a.role), ("rsvp", jBool a.rsvp), ("cutype", jsonString a.cutype)]) e.attendees),
    ("categories", jArr jsonString e.categories)] ++
    (if withIcs then [("ics", jsonString e.ics)] else []))

/-! ## Line level -/

/-- Normalise line endings and unfold continuation lines (RFC 5545 3.1). -/
def unfold (text : String) : String := Id.run do
  let cs := match text.toList with | '﻿' :: r => r | r => r
  -- CRLF / CR → LF
  let mut norm : Array Char := #[]
  let mut i := 0
  let a := cs.toArray
  while h : i < a.size do
    let c := a[i]
    if c == '\r' then
      norm := norm.push '\n'
      if i + 1 < a.size && a[i+1]! == '\n' then i := i + 2 else i := i + 1
    else
      norm := norm.push c
      i := i + 1
  -- drop LF + (SP | HTAB)
  let mut out : Array Char := #[]
  let mut j := 0
  while h : j < norm.size do
    let c := norm[j]
    if c == '\n' && j + 1 < norm.size && (norm[j+1]! == ' ' || norm[j+1]! == '\t') then j := j + 2
    else
      out := out.push c
      j := j + 1
  return String.ofList out.toList

abbrev Params := List (String × List String)

def Params.get? (ps : Params) (name : String) : Option String :=
  match ps.find? (·.1 == name) with
  | some (_, v :: _) => some v
  | _ => none

structure Property where
  name : String
  params : Params
  value : String
  deriving Repr, Inhabited

/-- Split a parameter value list on unquoted commas; quoted values lose their quotes. -/
private def splitParamValues (raw : String) : List String := Id.run do
  let mut out : Array String := #[]
  let mut cur := ""
  let mut inQ := false
  for c in raw.toList do
    if c == '"' then
      inQ := !inQ
      continue
    if c == ',' && !inQ then
      out := out.push (Str.trimJs cur)
      cur := ""
      continue
    cur := cur.push c
  out := out.push (Str.trimJs cur)
  return out.toList

/-- One unfolded content line: NAME;PARAM=VAL;PARAM="quoted:val":VALUE. -/
def parseContentLine (line : String) : Option Property := Id.run do
  let a := line.toList.toArray
  let mut inQ := false
  let mut colon : Option Nat := none
  let mut semis : Array Nat := #[]
  for h : i in [0:a.size] do
    let c := a[i]
    if c == '"' then inQ := !inQ
    else if !inQ then
      if c == ':' then
        colon := some i
        break
      if c == ';' then semis := semis.push i
  let some ci := colon | return none
  let value := Str.slice a (ci + 1) a.size
  let mut segments : Array String := #[]
  let mut prev := 0
  for s in semis do
    segments := segments.push (Str.slice a prev s)
    prev := s + 1
  segments := segments.push (Str.slice a prev ci)
  let name := (Str.trimJs (segments[0]?.getD "")).toUpper
  if name.isEmpty || !name.all (fun c => ('A' ≤ c && c ≤ 'Z') || Str.isDigit c || c == '-') then return none
  let mut params : Params := []
  for seg in segments.toList.drop 1 do
    let sc := seg.toList
    let eq := sc.findIdx (· == '=')
    if eq == 0 || eq ≥ sc.length then continue
    let pname := (Str.trimJs (String.ofList (sc.take eq))).toUpper
    let values := splitParamValues (String.ofList (sc.drop (eq + 1)))
    if params.any (·.1 == pname) then
      params := params.map fun (k, v) => if k == pname then (k, v ++ values) else (k, v)
    else params := params ++ [(pname, values)]
  return some { name, params, value }

inductive Component where
  | mk (name : String) (props : Array Property) (children : Array Component) (start end_ : Nat)
  deriving Inhabited

namespace Component
def name : Component → String | mk n .. => n
def props : Component → Array Property | mk _ p .. => p
def children : Component → Array Component | mk _ _ c .. => c
def start : Component → Nat | mk _ _ _ s _ => s
def end_ : Component → Nat | mk _ _ _ _ e => e
def addProp (p : Property) : Component → Component | mk n ps cs s e => mk n (ps.push p) cs s e
def addChild : Component → Component → Component | mk n ps cs s e, c => mk n ps (cs.push c) s e
def setEnd (e : Nat) : Component → Component | mk n ps cs s _ => mk n ps cs s e
def prop? (c : Component) (name : String) : Option Property := c.props.find? (·.name == name)
end Component

abbrev W := StateM (Array String)

def warn (w : String) : W Unit := modify (·.push w)

/-- Build the component tree via a BEGIN/END stack. Unterminated components are kept (they end at
end of input); a stray END is ignored. -/
def parseComponents (text : String) : W (Component × Array String) := do
  let lines := ((unfold text).splitOn "\n").toArray
  let last := lines.size - 1
  let mut stack : Array Component := #[.mk "ROOT" #[] #[] 0 last]
  for h : idx in [0:lines.size] do
    let line := lines[idx]
    if (Str.trimJs line).isEmpty then continue
    let some cl := parseContentLine line
      | do
        if (← get).size < 50 then warn s!"bad-line:{idx + 1}"
        continue
    if cl.name == "BEGIN" then
      stack := stack.push (.mk (Str.trimJs cl.value).toUpper #[] #[] idx last)
      continue
    if cl.name == "END" then
      let n := (Str.trimJs cl.value).toUpper
      let mut at_ : Option Nat := none
      let mut i := stack.size - 1
      while i > 0 do
        if stack[i]!.name == n then
          at_ := some i
          break
        i := i - 1
      let some atI := at_
        | do warn ("unbalanced-end:" ++ n); continue
      while stack.size > atI do
        let c := stack.back!.setEnd idx
        stack := stack.pop
        stack := stack.modify (stack.size - 1) (·.addChild c)
        if stack.size > atI then warn ("unterminated:" ++ c.name)
      continue
    stack := stack.modify (stack.size - 1) (·.addProp cl)
  while stack.size > 1 do
    let c := stack.back!
    stack := stack.pop
    stack := stack.modify (stack.size - 1) (·.addChild c)
    warn ("unterminated:" ++ c.name)
  return (stack[0]!, lines)

/-! ## Value types -/

/-- RFC 5545 3.3.11 TEXT unescaping. -/
def unescapeText (s : String) : String := Id.run do
  let a := s.toList.toArray
  let mut out := ""
  let mut i := 0
  while h : i < a.size do
    let c := a[i]
    if c == '\\' && i + 1 < a.size then
      let d := a[i+1]!
      if d == 'n' || d == 'N' then
        out := out.push '\n'; i := i + 2; continue
      if d == ',' || d == ';' || d == '\\' then
        out := out.push d; i := i + 2; continue
    out := out.push c
    i := i + 1
  return out

/-- Split a TEXT list on unescaped separators (the pieces are still escaped). -/
private def splitUnescaped (s : String) (sep : Char) : List String := Id.run do
  let a := s.toList.toArray
  let mut out : Array String := #[]
  let mut cur := ""
  let mut i := 0
  while h : i < a.size do
    let c := a[i]
    if c == '\\' && i + 1 < a.size then
      cur := cur.push c |>.push a[i+1]!
      i := i + 2
      continue
    if c == sep then
      out := out.push cur
      cur := ""
    else cur := cur.push c
    i := i + 1
  out := out.push cur
  return out.toList

/-- Length in UTF-16 code units (JavaScript's `String.length`). -/
def utf16Length (s : String) : Nat := s.toList.foldl (fun n c => n + if c.toNat > 0xFFFF then 2 else 1) 0

/-- At most `max` UTF-16 units, never splitting a surrogate pair. -/
def clampText (s : String) (max : Nat) : String := Id.run do
  if utf16Length s ≤ max then return s
  let mut n := 0
  let mut out := ""
  for c in s.toList do
    let w := if c.toNat > 0xFFFF then 2 else 1
    if n + w > max then break
    n := n + w
    out := out.push c
  return out

/-- At most `max` UTF-8 bytes on a character boundary. -/
def capUtf8 (s : String) (max : Nat) : String := Id.run do
  if s.utf8ByteSize ≤ max then return s
  let mut n := 0
  let mut out := ""
  for c in s.toList do
    if n + c.utf8Size > max then break
    n := n + c.utf8Size
    out := out.push c
  return out

/-- Parse a DATE or DATE-TIME property value: VALUE=DATE or 8 digits → all-day; trailing 'Z' →
UTC; TZID → `resolveTzid` (IANA, Windows names, VTIMEZONE fixed offset); otherwise floating,
anchored to `fallbackTz` when given, else the wall clock read as UTC with `floating` set. -/
def parseDateTime (value : String) (params : Params) (fallbackTz : Option String := none)
    (vtimezones : Option VTimezones := none) : W (Option DateTime) := do
  let v := Str.trimJs value
  let some (ys, ms, ds, time, z) := matchDateTime? v
    | do warn ("bad-datetime:" ++ String.ofList (v.toList.take 40)); return none
  let y := ys.toNat!
  let mo := ms.toNat!
  let d := ds.toNat!
  if mo < 1 || mo > 12 || d < 1 || d > 31 then
    warn ("bad-datetime:" ++ v); return none
  let date := pad4 y ++ "-" ++ pad2 mo ++ "-" ++ pad2 d
  let valueParam := ((params.get? "VALUE").getD "").toUpper
  let some (hs, mis, ss) := time
    | return some { loc := date ++ "T00:00:00", date := some date, allDay := true }
  if valueParam == "DATE" then return some { loc := date ++ "T00:00:00", date := some date, allDay := true }
  let h := hs.toNat!
  let mi := mis.toNat!
  let s := (ss.map String.toNat!).getD 0
  if h > 23 || mi > 59 || s > 60 then
    warn ("bad-datetime:" ++ v); return none
  let s := if s == 60 then 59 else s
  let loc := date ++ "T" ++ pad2 h ++ ":" ++ pad2 mi ++ ":" ++ pad2 s
  let naive := zonedToUtc loc none
  if z then return some { utc := some naive, loc, tzid := some "UTC" }
  let tzidRaw := params.get? "TZID"
  if let some raw := tzidRaw then
    let r := resolveTzid (some raw) vtimezones
    if let some tz := r.tzid then return some { utc := some (zonedToUtc loc (some tz)), loc, tzid := some tz, tzidRaw }
    if let some off := r.fixedOffsetMinutes then
      return some { utc := some (naive - off * minuteMs), loc, tzidRaw }
    warn ("unknown-tzid:" ++ raw)
  if let some fb := canonicalTimeZone fallbackTz then
    return some { utc := some (zonedToUtc loc (some fb)), loc, tzid := some fb, tzidRaw }
  return some { utc := some naive, loc, tzidRaw, floating := true }

structure Duration where
  weeks : Nat := 0
  days : Nat := 0
  hours : Nat := 0
  minutes : Nat := 0
  seconds : Nat := 0
  sign : Int := 1
  deriving Repr, BEq

/-- RFC 5545 3.3.6 DURATION ('P1DT2H', '-PT15M', 'P2W'). -/
def parseDuration (str : String) : Option Duration := do
  let cs := (Str.trimJs str).toList
  let (sign, cs) : Int × List Char := match cs with | '-' :: r => (-1, r) | '+' :: r => (1, r) | r => (1, r)
  let cs ← match cs with | c :: r => if c == 'P' || c == 'p' then some r else none | [] => none
  -- `<digits><letter>` at the head, if the letter matches
  let part (l : List Char) (letter : Char) : Option Nat × List Char :=
    let ds := l.takeWhile Str.isDigit
    match l.drop ds.length with
    | c :: r => if !ds.isEmpty && c.toUpper == letter then (some (String.ofList ds).toNat!, r) else (none, l)
    | [] => (none, l)
  let (w, cs) := part cs 'W'
  let (d, cs) := part cs 'D'
  let (h, mi, s, cs) ← match cs with
    | t :: r =>
      if t == 'T' || t == 't' then
        let (h, r) := part r 'H'
        let (mi, r) := part r 'M'
        let (s, r) := part r 'S'
        some (h, mi, s, r)
      else none
    | [] => some (none, none, none, [])
  if !cs.isEmpty then none
  if w.isNone && d.isNone && h.isNone && mi.isNone && s.isNone then none
  pure { weeks := w.getD 0, days := d.getD 0, hours := h.getD 0, minutes := mi.getD 0, seconds := s.getD 0, sign }

def Duration.toMs (d : Duration) : Int :=
  d.sign * ((((d.weeks * 7 + d.days) * 86400 + d.hours * 3600 + d.minutes * 60 + d.seconds) * 1000 : Nat) : Int)

/-! ## VEVENT -/

private def param (p : Property) (name : String) : Option String := p.params.get? name

private def stripMailto (s : String) : String :=
  if s.length ≥ 7 && (String.ofList (s.toList.take 7)).toLower == "mailto:" then String.ofList (s.toList.drop 7) else s

private def calAddress (p : Property) : Organizer :=
  let email := (Str.trimJs (stripMailto (Str.trimJs p.value))).toLower
  let name := match param p "CN" with
    | some cn => if cn.isEmpty then "" else Str.trimJs (unescapeText cn)
    | none => ""
  { email := clampText email 254, name := clampText name 200 }

private def addDays (dt : DateTime) (n : Int) : DateTime :=
  let date := isoDate (dayOfIso (dt.date.getD dt.loc) + n)
  { dt with utc := none, loc := date ++ "T00:00:00", date := some date, allDay := true, tzid := none, tzidRaw := none, floating := false }

/-- Fixed offset (ms) implied by a timed DateTime without an IANA zone: 0 for floating. -/
private def impliedOffsetMs (dt : DateTime) : Int :=
  match dt.utc with
  | none => 0
  | some u => zonedToUtc dt.loc none - u

private def addMs (start : DateTime) (ms : Int) : DateTime :=
  let utc := start.utc.getD 0 + ms
  let loc := match start.tzid with
    | some _ => utcToZoned utc start.tzid
    | none => utcToZoned (utc + impliedOffsetMs start) none
  { start with utc := some utc, loc }

private def timedLike (start : DateTime) (loc : String) : DateTime :=
  let utc := match start.tzid with
    | some _ => zonedToUtc loc start.tzid
    | none => zonedToUtc loc none - impliedOffsetMs start
  { start with utc := some utc, loc, date := none, allDay := false }

/-- DTEND → DTSTART+DURATION → all-day +1 day → zero length. -/
private def resolveEnd (start : DateTime) (dtend : Option DateTime) (duration : Option Duration) : W (DateTime × Int) := do
  let mut end_ := dtend
  if let some e := end_ then
    if e.allDay != start.allDay then
      end_ := some (if start.allDay then addDays e 0 else timedLike start e.loc)
  if end_.isNone then
    if let some d := duration then
      if start.allDay then
        let days : Int := max 1 (d.sign * ((d.weeks * 7 + d.days + (d.hours * 3600 + d.minutes * 60 + d.seconds) / 86400 : Nat) : Int))
        end_ := some (addDays start days)
      else end_ := some (addMs start (max 0 d.toMs))
  let mut e := end_.getD (if start.allDay then addDays start 1 else start)
  let mut durationMs : Int :=
    if start.allDay then (dayOfIso (e.date.getD "") - dayOfIso (start.date.getD "")) * dayMs
    else e.utc.getD 0 - start.utc.getD 0
  if durationMs < 0 || (start.allDay && durationMs == 0) then
    if durationMs < 0 then warn "end-before-start"
    e := if start.allDay then addDays start 1 else start
    durationMs := if start.allDay then dayMs else 0
  return (e, durationMs)

/-- RDATE/EXDATE lists (comma-separated values, per-property TZID/VALUE). -/
private def dateList (props : Array Property) (name : String) (fallbackTz : Option String)
    (vtimezones : VTimezones) : W (List DateTime) := do
  let mut out : Array DateTime := #[]
  for p in props do
    if p.name != name then continue
    if ((param p "VALUE").getD "").toUpper == "PERIOD" then
      warn (name.toLower ++ "-period-skipped")
      continue
    for piece in p.value.splitOn "," do
      if (Str.trimJs piece).isEmpty then continue
      if out.size ≥ maxDateList then
        warn (name.toLower ++ "-truncated")
        return out.toList
      if let some dt ← parseDateTime piece p.params fallbackTz (some vtimezones) then out := out.push dt
  return out.toList

structure Ctx where
  method : Option String
  fallbackTz : Option String
  vtimezones : VTimezones

private def parseVEvent (comp : Component) (lines : Array String) (ctx : Ctx) : W Event := do
  let vt := ctx.vtimezones
  let first (name : String) := comp.prop? name
  let text (name : String) (max : Nat := shortTextMax) : W String := do
    match first name with
    | none => return ""
    | some p =>
      let s := unescapeText p.value
      if utf16Length s > max then warn (name.toLower ++ "-truncated")
      return clampText s max
  let summary ← text "SUMMARY"
  let location ← text "LOCATION"
  let description ← text "DESCRIPTION" descriptionMax

  let stampDt ← match first "DTSTAMP" with
    | some p => parseDateTime p.value p.params none (some vt)
    | none => pure none
  let stampMs : Int := match stampDt with
    | some dt => match dt.utc with | some u => u | none => dayOfIso (dt.date.getD "") * dayMs
    | none => 0

  let start0 ← match first "DTSTART" with
    | some p => parseDateTime p.value p.params ctx.fallbackTz (some vt)
    | none => pure none
  let start : DateTime ← match start0 with
    | some s => pure s
    | none => do
      warn "no-dtstart"
      pure { utc := some stampMs, loc := utcToZoned stampMs none, tzid := some "UTC" }
  let zoneFb := start.tzid.orElse fun _ => ctx.fallbackTz

  let dtend ← match first "DTEND" with
    | some p => parseDateTime p.value p.params zoneFb (some vt)
    | none => pure none
  let durP := first "DURATION"
  let duration := durP.bind (parseDuration ·.value)
  if durP.isSome && duration.isNone then warn "bad-duration"
  let (end_, durationMs) ← resolveEnd start dtend duration

  let mut uid := ((first "UID").map (Str.trimJs ·.value)).getD ""
  if uid.isEmpty then
    warn "no-uid"
    uid := "mv-" ++ hex (sha256 (summary ++ "|" ++ start.loc).toUTF8)
  uid := clampText uid 512

  let recurrenceId ← match first "RECURRENCE-ID" with
    | some p => do
      let dt ← parseDateTime p.value p.params zoneFb (some vt)
      pure (dt.map fun dt => ({ dt with range := if ((param p "RANGE").getD "").toUpper == "THISANDFUTURE" then some "THISANDFUTURE" else none } : RecurrenceId))
    | none => pure none

  let rruleRaw := (first "RRULE").map (Str.trimJs ·.value)
  let rrule := match rruleRaw with
    | some r => if r.isEmpty then none else parseRule? r start.tzid
    | none => none
  if (rruleRaw.map (!·.isEmpty)).getD false && rrule.isNone then warn "bad-rrule"
  if let some r := rrule then
    if !r.unsupported.isEmpty then warn ("rrule-unsupported:" ++ ",".intercalate r.unsupported)

  let rdates ← dateList comp.props "RDATE" zoneFb vt
  let exdates ← dateList comp.props "EXDATE" zoneFb vt

  let organizer := (first "ORGANIZER").map calAddress

  let mut attendees : Array Attendee := #[]
  for p in comp.props do
    if p.name != "ATTENDEE" then continue
    if attendees.size ≥ maxAttendees then
      warn "attendees-truncated"
      break
    let a := calAddress p
    let orD (k d : String) := match param p k with | some v => if v.isEmpty then d else v | none => d
    attendees := attendees.push {
      email := a.email, name := a.name
      partstat := (orD "PARTSTAT" "NEEDS-ACTION").toUpper
      role := (orD "ROLE" "REQ-PARTICIPANT").toUpper
      rsvp := (orD "RSVP" "").toUpper == "TRUE"
      cutype := (orD "CUTYPE" "INDIVIDUAL").toUpper }

  let mut categories : Array String := #[]
  for p in comp.props do
    if p.name != "CATEGORIES" then continue
    for piece in splitUnescaped p.value ',' do
      let c := clampText (Str.trimJs (unescapeText piece)) 100
      if !c.isEmpty && !categories.contains c && categories.size < maxCategories then categories := categories.push c

  let trimmed (name : String) : Option String :=
    (first name).bind fun p => let v := Str.trimJs p.value; if v.isEmpty then none else some v
  let status := (trimmed "STATUS").map String.toUpper
  let sequence := match first "SEQUENCE" with
    | some p => match jsInteger? (Str.trimJs p.value) with
      | some n => if n ≥ 0 then n.toNat else 0
      | none => 0
    | none => 0
  let url := (trimmed "URL").map (clampText · 2048)
  let conference := (trimmed "X-GOOGLE-CONFERENCE").map (clampText · 2048)

  let mut ics := "\r\n".intercalate ((lines.extract comp.start (comp.end_ + 1)).toList)
  if ics.utf8ByteSize > icsMax then
    warn "ics-truncated"
    ics := capUtf8 ics icsMax

  return {
    uid, recurrenceId, sequence, stamp := stampMs / 1000, method := ctx.method, status, summary,
    location, description, url, conference, start, end_, durationMs, rrule, rruleRaw, rdates,
    exdates, organizer, attendees := attendees.toList, categories := categories.toList, ics }

private def parseVTimezone (vtz : Component) : Option (String × Option Int) := do
  let p ← vtz.prop? "TZID"
  let tzid := stripQuotes (Str.trimJs p.value)
  if tzid.isEmpty then none
  let std := vtz.children.find? (·.name == "STANDARD")
  let off : Option Int := do
    let to ← (← std).prop? "TZOFFSETTO"
    let cs := (Str.trimJs to.value).toList
    match cs with
    | s :: a :: b :: c :: d :: rest =>
      if (s == '+' || s == '-') && [a, b, c, d].all Str.isDigit && (rest.isEmpty || (rest.length == 2 && rest.all Str.isDigit)) then
        let v : Int := (String.ofList [a, b]).toNat! * 60 + (String.ofList [c, d]).toNat!
        some (if s == '-' then -v else v)
      else none
    | _ => none
  pure (tzid, off)

/-- Parse an iCalendar text. VEVENTs (inside VCALENDAR or bare) become records. `method`: the
iTIP method from the MIME part, used when the calendar has no METHOD; `tz`: zone for floating
times when there is no X-WR-TIMEZONE. -/
def parseCalendar (text : String) (method : Option String := none) (tz : Option String := none) : Calendar :=
  let act : W Calendar := do
    let (root, lines) ← parseComponents text
    let calendars := root.children.filter (·.name == "VCALENDAR")
    let scopes := if calendars.isEmpty then #[root] else calendars
    let cal := scopes[0]!
    let method :=
      match (cal.prop? "METHOD").map (Str.trimJs ·.value) with
      | some m => if m.isEmpty then (method.map String.toUpper) else some m.toUpper
      | none => method.map String.toUpper
    let method := match method with | some m => if m.isEmpty then none else some m | none => none
    let calname := (cal.prop? "X-WR-CALNAME").bind fun p =>
      let n := clampText (Str.trimJs (unescapeText p.value)) 200
      if n.isEmpty then none else some n
    let mut tzid : Option String := none
    if let some p := cal.prop? "X-WR-TIMEZONE" then
      let v := Str.trimJs p.value
      if !v.isEmpty then
        tzid := (resolveTzid (some v)).tzid
        if tzid.isNone then warn ("unknown-x-wr-timezone:" ++ clampText v 40)
    if tzid.isNone then
      if let some t := tz then if !t.isEmpty then tzid := canonicalTimeZone (some t)
    let mut vtimezones : VTimezones := []
    for scope in scopes do
      for c in scope.children do
        if c.name != "VTIMEZONE" then continue
        if let some (id, off) := parseVTimezone c then
          if !(vtimezones.any (·.1 == id)) then vtimezones := vtimezones ++ [(id, off)]
    let ctx : Ctx := { method, fallbackTz := tzid, vtimezones }
    let mut events : Array Event := #[]
    for scope in scopes do
      for c in scope.children do
        if c.name != "VEVENT" then continue
        events := events.push (← parseVEvent c lines ctx)
    if calendars.isEmpty && !events.isEmpty then warn "no-vcalendar"
    return { method, calname, tzid, events := events.toList, vtimezones, warnings := [] }
  let (cal, ws) := act.run #[]
  { cal with warnings := ws.toList }

/-- The VEVENT records of an iCalendar text. -/
def parse (text : String) : List Event := (parseCalendar text).events

/-- Stable row key for the `events` table: base64url(SHA-256("mv-evt\0" + uid + "\0" + rid)),
rid = '' | 'T' + utcSeconds | 'D' + YYYYMMDD (`eventRowKey`). -/
def rowKey (uid : String) (recurrenceId : Option DateTime) : String :=
  Core.Base64.encodeUrl (sha256 ("mv-evt\x00" ++ uid ++ "\x00" ++ ridKey recurrenceId).toUTF8)

/-! ## Calendar parts of a message (`findCalendarText`) -/

structure Part where
  mimeType : Option String := none
  filename : Option String := none
  method : Option String := none
  content : ByteArray

private def containsCI (hay needle : String) : Bool :=
  let h := hay.toUpper.toList.toArray
  (Str.indexOf h needle.toUpper.toList.toArray).isSome

/-- `;\s*method\s*=\s*"?([a-z-]+)"?` (case-insensitive) in a Content-Type value. -/
private def methodParam? (mimeFull : String) : Option String := Id.run do
  let cs := mimeFull.toList.toArray
  for h : i in [0:cs.size] do
    if cs[i] != ';' then continue
    let mut j := i + 1
    while j < cs.size && Str.isJsWhitespace cs[j]! do j := j + 1
    if (Str.slice cs j (j + 6)).toLower != "method" then continue
    j := j + 6
    while j < cs.size && Str.isJsWhitespace cs[j]! do j := j + 1
    if j ≥ cs.size || cs[j]! != '=' then continue
    j := j + 1
    while j < cs.size && Str.isJsWhitespace cs[j]! do j := j + 1
    if j < cs.size && cs[j]! == '"' then j := j + 1
    let st := j
    while j < cs.size && (Str.isAlpha cs[j]! || cs[j]! == '-') do j := j + 1
    if j > st then return some (Str.slice cs st j).toUpper
  return none

/-- The iCalendar parts among a message's attachments: text/calendar, application/ics or a
*.ics filename, at most 1 MiB each, decoded as UTF-8 (lossy) without a BOM, with the iTIP method
from the part or its Content-Type. -/
def findCalendarText (parts : List Part) : List (String × Option String) := Id.run do
  let mut out : Array (String × Option String) := #[]
  for a in parts do
    let mimeFull := (a.mimeType.getD "").toLower
    let mime := Str.trimJs ((mimeFull.splitOn ";").headD "")
    let filename := (a.filename.getD "").toLower
    if !(mime == "text/calendar" || mime == "application/ics" || filename.endsWith ".ics") then continue
    if a.content.size > calendarPartMax then continue
    let text := Core.Text.Charset.utf8Lossy a.content
    let text := match text.toList with | '﻿' :: r => String.ofList r | _ => text
    if !(containsCI text "BEGIN:VCALENDAR" || containsCI text "BEGIN:VEVENT") then continue
    let m := (a.method.map fun m => (Str.trimJs m).toUpper).bind fun m => if m.isEmpty then none else some m
    let m := m.orElse fun _ => methodParam? mimeFull
    out := out.push (text, m)
  return out.toList

/-! ## Occurrences (`expandOccurrences` / `lastOccurrenceEnd` of `shared/rrule.js`) -/

def Event.series (e : Event) : Series :=
  { start := e.start, end_ := e.end_, durationMs := e.durationMs, rrule := e.rrule, rdates := e.rdates, exdates := e.exdates }

def isCancelled (e : Event) : Bool := e.status == some "CANCELLED" || e.method == some "CANCEL"

/-- A recurrence master: an RRULE or RDATEs, and not itself an override. -/
def isRecurring (e : Event) : Bool := e.recurrenceId.isNone && (e.rrule.isSome || !e.rdates.isEmpty)

/-- Epoch ms of the first (seed) start. -/
def firstStart (e : Event) : Int :=
  let s := e.start
  let tz := canonicalTimeZone s.tzid
  if s.allDay then
    match tz with
    | some _ => zonedToUtc (s.dateOf ++ "T00:00:00") tz
    | none => dayOfIso s.dateOf * dayMs
  else match s.utc with
    | some u => u
    | none => zonedToUtc s.loc tz

private def recordSpan (e : Event) : Int × Int :=
  let start := firstStart e
  let s := e.start
  let en := e.end_
  let tz := canonicalTimeZone s.tzid
  let end_ :=
    if s.allDay then
      let endDay := max (dayOfIso en.dateOf) (dayOfIso s.dateOf + 1)
      match tz with
      | some _ => zonedToUtc (isoDate endDay ++ "T00:00:00") tz
      | none => endDay * dayMs
    else match en.utc with
      | some u => if !en.floating then u else start + max 0 e.durationMs
      | none => start + max 0 e.durationMs
  (start, max start end_)

structure Occurrence where
  uid : String
  start : Int
  end_ : Int
  allDay : Bool
  /-- local calendar date of `start` (display zone, else the event zone) -/
  dateKey : String
  isOverride : Bool
  /-- the original slot of the instance (what a RECURRENCE-ID would carry) -/
  recurrenceId : Option DateTime
  cancelled : Bool
  complex : Bool
  deriving Repr, BEq, Inhabited

private def slotDateTime (ctx : Core.RRule.Ctx) (inst : Instance) : DateTime :=
  let s := ctx.series.start
  { utc := if ctx.allDay then none else some inst.start, loc := inst.loc, date := if ctx.allDay then inst.date else none,
    tzid := s.tzid, tzidRaw := s.tzidRaw, allDay := ctx.allDay, floating := s.floating }

private def mkOcc (src : Event) (start end_ : Int) (allDay : Bool) (rid : Option DateTime) (isOverride cancelled complex : Bool)
    (displayTz : Option String) (date : Option String) : Occurrence :=
  let dateKey := match allDay, date with
    | true, some d => d
    | _, _ => take10 (utcToZoned start displayTz)
  { uid := src.uid, start, end_, allDay, dateKey, isOverride, recurrenceId := rid, cancelled, complex }

/-- The occurrences of `record` that intersect [rangeStart, rangeEnd), sorted by (start, end).
Masters recur in wall-clock time in their zone; COUNT counts from DTSTART, UNTIL is inclusive,
EXDATEs remove instances, RDATEs are merged in; `overrides` (same UID, with RECURRENCE-ID)
replace their slot, cancelled ones remove it, and one moved into the range from outside still
shows. Unsupported rules or a THISANDFUTURE override degrade to the first instance, `complex`.
`tz`: zone for `dateKey` (default: the event's). -/
def expandOccurrences (record : Event) (overrides : List Event) (rangeStart rangeEnd : Int)
    (tz : Option String := none) (maxInstances : Nat := 2000) : List Occurrence := Id.run do
  let ctx := context record.series
  let displayTz := (canonicalTimeZone tz).orElse fun _ => ctx.tz
  let cancelled := isCancelled record
  let inRange (s e : Int) := s < rangeEnd && (e > rangeStart || s ≥ rangeStart)
  if !isRecurring record then
    let (s, e) := recordSpan record
    if inRange s e then
      return [mkOcc record s e ctx.allDay (record.recurrenceId.map (·.toDateTime)) record.recurrenceId.isSome cancelled false
        displayTz (if ctx.allDay then some record.start.dateOf else none)]
    return []
  let mine := (overrides.filter fun o => o.uid == record.uid && o.recurrenceId.isSome).toArray
  let complex := (record.rrule.map (·.isComplex)).getD false ||
    mine.any (fun o => (o.recurrenceId.bind (·.range)) == some "THISANDFUTURE")
  if complex then
    let seed := instanceForDay ctx ctx.seedDay
    if inRange seed.start seed.end_ then
      return [mkOcc record seed.start seed.end_ ctx.allDay (some (slotDateTime ctx seed)) false cancelled true displayTz seed.date]
    return []
  let mut byKey : List (String × Nat) := []
  let addKey (m : List (String × Nat)) (k : String) (i : Nat) := if m.any (·.1 == k) then m else m ++ [(k, i)]
  for h : i in [0:mine.size] do
    let rid := (mine[i].recurrenceId.getD default).toDateTime
    byKey := addKey byKey (ridKey (some rid)) i
    byKey := addKey byKey ("L" ++ rid.loc) i
    if !rid.allDay then byKey := addKey byKey ("D" ++ String.ofList ((take10 rid.loc).toList.filter (· != '-'))) i
  let get? (k : String) : Option Nat := (byKey.find? (·.1 == k)).map (·.2)
  let findOverride (inst : Instance) : Option Nat :=
    if ctx.allDay then
      (get? ("D" ++ String.ofList ((inst.date.getD "").toList.filter (· != '-')))).orElse fun _ => get? ("L" ++ inst.loc)
    else
      (get? ("T" ++ toString (inst.start / 1000))).orElse fun _ =>
        (get? ("L" ++ inst.loc)).orElse fun _ => get? ("D" ++ String.ofList ((take10 inst.loc).toList.filter (· != '-')))
  let emitOverride (o : Event) : Option Occurrence :=
    let (s, e) := recordSpan o
    if !inRange s e then none else
    let allDay := o.start.allDay
    some (mkOcc o s e allDay (o.recurrenceId.map (·.toDateTime)) true cancelled false displayTz
      (if allDay then some o.start.dateOf else none))
  let ex := exdateIndex ctx
  let fromDay := localDayOf ctx rangeStart - ctx.slackDays - 1
  let limitDay := localDayOf ctx rangeEnd + 1
  let (_, (out, used)) := walkInstances ctx (some fromDay) (some limitDay) maxInstances
    ((#[] : Array Occurrence), ([] : List Nat)) fun inst (out, used) =>
      if inst.start ≥ rangeEnd then (false, (out, used)) else
      match findOverride inst with
      | some i =>
        let o := mine[i]!
        let out := if isCancelled o then out else match emitOverride o with | some occ => out.push occ | none => out
        (true, (out, i :: used))
      | none =>
        if isExcluded ex inst ctx then (true, (out, used))
        else if inRange inst.start inst.end_ then
          (true, (out.push (mkOcc record inst.start inst.end_ ctx.allDay (some (slotDateTime ctx inst)) false cancelled false displayTz inst.date), used))
        else (true, (out, used))
  let mut out := out
  for h : i in [0:mine.size] do
    let o := mine[i]
    if !used.contains i && !isCancelled o then
      if let some occ := emitOverride o then out := out.push occ
  return out.toList.mergeSort fun a b => decide (a.start < b.start || (a.start == b.start && a.end_ ≤ b.end_))

/-- Epoch ms when the last instance ends, or none when the series is unbounded, its rule is beyond
this expander, the walk hit `maxInstances`, or an instance starts after `horizon`. -/
def lastOccurrenceEnd (record : Event) (maxInstances : Nat := 5000) (horizon : Option Int := none) : Option Int := Id.run do
  if !isRecurring record then return some (recordSpan record).2
  let ctx := context record.series
  let mut limitDay : Option Int := none
  if let some rule := record.rrule then
    if rule.count.isNone && rule.until_.isNone then return none
    if rule.isComplex then
      let some u := rule.until_ | return none
      let uMs : Option Int := match u.utc with
        | some x => some x
        | none => u.date.map fun d => (dayOfIso d + 1) * dayMs
      let some uMs := uMs | return none
      let mut last := uMs + ctx.durationMs
      for r in (sortedRdates ctx).toList do last := max last r.end_
      return match horizon with
        | some hz => if last > hz + ctx.durationMs then none else some last
        | none => some last
    if let some u := rule.until_ then
      let untilDay : Option Int := match u.date with
        | some d => some (dayOfIso d)
        | none => u.utc.map (· / dayMs)
      if let some ud := untilDay then limitDay := some (ud + 1)
  let (capped, (last, beyond)) := walkInstances ctx none limitDay maxInstances ((none : Option Int), false) fun inst (last, beyond) =>
    match horizon with
    | some hz => if inst.start > hz then (false, (last, true)) else (true, (some (match last with | some l => max l inst.end_ | none => inst.end_), beyond))
    | none => (true, (some (match last with | some l => max l inst.end_ | none => inst.end_), beyond))
  if capped || beyond then return none
  return last

/-- Retention for a calendar row: unbounded/complex series keep 2 years from receipt; bounded ones
keep 30 days past their last instance, never less than 90 days from receipt. -/
def computeEventExpiry (record : Event) (receivedAtMs : Int) : Int :=
  match lastOccurrenceEnd record with
  | none => receivedAtMs + 730 * dayMs
  | some e => max (e + 30 * dayMs) (receivedAtMs + 90 * dayMs)

end Core.Ical
