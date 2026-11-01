import Core.Ical
import Lean.Data.Json

/-! Calendar tests: `Core.Ical` / `Core.RRule` against MailVault's `shared/ical.js` and
`shared/rrule.js`. The vectors (`fixtures/rrule-vectors.json`, written by MailVault's
`scripts/rrule-vectors.ts`) hold, per case, the parsed record (less `ics`), its overrides, a
range, a display zone and what `expandOccurrences` / `lastOccurrenceEnd` said; the zone checks pin
`zonedToUtc` / `utcToZoned`. The rest are the vitest assertions of `test/unit/ical.test.ts` and
`test/unit/rrule.test.ts`, ported by hand. One line per check, exit 1 on any failure. -/

open Core.Ical Lean
open Core.RRule hiding parse

structure Tally where
  passed : Nat := 0
  failed : Nat := 0

def check (t : IO.Ref Tally) (name : String) (ok : Bool) (detail : String := "") : IO Unit := do
  if ok then
    IO.println s!"ok   {name}"
    t.modify fun x => { x with passed := x.passed + 1 }
  else
    IO.println s!"FAIL {name}{if detail.isEmpty then "" else ": " ++ detail}"
    t.modify fun x => { x with failed := x.failed + 1 }

def checkEq [BEq α] [ToString α] (t : IO.Ref Tally) (name : String) (expected got : α) : IO Unit :=
  check t name (expected == got) s!"expected {expected} got {got}"

/-- First difference between two JSON values, as a path and both sides. -/
partial def jsonDiff (exp got : Json) (path : String := "") : Option String :=
  match exp, got with
  | .obj _, .obj _ =>
    let keys (j : Json) : List String := match j with
      | .obj kvs => kvs.toArray.toList.map (·.1)
      | _ => []
    let all := (keys exp ++ keys got).eraseDups
    all.findSome? fun k =>
      match exp.getObjVal? k, got.getObjVal? k with
      | .ok a, .ok b => jsonDiff a b (path ++ "." ++ k)
      | .ok a, .error _ => some s!"{path}.{k}: expected {a.compress} got (missing)"
      | .error _, .ok b => some s!"{path}.{k}: expected (missing) got {b.compress}"
      | _, _ => none
  | .arr a, .arr b =>
    if a.size != b.size then some s!"{path}: expected {a.size} items got {b.size} ({exp.compress} vs {got.compress})"
    else (List.range a.size).findSome? fun i => jsonDiff a[i]! b[i]! s!"{path}[{i}]"
  | a, b => if a.compress == b.compress then none else some s!"{path}: expected {a.compress} got {b.compress}"

/-! ## Records from JSON (as the vault stores them) -/

def optStr (j : Json) (k : String) : Option String := (j.getObjValAs? (Option String) k).toOption.join
def optInt (j : Json) (k : String) : Option Int := (j.getObjValAs? (Option Int) k).toOption.join
def jstr (j : Json) (k : String) : String := (j.getObjValAs? String k).toOption.getD ""
def jint (j : Json) (k : String) : Int := (j.getObjValAs? Int k).toOption.getD 0
def jbool (j : Json) (k : String) : Bool := (j.getObjValAs? Bool k).toOption.getD false
def jarr (j : Json) (k : String) : List Json := ((j.getObjValAs? (Array Json) k).toOption.getD #[]).toList
def isNull (j : Json) (k : String) : Bool := match j.getObjVal? k with | .ok .null => true | .ok _ => false | .error _ => true

def dtOfJson (j : Json) : DateTime :=
  { utc := optInt j "utc", loc := jstr j "local", date := optStr j "date", tzid := optStr j "tzid",
    tzidRaw := optStr j "tzidRaw", allDay := jbool j "allDay", floating := jbool j "floating" }

def ruleOfJson (j : Json) : RRule :=
  let ints (k : String) : Option (List Int) := (j.getObjValAs? (Array Int) k).toOption.map (·.toList)
  { freq := match jstr j "freq" with | "WEEKLY" => .weekly | "MONTHLY" => .monthly | "YEARLY" => .yearly | _ => .daily
    interval := (jint j "interval").toNat
    count := (j.getObjValAs? Nat "count").toOption
    until_ := (j.getObjVal? "until").toOption.map fun u => { utc := optInt u "utc", date := optStr u "date" }
    byday := (j.getObjValAs? (Array Json) "byday").toOption.map fun l => l.toList.map fun b => { ord := jint b "ord", day := jstr b "day" }
    bymonthday := ints "bymonthday", bymonth := ints "bymonth", bysetpos := ints "bysetpos"
    wkst := jstr j "wkst"
    unsupported := ((j.getObjValAs? (Array String) "unsupported").toOption.getD #[]).toList }

def eventOfJson (j : Json) : Event :=
  let sub (k : String) : Option Json := if isNull j k then none else (j.getObjVal? k).toOption
  { v := (jint j "v").toNat, uid := jstr j "uid"
    recurrenceId := (sub "recurrenceId").map fun r => { dtOfJson r with range := optStr r "range" }
    sequence := (jint j "sequence").toNat, stamp := jint j "stamp", method := optStr j "method", status := optStr j "status"
    summary := jstr j "summary", location := jstr j "location", description := jstr j "description"
    url := optStr j "url", conference := optStr j "conference"
    start := dtOfJson ((sub "start").getD .null), end_ := dtOfJson ((sub "end").getD .null)
    durationMs := jint j "durationMs", rrule := (sub "rrule").map ruleOfJson, rruleRaw := optStr j "rruleRaw"
    rdates := (jarr j "rdates").map dtOfJson, exdates := (jarr j "exdates").map dtOfJson
    organizer := (sub "organizer").map fun o => { email := jstr o "email", name := jstr o "name" }
    attendees := (jarr j "attendees").map fun a =>
      { email := jstr a "email", name := jstr a "name", partstat := jstr a "partstat", role := jstr a "role",
        rsvp := jbool a "rsvp", cutype := jstr a "cutype" }
    categories := (jarr j "categories").filterMap fun c => match c with | .str s => some s | _ => none }

def occJson (o : Occurrence) : Json :=
  Json.mkObj [("uid", o.uid), ("start", o.start), ("end", o.end_), ("allDay", o.allDay), ("dateKey", o.dateKey),
    ("isOverride", o.isOverride), ("cancelled", o.cancelled), ("complex", o.complex), ("rid", ridKey o.recurrenceId)]

/-! ## Helpers mirroring the vitest files -/

/-- `Date.parse` of 'YYYY-MM-DDTHH:MM:SSZ'. -/
def T (s : String) : Int := zonedToUtc (String.ofList (s.toList.filter (· != 'Z'))) none
def iso (ms : Int) : String := utcToZoned ms none ++ ".000Z"
def starts (occ : List Occurrence) : List String := occ.map fun o => if o.allDay then o.dateKey else iso o.start

def wrapVevent (text : String) : String :=
  if (text.splitOn "BEGIN:VCALENDAR").length > 1 then text
  else "BEGIN:VCALENDAR\nBEGIN:VEVENT\n" ++ Core.Text.Str.trimJs text ++ "\nEND:VEVENT\nEND:VCALENDAR"

/-- Master and overrides of an inline VEVENT body (or whole calendar). -/
def load (text : String) : Event × List Event :=
  let evs := parse (wrapVevent text)
  ((evs.find? (·.recurrenceId.isNone)).getD default, evs.filter (·.recurrenceId.isSome))

def master (text : String) : Event := (load text).1

def fixture (name : String) : IO String := IO.FS.readFile s!"test/fixtures/ics/{name}"

def byUid (evs : List Event) (uid : String) : Event := (evs.find? (·.uid == uid)).getD default

def runW (x : W α) : α × List String := let (a, w) := x.run #[]; (a, w.toList)

def inlineCalendars : List (String × String) := [
  ("exdate-and-rdate", r#"
BEGIN:VEVENT
UID:exr@x
DTSTAMP:20260101T000000Z
DTSTART;TZID=America/New_York:20260105T090000
DTEND;TZID=America/New_York:20260105T093000
RRULE:FREQ=WEEKLY;COUNT=6
EXDATE;TZID=America/New_York:20260112T090000
RDATE;TZID=America/New_York:20260114T150000,20260301T090000
SUMMARY:Weekly with a hole and two extras
END:VEVENT"#),
  ("spring-forward-gap", r#"
BEGIN:VEVENT
UID:gap@x
DTSTAMP:20260101T000000Z
DTSTART;TZID=America/Los_Angeles:20260301T023000
DURATION:PT45M
RRULE:FREQ=DAILY;COUNT=12
SUMMARY:02:30 every night across the spring gap
END:VEVENT"#),
  ("fall-back-ambiguous", r#"
BEGIN:VEVENT
UID:amb@x
DTSTAMP:20260101T000000Z
DTSTART;TZID=America/Los_Angeles:20261028T013000
DTEND;TZID=America/Los_Angeles:20261028T020000
RRULE:FREQ=DAILY;COUNT=8
SUMMARY:01:30 across the autumn repeat
END:VEVENT"#),
  ("wkst-sunday-biweekly", r#"
BEGIN:VEVENT
UID:wkst@x
DTSTAMP:20260101T000000Z
DTSTART;TZID=Europe/Berlin:20260106T180000
DTEND;TZID=Europe/Berlin:20260106T190000
RRULE:FREQ=WEEKLY;INTERVAL=2;BYDAY=TU,SU;WKST=SU;COUNT=9
SUMMARY:Every other week, the week starting Sunday
END:VEVENT"#),
  ("monthly-31st", r#"
BEGIN:VEVENT
UID:m31@x
DTSTAMP:20260101T000000Z
DTSTART;TZID=Europe/London:20260131T120000
DTEND;TZID=Europe/London:20260131T130000
RRULE:FREQ=MONTHLY
SUMMARY:The 31st, in the months that have one
END:VEVENT"#),
  ("monthly-friday-13th", r#"
BEGIN:VEVENT
UID:f13@x
DTSTAMP:20260101T000000Z
DTSTART;TZID=America/Chicago:20260213T080000
DTEND;TZID=America/Chicago:20260213T083000
RRULE:FREQ=MONTHLY;BYMONTHDAY=13;BYDAY=FR
SUMMARY:Friday the 13th
END:VEVENT"#),
  ("yearly-leap-day", r#"
BEGIN:VEVENT
UID:leap@x
DTSTAMP:20240101T000000Z
DTSTART;VALUE=DATE:20240229
RRULE:FREQ=YEARLY
SUMMARY:29 February, all day
END:VEVENT"#),
  ("yearly-20th-monday", r#"
BEGIN:VEVENT
UID:y20@x
DTSTAMP:20260101T000000Z
DTSTART;TZID=Asia/Tokyo:20260518T100000
DTEND;TZID=Asia/Tokyo:20260518T110000
RRULE:FREQ=YEARLY;BYDAY=20MO
SUMMARY:The twentieth Monday of the year
END:VEVENT"#),
  ("yearly-two-months", r#"
BEGIN:VEVENT
UID:y2m@x
DTSTAMP:20260101T000000Z
DTSTART;TZID=Australia/Sydney:20260410T070000
DTEND;TZID=Australia/Sydney:20260410T080000
RRULE:FREQ=YEARLY;BYMONTH=4,10;BYMONTHDAY=10;UNTIL=20281231T000000Z
SUMMARY:The tenth of April and of October, south of the line
END:VEVENT"#),
  ("allday-weekly-with-override-and-cancel", r#"
BEGIN:VEVENT
UID:adw@x
DTSTAMP:20260101T000000Z
DTSTART;VALUE=DATE:20260302
DTEND;VALUE=DATE:20260304
RRULE:FREQ=WEEKLY;COUNT=5
EXDATE;VALUE=DATE:20260316
SUMMARY:Two days, weekly
END:VEVENT
BEGIN:VEVENT
UID:adw@x
DTSTAMP:20260102T000000Z
RECURRENCE-ID;VALUE=DATE:20260309
DTSTART;VALUE=DATE:20260311
DTEND;VALUE=DATE:20260312
SUMMARY:Moved to Wednesday, one day
END:VEVENT
BEGIN:VEVENT
UID:adw@x
DTSTAMP:20260102T000000Z
RECURRENCE-ID;VALUE=DATE:20260323
DTSTART;VALUE=DATE:20260323
DTEND;VALUE=DATE:20260325
STATUS:CANCELLED
SUMMARY:Not this week
END:VEVENT"#),
  ("override-moved-out-of-its-week", r#"
BEGIN:VEVENT
UID:mov@x
DTSTAMP:20260101T000000Z
DTSTART;TZID=America/New_York:20260107T140000
DTEND;TZID=America/New_York:20260107T150000
RRULE:FREQ=WEEKLY;BYDAY=WE
SUMMARY:Wednesdays
END:VEVENT
BEGIN:VEVENT
UID:mov@x
DTSTAMP:20260102T000000Z
RECURRENCE-ID;TZID=America/New_York:20260121T140000
DTSTART;TZID=America/New_York:20260305T100000
DTEND;TZID=America/New_York:20260305T110000
SUMMARY:That Wednesday, moved to March
END:VEVENT"#),
  ("this-and-future", r#"
BEGIN:VEVENT
UID:taf@x
DTSTAMP:20260101T000000Z
DTSTART;TZID=Europe/Paris:20260105T090000
DTEND;TZID=Europe/Paris:20260105T100000
RRULE:FREQ=WEEKLY
SUMMARY:Weekly, until someone moved the rest
END:VEVENT
BEGIN:VEVENT
UID:taf@x
DTSTAMP:20260102T000000Z
RECURRENCE-ID;RANGE=THISANDFUTURE;TZID=Europe/Paris:20260202T090000
DTSTART;TZID=Europe/Paris:20260202T110000
DTEND;TZID=Europe/Paris:20260202T120000
SUMMARY:From here on, at eleven
END:VEVENT"#),
  ("floating", r#"
BEGIN:VEVENT
UID:flt@x
DTSTAMP:20260101T000000Z
DTSTART:20260110T073000
DTEND:20260110T080000
RRULE:FREQ=DAILY;INTERVAL=3;COUNT=5
EXDATE:20260116T073000
SUMMARY:No zone at all
END:VEVENT"#),
  ("utc-monthly-last-weekday", r#"
BEGIN:VEVENT
UID:utc@x
DTSTAMP:20260101T000000Z
DTSTART:20260130T160000Z
DTEND:20260130T170000Z
RRULE:FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1;COUNT=7
SUMMARY:The last working day, in UTC
END:VEVENT"#),
  ("fixed-offset", r#"
BEGIN:VEVENT
UID:fix@x
DTSTAMP:20260101T000000Z
DTSTART;TZID="(UTC+05:30) Chennai, Kolkata, Mumbai, New Delhi":20260112T093000
DTEND;TZID="(UTC+05:30) Chennai, Kolkata, Mumbai, New Delhi":20260112T100000
RRULE:FREQ=WEEKLY;COUNT=3
SUMMARY:A Windows zone name
END:VEVENT"#),
  ("long-event-overlapping-range", r#"
BEGIN:VEVENT
UID:long@x
DTSTAMP:20260101T000000Z
DTSTART;TZID=America/Denver:20260201T220000
DURATION:P2DT6H
RRULE:FREQ=MONTHLY;BYMONTHDAY=1
SUMMARY:Starts late on the first and runs two days
END:VEVENT"#),
  ("rdate-only", r#"
BEGIN:VEVENT
UID:rd@x
DTSTAMP:20260101T000000Z
DTSTART;TZID=America/New_York:20260203T100000
DTEND;TZID=America/New_York:20260203T103000
RDATE;TZID=America/New_York:20260210T100000,20260217T160000,20260203T100000
SUMMARY:Three dates and no rule
END:VEVENT"#),
  ("single-cancelled", r#"
BEGIN:VEVENT
UID:can@x
DTSTAMP:20260101T000000Z
DTSTART;TZID=America/New_York:20260203T100000
DTEND;TZID=America/New_York:20260203T103000
STATUS:CANCELLED
SUMMARY:Called off
END:VEVENT"#)]

/-! ## Vectors -/

/-- The calendar each vector label was parsed from, wrapped the way `casesOf` wraps it. -/
def sources : IO (List (String × String)) := do
  let dir : System.FilePath := "test/fixtures/ics"
  let names := (← dir.readDir).map (·.fileName)
  let files := (names.filter fun n => n.endsWith ".ics").qsort (fun a b => a < b)
  let mut out : List (String × String) := []
  for f in files do out := out ++ [(f, ← IO.FS.readFile (dir / f))]
  for (n, body) in inlineCalendars do
    out := out ++ [(n, if (body.splitOn "BEGIN:VCALENDAR").length > 1 then body
      else "BEGIN:VCALENDAR\n" ++ Core.Text.Str.trimJs body ++ "\nEND:VCALENDAR")]
  return out

def labelOf (name : String) : String := (name.splitOn "#").headD ""

def zoneVectors (t : IO.Ref Tally) (v : Json) : IO Unit := do
  for z in jarr v "zones" do
    let loc := jstr z "local"
    let tz := optStr z "tz"
    let utc := jint z "utc"
    let back := jstr z "back"
    let got := zonedToUtc loc tz
    checkEq t s!"zone {loc} in {tz.getD "null"} is {utc} ms" utc got
    checkEq t s!"zone {utc} ms in {tz.getD "null"} reads {back}" back (utcToZoned utc tz)

/-- Every vector record (and override) equals what `parse` makes of its source calendar. -/
def parseVectors (t : IO.Ref Tally) (v : Json) : IO Unit := do
  let srcs ← sources
  let mut seen : List String := []
  let mut perLabel : List (String × Nat) := []
  for c in jarr v "cases" do
    let label := labelOf (jstr c "name")
    let some (_, text) := srcs.find? (·.1 == label)
      | check t s!"parse {label}: source found" false; continue
    let events := parse text
    for rj in [(c.getObjVal? "record").toOption.getD .null] ++ jarr c "overrides" do
      let uid := jstr rj "uid"
      let rid := ridKey ((eventOfJson rj).recurrenceId.map (·.toDateTime))
      let key := s!"{label}#{uid} {rid}"
      if seen.contains key then continue
      seen := seen ++ [key]
      perLabel := match perLabel.find? (·.1 == label) with
        | some _ => perLabel.map fun (l, n) => if l == label then (l, n + 1) else (l, n)
        | none => perLabel ++ [(label, 1)]
      match events.find? (fun e => e.uid == uid && ridKey (e.recurrenceId.map (·.toDateTime)) == rid) with
      | none => check t s!"parse {key}" false "no such event parsed"
      | some e =>
        match Json.parse (e.toJson (withIcs := false)) with
        | .error err => check t s!"parse {key}" false s!"our JSON does not parse: {err}"
        | .ok got =>
          match jsonDiff rj got with
          | none => check t s!"parse {key}" true
          | some d => check t s!"parse {key}" false d
  for (label, n) in perLabel do
    let some (_, text) := srcs.find? (·.1 == label) | continue
    checkEq t s!"parse {label} yields {n} events" n (parse text).length

/-- `expandOccurrences` and `lastOccurrenceEnd` of every vector case, from the stored JSON record. -/
def expandVectors (t : IO.Ref Tally) (v : Json) : IO Unit := do
  for c in jarr v "cases" do
    let name := jstr c "name"
    let record := eventOfJson ((c.getObjVal? "record").toOption.getD .null)
    let overrides := (jarr c "overrides").map eventOfJson
    let occ := expandOccurrences record overrides (jint c "from") (jint c "to") (optStr c "tz")
    let exp := Json.arr (jarr c "occurrences").toArray
    let got := Json.arr (occ.map occJson).toArray
    match jsonDiff exp got with
    | none => check t s!"expand {name}" true
    | some d => check t s!"expand {name}" false d
    let lastExp := optInt c "lastEnd"
    let lastGot := lastOccurrenceEnd record
    check t s!"lastEnd {name}" (lastExp == lastGot) s!"expected {lastExp} got {lastGot}"
    -- the plain-rule API agrees wherever nothing but the rule is in play
    if let some rule := record.rrule then
      if record.rdates.isEmpty && record.exdates.isEmpty && overrides.isEmpty then
        let plain := Core.RRule.expand rule record.start (jint c "from") (jint c "to") record.durationMs
        let want := (jarr c "occurrences").map (jint · "start")
        check t s!"RRule.expand {name}" (plain == want) s!"expected {want} got {plain}"

/-! ## `ical.test.ts`, by hand -/

def dt (loc : String) (utc : Option Int := none) (date : Option String := none) (tzid tzidRaw : Option String := none)
    (allDay floating : Bool := false) : DateTime := { utc, loc, date, tzid, tzidRaw, allDay, floating }

def icalUnit (t : IO.Ref Tally) : IO Unit := do
  -- content lines
  checkEq t "unfold joins CRLF, CR and LF continuations" "A:bcde\nF:g" (unfold "A:b\r\n c\r d\n\te\nF:g")
  checkEq t "unfold treats a bare CR as a line break" "A:b\nC:d" (unfold "A:b\rC:d")
  checkEq t "unfold drops a BOM" "BEGIN:VCALENDAR" (unfold "﻿BEGIN:VCALENDAR")
  let p := parseContentLine "attendee;cn=\"Chen, Wei\";Member=\"mailto:a@x.example\",\"mailto:b@y.example\";rsvp=TRUE:mailto:wei@x.example"
  check t "a content line's value starts at the first colon outside quotes" <| match p with
    | some p => p.name == "ATTENDEE" && p.value == "mailto:wei@x.example" && p.params.get? "CN" == some "Chen, Wei" &&
        (p.params.find? (·.1 == "MEMBER")).map (·.2) == some ["mailto:a@x.example", "mailto:b@y.example"] &&
        p.params.get? "RSVP" == some "TRUE"
    | none => false
  check t "parameter names are upper-cased and values keep their case" <|
    ((parseContentLine "DTSTART;tzid=America/Los_Angeles:20260101T090000").bind (·.params.get? "TZID")) == some "America/Los_Angeles"
  check t "parameter value lists split on commas" <|
    ((parseContentLine "X-FOO;P=a,b,c:v").map fun p => (p.params.find? (·.1 == "P")).map (·.2)) == some (some ["a", "b", "c"])
  check t "a line without a colon is not a content line" (parseContentLine "no colon here").isNone
  check t "a URL value keeps its colons" (((parseContentLine "URL:https://example.com/a:b").map (·.value)) == some "https://example.com/a:b")

  -- value types
  let pd (v : String) (ps : Params) (fb : Option String := none) := (runW (parseDateTime v ps fb)).1
  check t "VALUE=DATE is all-day" (pd "20260704" [("VALUE", ["DATE"])] == some (dt "2026-07-04T00:00:00" (date := some "2026-07-04") (allDay := true)))
  check t "an 8-digit value is all-day" ((pd "20260704" []).map (·.allDay) == some true)
  check t "a trailing Z is UTC" (pd "20260310T160000Z" [] == some (dt "2026-03-10T16:00:00" (some (T "2026-03-10T16:00:00Z")) (tzid := some "UTC")))
  let la := pd "20260415T100000" [("TZID", ["America/Los_Angeles"])]
  check t "a TZID value is zoned" (la.map (fun d => (d.utc, d.tzid, d.floating)) == some (some (T "2026-04-15T17:00:00Z"), some "America/Los_Angeles", false))
  check t "a value without zone is floating" (pd "20260415T100000" [] == some (dt "2026-04-15T10:00:00" (some (T "2026-04-15T10:00:00Z")) (floating := true)))
  let fb := pd "20260415T100000" [] (some "Europe/Amsterdam")
  check t "a floating value takes the fallback zone" (fb.map (fun d => (d.utc, d.tzid, d.floating)) == some (some (T "2026-04-15T08:00:00Z"), some "Europe/Amsterdam", false))
  let (bad, ws) := runW do
    let a ← parseDateTime "2026-04-15" []
    let b ← parseDateTime "20261340T100000" []
    let c ← parseDateTime "20260415T250000" []
    return [a, b, c]
  check t "malformed date-times are refused with one warning each" (bad.all (·.isNone) && ws.length == 3)
  check t "a leap second reads as :59" ((pd "20260415T235960Z" []).map (·.loc) == some "2026-04-15T23:59:59")
  check t "durations parse" <|
    parseDuration "P1DT2H" == some { days := 1, hours := 2 } && parseDuration "-PT15M" == some { minutes := 15, sign := -1 } &&
    parseDuration "P2W" == some { weeks := 2 } && (parseDuration "+P0DT0H10M0S").map (·.minutes) == some 10 &&
    parseDuration "P" == none && parseDuration "1H" == none && (parseDuration "PT1H30M").map (·.minutes) == some 30

  -- RRULE
  let r := parseRule?
  check t "RRULE weekly with UNTIL and WKST" <| r "FREQ=WEEKLY;BYDAY=MO,WE;UNTIL=20260131T225959Z;WKST=SU" ==
    some { freq := .weekly, wkst := "SU", until_ := some { utc := some (T "2026-01-31T22:59:59Z"), date := some "2026-01-31" },
           byday := some [⟨0, "MO"⟩, ⟨0, "WE"⟩] }
  check t "RRULE monthly ordinal with INTERVAL" (r "FREQ=MONTHLY;BYDAY=2TU;INTERVAL=3" == some { freq := .monthly, interval := 3, byday := some [⟨2, "TU"⟩] })
  check t "RRULE negative ordinal" ((r "FREQ=YEARLY;BYMONTH=3;BYDAY=-1SU").bind (·.byday) == some [⟨-1, "SU"⟩])
  check t "RRULE negative month day" ((r "FREQ=MONTHLY;BYMONTHDAY=-1,15").bind (·.bymonthday) == some [-1, 15])
  check t "RRULE COUNT" ((r "FREQ=DAILY;COUNT=5;INTERVAL=2").bind (·.count) == some 5)
  check t "RRULE UNTIL as a date" ((r "FREQ=DAILY;UNTIL=20260131").bind (·.until_) == some { utc := none, date := some "2026-01-31" })
  check t "RRULE floating UNTIL is anchored to the DTSTART zone" <|
    (parseRule? "FREQ=DAILY;UNTIL=20260131T235959" (some "America/Los_Angeles")).bind (·.until_) == some { utc := some (T "2026-02-01T07:59:59Z"), date := some "2026-01-31" }
  let uns (s : String) := (r s).map (·.unsupported)
  check t "RRULE reports BYHOUR and BYMINUTE" (uns "FREQ=DAILY;BYHOUR=9,17;BYMINUTE=0" == some ["BYHOUR=9,17", "BYMINUTE=0"])
  check t "RRULE reports a sub-daily FREQ" (uns "FREQ=HOURLY;INTERVAL=3" == some ["FREQ=HOURLY"])
  check t "RRULE reports BYSETPOS=2" (uns "FREQ=MONTHLY;BYDAY=MO;BYSETPOS=2" == some ["BYSETPOS=2"])
  check t "RRULE accepts BYSETPOS=-1" (uns "FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1" == some [])
  check t "RRULE reports BYWEEKNO" (uns "FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO" == some ["BYWEEKNO=20"])
  check t "RRULE reports BYYEARDAY" (uns "FREQ=YEARLY;BYYEARDAY=100" == some ["BYYEARDAY=100"])
  check t "RRULE reports a WEEKLY ordinal" (uns "FREQ=WEEKLY;BYDAY=2MO" == some ["BYDAY ordinal with FREQ=WEEKLY"])
  check t "RRULE reports BYMONTHDAY with WEEKLY" (uns "FREQ=WEEKLY;BYMONTHDAY=1" == some ["BYMONTHDAY with FREQ=WEEKLY"])
  check t "RRULE reports a bad BYDAY" (uns "FREQ=DAILY;BYDAY=XX" == some ["BYDAY=XX"])
  check t "RRULE reports RSCALE and SKIP" (uns "FREQ=DAILY;RSCALE=GREGORIAN;SKIP=FORWARD" == some ["RSCALE=GREGORIAN", "SKIP=FORWARD"])
  check t "RRULE without FREQ is none" ((r "INTERVAL=2").isNone && (r "").isNone)
  check t "RRule.parse reports a missing FREQ" (match Core.RRule.parse "INTERVAL=2" with | .error _ => true | .ok _ => false)

def icalCalendars (t : IO.Ref Tally) : IO Unit := do
  -- Google REQUEST
  let r := parseCalendar (← fixture "google-request.ics")
  let e := r.events.headD default
  check t "Google REQUEST: method, no warnings, VTIMEZONE offset" <|
    r.method == some "REQUEST" && r.warnings.isEmpty && r.vtimezones.get? "America/Los_Angeles" == some (some (-480)) && r.events.length == 1
  check t "Google REQUEST: header fields" <|
    e.v == 1 && e.uid == "7k2q9c1v3b5n8m4x6z0a2s4d@google.com" && e.method == some "REQUEST" && e.status == some "CONFIRMED" &&
    e.sequence == 0 && e.stamp == T "2026-04-01T17:15:12Z" / 1000 && e.summary == "Vault roadmap sync" &&
    e.location == "Room 4B, 2nd floor" && e.conference == some "https://meet.google.com/abc-defg-hij" && e.url.isNone
  check t "Google REQUEST: description is unescaped" <|
    (e.description.splitOn "Agenda:\n- Retention review\n- Calendar import, then Q&A").length > 1 &&
    (e.description.splitOn "Join with Google Meet: https://meet.google.com/abc-defg-hij").length > 1
  check t "Google REQUEST: start, end, duration" <|
    e.start == dt "2026-04-15T10:00:00" (some (T "2026-04-15T17:00:00Z")) (tzid := some "America/Los_Angeles") (tzidRaw := some "America/Los_Angeles") &&
    e.end_.utc == some (T "2026-04-15T17:30:00Z") && e.durationMs == 30 * 60000
  check t "Google REQUEST: organizer and attendees" <|
    e.organizer == some { email := "dana.whitfield@example.com", name := "Dana Whitfield" } &&
    e.attendees == [
      { email := "dana.whitfield@example.com", name := "Dana Whitfield", partstat := "ACCEPTED", role := "REQ-PARTICIPANT", rsvp := false, cutype := "INDIVIDUAL" },
      { email := "alex@vault.example", name := "alex@vault.example", partstat := "NEEDS-ACTION", role := "REQ-PARTICIPANT", rsvp := true, cutype := "INDIVIDUAL" },
      { email := "priya.raman@example.com", name := "Priya Raman", partstat := "TENTATIVE", role := "OPT-PARTICIPANT", rsvp := false, cutype := "INDIVIDUAL" }] &&
    e.rrule.isNone && e.recurrenceId.isNone
  check t "Google REQUEST: the VEVENT block is kept verbatim with VALARM, without VCALENDAR" <|
    e.ics.startsWith "BEGIN:VEVENT\r\n" && e.ics.endsWith "END:VEVENT" && (e.ics.splitOn "BEGIN:VALARM").length > 1 &&
    (e.ics.splitOn "VCALENDAR").length == 1
  -- the matching CANCEL
  let rc := parseCalendar (← fixture "google-cancel.ics")
  let c := rc.events.headD default
  check t "Google CANCEL: method, status, sequence, same uid and summary, empty location" <|
    rc.method == some "CANCEL" && c.method == some "CANCEL" && c.status == some "CANCELLED" && c.sequence == 1 &&
    c.uid == e.uid && c.summary == e.summary && c.location == ""
  checkEq t "Google CANCEL has the REQUEST's row key" (rowKey e.uid none) (rowKey c.uid (c.recurrenceId.map (·.toDateTime)))
  -- Outlook
  let text ← fixture "outlook-request.ics"
  let ro := parseCalendar text
  let o := ro.events.headD default
  check t "Outlook: CRLF input, no warnings, VTIMEZONE offset" <|
    (text.splitOn "\r\n").length > 1 && ro.warnings.isEmpty && ro.vtimezones.get? "Eastern Standard Time" == some (some (-300))
  check t "Outlook: folded uid, summary, sequence" <|
    o.uid == "040000008200E00074C5B7101A82E00800000000A0B1C2D3E4F5DA01000000000000000010000000ABCDEF0123456789ABCDEF0123456789" &&
    o.summary == "Contoso vendor review" && o.sequence == 2
  check t "Outlook: a quoted Windows TZID maps to America/New_York" <|
    o.start.tzid == some "America/New_York" && o.start.tzidRaw == some "Eastern Standard Time" &&
    o.start.utc == some (T "2026-07-21T18:00:00Z") && o.end_.utc == some (T "2026-07-21T19:00:00Z")
  checkEq t "Outlook: description" "Please review the attached deck before the call.\n\nJoin Microsoft Teams Meeting\nhttps://teams.microsoft.com/l/meetup-join/19%3ameeting_abc%40thread.v2/0\n" o.description
  check t "Outlook: attendees and organizer" <|
    o.attendees.map (fun a => (a.email, a.name, a.role)) == [("alex@vault.example", "Alex Vault", "REQ-PARTICIPANT"), ("wei.chen@contoso.example", "Chen, Wei", "OPT-PARTICIPANT")] &&
    o.organizer == some { email := "marcus.bell@contoso.example", name := "Marcus Bell" }
  -- Apple
  let ra := parseCalendar (← fixture "apple-invite.ics")
  let a := ra.events.headD default
  check t "Apple: no warnings, uid, organizer with accents" <|
    ra.warnings.isEmpty && a.uid == "6A1B2C3D-4E5F-4A6B-8C7D-9E0F1A2B3C4D" &&
    a.organizer == some { email := "2_abcdef0123456789@imip.me.example", name := "Élodie Marchand" }
  check t "Apple: attendees" <|
    (a.attendees.headD default).role == "CHAIR" &&
    a.attendees[1]? == some { email := "alex@vault.example", name := "Alex Vault", partstat := "NEEDS-ACTION", role := "REQ-PARTICIPANT", rsvp := true, cutype := "INDIVIDUAL" }
  check t "Apple: escapes, categories, URL, DTEND listed first" <|
    a.description == "Dinner to celebrate the launch. Bring nothing; we have it covered." &&
    a.location == "Le Petit Carré\n12 Rue de Rivoli, 75004 Paris" && a.categories == ["Personal", "Food & drink", "Team, offsite"] &&
    a.url == some "https://www.icloud.com/calendar/invite/abc123" && a.start.utc == some (T "2026-10-03T17:00:00Z") &&
    a.end_.utc == some (T "2026-10-03T18:30:00Z") && a.durationMs == 90 * 60000 && a.status.isNone
  -- all-day
  let ad := parse (← fixture "allday.ics")
  let a0 := ad.headD default
  let a1 := ad[1]?.getD default
  check t "all-day: VALUE=DATE start, two-day end" <|
    a0.start == dt "2026-07-04T00:00:00" (date := some "2026-07-04") (allDay := true) && a0.end_.date == some "2026-07-06" && a0.durationMs == 2 * 86400000
  check t "all-day: a missing DTEND is one day" <|
    a1.start.date == some "2026-12-25" && a1.end_.date == some "2026-12-26" && a1.end_.allDay && a1.durationMs == 86400000
  -- DURATION
  let du := (parse (← fixture "duration.ics")).headD default
  check t "the end derives from DURATION" <|
    du.start.utc == some (T "2026-03-10T16:00:00Z") && du.end_ == dt "2026-03-10T17:30:00" (some (T "2026-03-10T17:30:00Z")) (tzid := some "UTC") &&
    du.durationMs == 90 * 60000 && (du.attendees.headD default).name == ""
  -- recurring master + override
  let ro := parse (← fixture "recurring-override.ics")
  let m := ro.headD default
  let ov := ro[1]?.getD default
  check t "recurring master: RRULE and EXDATE" <|
    ro.length == 2 && m.rruleRaw == some "FREQ=WEEKLY;BYDAY=TH" && m.rrule == some { freq := .weekly, byday := some [⟨0, "TH"⟩] } &&
    m.exdates == [dt "2026-05-21T15:00:00" (some (T "2026-05-21T19:00:00Z")) (tzid := some "America/New_York") (tzidRaw := some "America/New_York")] &&
    m.recurrenceId.isNone
  check t "override: RECURRENCE-ID, start, sequence, summary" <|
    ov.uid == m.uid && ov.recurrenceId == some { toDateTime := dt "2026-05-28T15:00:00" (some (T "2026-05-28T19:00:00Z")) (tzid := some "America/New_York") (tzidRaw := some "America/New_York"), range := none } &&
    ov.start.utc == some (T "2026-05-28T20:00:00Z") && ov.sequence == 1 && ov.summary == "Weekly standup (moved to 4pm)"
  check t "override and master have different row keys" (rowKey m.uid none != rowKey ov.uid (ov.recurrenceId.map (·.toDateTime)))
  let taf := (parse "BEGIN:VCALENDAR\nBEGIN:VEVENT\nUID:x\nDTSTART:20260101T100000Z\nRECURRENCE-ID;RANGE=THISANDFUTURE:20260108T100000Z\nEND:VEVENT\nEND:VCALENDAR").headD default
  check t "THISANDFUTURE stays on RECURRENCE-ID" ((taf.recurrenceId.bind (·.range)) == some "THISANDFUTURE")
  -- RRULE matrix
  let rm := parseCalendar (← fixture "rrule-matrix.ics")
  let u := byUid rm.events
  check t "matrix: daily interval 2 count 5" ((u "daily-interval-2-count-5").rrule == some { freq := .daily, interval := 2, count := some 5 })
  check t "matrix: weekly UNTIL" (((u "weekly-mo-we-until").rrule.bind (·.until_)) == some { utc := some (T "2026-01-31T22:59:59Z"), date := some "2026-01-31" })
  check t "matrix: last day of month" (((u "monthly-last-day").rrule.bind (·.bymonthday)) == some [-1])
  check t "matrix: second Tuesday" (((u "monthly-second-tuesday").rrule.bind (·.byday)) == some [⟨2, "TU"⟩])
  check t "matrix: last Sunday of March" ((u "yearly-last-sunday-march").rrule == some { freq := .yearly, byday := some [⟨-1, "SU"⟩], bymonth := some [3] })
  check t "matrix: last weekday" (((u "monthly-last-weekday").rrule.bind (·.bysetpos)) == some [-1])
  check t "matrix: BYHOUR is unsupported" (((u "unsupported-byhour").rrule.map (·.unsupported)) == some ["BYHOUR=9,17", "BYMINUTE=0"])
  check t "matrix: HOURLY reads as DAILY, unsupported" ((u "unsupported-hourly").rrule == some { freq := .daily, interval := 3, count := some 4, unsupported := ["FREQ=HOURLY"] })
  checkEq t "matrix: warnings" ["rrule-unsupported:BYHOUR=9,17,BYMINUTE=0", "rrule-unsupported:FREQ=HOURLY"] rm.warnings
  check t "matrix: no METHOD" rm.method.isNone
  -- Windows / VTIMEZONE / unknown zones
  let pr := parseCalendar (← fixture "puerto-rico.ics")
  let u := byUid pr.events
  let p := u "pr-weekly-sync-0001@contoso.example"
  check t "a Windows TZID maps to America/Puerto_Rico" <|
    p.start.tzid == some "America/Puerto_Rico" && p.start.tzidRaw == some "SA Western Standard Time" && p.start.utc == some (T "2026-01-05T14:00:00Z")
  let cu := u "custom-tz-0002@contoso.example"
  check t "an unknown TZID falls back to its VTIMEZONE offset" <|
    cu.start.tzid.isNone && !cu.start.floating && cu.start.utc == some (T "2026-02-10T04:00:00Z") && cu.end_.utc == some (T "2026-02-10T04:30:00Z")
  let px := u "prefixed-tz-0003@contoso.example"
  check t "an Outlook display-name TZID resolves" (px.start.tzid == some "America/Chicago" && px.start.utc == some (T "2026-02-10T15:30:00Z"))
  let un := u "unknown-tz-0004@contoso.example"
  check t "an unknown zone without VTIMEZONE is floating" <|
    un.start.floating && un.start.tzid.isNone && un.start.tzidRaw == some "Mars/Olympus_Mons" && un.start.utc == some (T "2026-02-10T09:30:00Z")
  check t "an unknown zone is warned about" (pr.warnings.contains "unknown-tzid:Mars/Olympus_Mons")
  check t "a VTIMEZONE STANDARD offset is recorded" (pr.vtimezones.get? "Customized Time Zone" == some (some 330))
  -- no UID
  let nu ← fixture "no-uid.ics"
  let na := parseCalendar nu
  let ne := na.events.headD default
  check t "a missing UID is synthesised deterministically" <|
    na.warnings == ["no-uid"] && ne.uid.startsWith "mv-" && ne.uid.length == 67 && ne.uid == ((parse nu).headD default).uid && ne.stamp == 0
  check t "a different summary synthesises a different UID" <|
    ((parse (nu.replace "Concert: The Folded Lines" "Other")).headD default).uid != ne.uid
  -- feed
  let gf := parseCalendar (← fixture "google-feed.ics")
  check t "X-WR-CALNAME / X-WR-TIMEZONE are read and other components ignored" <|
    gf.calname == some "Studio bookings" && gf.tzid == some "Europe/Amsterdam" && gf.events.length == 1 &&
    (gf.events.headD default).start == dt "2026-03-02T14:00:00" (some (T "2026-03-02T13:00:00Z")) (tzid := some "Europe/Amsterdam")
  let r2 := parseCalendar "BEGIN:VCALENDAR\nBEGIN:VEVENT\nUID:f\nDTSTART:20260302T140000\nEND:VEVENT\nEND:VCALENDAR" (method := some "reply") (tz := some "Asia/Tokyo")
  check t "opts.tz anchors floating times and opts.method fills a missing METHOD" <|
    r2.method == some "REPLY" && (r2.events.headD default).start.utc == some (T "2026-03-02T05:00:00Z") && (r2.events.headD default).method == some "REPLY"
  -- malformed
  let mf := parseCalendar "BEGIN:VCALENDAR\nMETHOD:REQUEST\nBEGIN:VEVENT\nUID:a\nDTSTART:20260101T100000Z\nDTEND:20260101T090000Z\ngarbage line\nBEGIN:VALARM\nTRIGGER:-PT5M\nEND:VEVENT\nEND:VCALENDAR\nEND:VTODO\n"
  let me := mf.events.headD default
  check t "an end before the start collapses to zero length" (me.durationMs == 0 && me.end_.utc == me.start.utc)
  check t "malformed input is warned about, not fatal" <|
    ["end-before-start", "bad-line:7", "unterminated:VALARM", "unbalanced-end:VTODO"].all mf.warnings.contains
  let ns := parseCalendar "BEGIN:VCALENDAR\nMETHOD:CANCEL\nBEGIN:VEVENT\nUID:b\nDTSTAMP:20260101T000000Z\nSTATUS:CANCELLED\nEND:VEVENT\nEND:VCALENDAR"
  check t "a missing DTSTART falls back to DTSTAMP" <|
    ns.warnings.contains "no-dtstart" && (ns.events.headD default).start.utc == some (T "2026-01-01T00:00:00Z") && (ns.events.headD default).status == some "CANCELLED"
  let pe := parseCalendar "BEGIN:VCALENDAR\nBEGIN:VEVENT\nUID:c\nDTSTART:20260101T100000Z\nRDATE;VALUE=PERIOD:20260108T100000Z/PT2H\nRDATE:20260115T100000Z,20260122T100000Z\nEXDATE;VALUE=DATE:20260115\nEND:VEVENT\nEND:VCALENDAR"
  let pev := pe.events.headD default
  check t "PERIOD RDATEs are skipped, other RDATE/EXDATE values kept" <|
    pe.warnings == ["rdate-period-skipped"] && pev.rdates.map (·.utc) == [some (T "2026-01-15T10:00:00Z"), some (T "2026-01-22T10:00:00Z")] &&
    pev.exdates == [dt "2026-01-15T00:00:00" (date := some "2026-01-15") (allDay := true)]
  let mx := parse "BEGIN:VCALENDAR\nBEGIN:VEVENT\nUID:m1\nDTSTART;TZID=America/Los_Angeles:20260101T100000\nDTEND;VALUE=DATE:20260102\nEND:VEVENT\nBEGIN:VEVENT\nUID:m2\nDTSTART;VALUE=DATE:20260101\nDTEND:20260103T083000Z\nEND:VEVENT\nEND:VCALENDAR"
  let m1 := mx.headD default
  let m2 := mx[1]?.getD default
  check t "a DATE end on a timed start is coerced to the start's zone" <|
    m1.end_ == dt "2026-01-02T00:00:00" (some (T "2026-01-02T08:00:00Z")) (tzid := some "America/Los_Angeles") (tzidRaw := some "America/Los_Angeles") &&
    m1.durationMs == 14 * 3600000
  check t "a DATE-TIME end on an all-day start is coerced to a date" <|
    m2.end_.utc.isNone && m2.end_.date == some "2026-01-03" && m2.end_.allDay && m2.durationMs == 2 * 86400000
  let bare := parseCalendar "BEGIN:VEVENT\nUID:d\nDTSTART;VALUE=DATE:20260101\nEND:VEVENT"
  check t "a bare VEVENT parses with a warning" (bare.events.length == 1 && bare.warnings.contains "no-vcalendar")
  check t "empty input has no events" (parse "").isEmpty
  -- 32 KB cap
  let big := String.ofList (List.replicate 40000 'é')
  let bg := parseCalendar s!"BEGIN:VCALENDAR\nBEGIN:VEVENT\nUID:big\nDTSTART:20260101T100000Z\nDESCRIPTION:{big}\nSUMMARY:big\nEND:VEVENT\nEND:VCALENDAR"
  let be := bg.events.headD default
  check t "the stored VEVENT block is capped at 32 KB on a UTF-8 boundary" <|
    bg.warnings.contains "ics-truncated" && bg.warnings.contains "description-truncated" && be.ics.utf8ByteSize ≤ icsMax &&
    !(be.ics.contains '�') && be.description.length == 16 * 1024
  -- row keys
  checkEq t "row key golden: master" "cUClQsDvn3aD4KW-Y51KhS4G4-AB1nD2kWJkND-OCb0" (rowKey "7k2q9c1v3b5n8m4x6z0a2s4d@google.com" none)
  let timed := dt "2026-05-28T15:00:00" (some (T "2026-05-28T19:00:00Z")) (tzid := some "America/New_York") (tzidRaw := some "America/New_York")
  checkEq t "row key golden: timed override" "W2IYIlHVigy0q43N4lE1eTWZ386KZUf3er_j2y92A_c" (rowKey "weekly-standup-4d2c@google.com" (some timed))
  checkEq t "row key golden: all-day override" "JqWQkrGrmTvl50Biyw8Y2Z0zOjKk70PpjEmL50-Pxio"
    (rowKey "allday-two-days@example.com" (some (dt "2026-07-04T00:00:00" (date := some "2026-07-04") (allDay := true))))
  checkEq t "row key: the same slot in another zone hashes the same" "W2IYIlHVigy0q43N4lE1eTWZ386KZUf3er_j2y92A_c"
    (rowKey "weekly-standup-4d2c@google.com" (some { timed with loc := "2026-05-28T12:00:00", tzid := some "America/Los_Angeles", tzidRaw := some "America/Los_Angeles" }))
  checkEq t "sha256 of the empty string" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" (hex (sha256 ByteArray.empty))
  checkEq t "sha256 of 'abc'" "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" (hex (sha256 "abc".toUTF8))
  -- calendar parts
  let ics ← fixture "duration.ics"
  let parts := findCalendarText [
    { mimeType := some "text/calendar; charset=UTF-8; method=REQUEST", filename := some "invite.ics", content := ("﻿" ++ ics).toUTF8 },
    { mimeType := some "application/ics", method := some "cancel", content := ics.toUTF8 },
    { mimeType := some "application/octet-stream", filename := some "Meeting.ICS", content := ics.toUTF8 },
    { mimeType := some "application/pdf", filename := some "deck.pdf", content := "not a calendar".toUTF8 },
    { mimeType := some "text/calendar", filename := some "huge.ics", content := ByteArray.mk (Array.replicate (calendarPartMax + 1) 0) },
    { mimeType := some "text/calendar", filename := some "empty.ics", content := ByteArray.empty }]
  check t "calendar parts are picked by MIME type or .ics name, BOM stripped, size capped" <|
    parts.map (·.2) == [some "REQUEST", some "CANCEL", none] && (parts.headD ("", none)).1.startsWith "BEGIN:VCALENDAR" &&
    (parts[1]?.map (·.1)) == some ics

/-! ## `rrule.test.ts`, by hand, plus edge cases -/

def LA : Option String := some "America/Los_Angeles"

def zonesUnit (t : IO.Ref Tally) : IO Unit := do
  let rz (s : String) := resolveTzid (some s)
  check t "IANA names in any case, UTC and GMT resolve" <|
    rz "America/Los_Angeles" == { tzid := LA } && (rz "america/los_angeles").tzid == LA && (rz "UTC").tzid == some "UTC" &&
    (rz "GMT").tzid == some "UTC" && (rz "Etc/GMT+5").tzid == some "Etc/GMT+5" && (canonicalTimeZone (some "Not/AZone")).isNone &&
    (canonicalTimeZone (some "")).isNone
  check t "ICU spellings: Asia/Kolkata reads as Asia/Calcutta, US/Eastern as America/New_York" <|
    canonicalTimeZone (some "Asia/Kolkata") == some "Asia/Calcutta" && canonicalTimeZone (some "US/Eastern") == some "America/New_York"
  check t "quotes and Outlook prefixes are stripped" <|
    (rz "\"Eastern Standard Time\"").tzid == some "America/New_York" && (rz "(UTC-05:00) Eastern Standard Time").tzid == some "America/New_York" &&
    (rz "\"(UTC-05:00) Eastern Time (US & Canada)\"").tzid == some "America/New_York" && (rz "(UTC-08:00) America/Los_Angeles").tzid == LA
  let expected : List (String × String) := [
    ("Pacific Standard Time", "America/Los_Angeles"), ("Mountain Standard Time", "America/Denver"),
    ("Central Standard Time", "America/Chicago"), ("Eastern Standard Time", "America/New_York"),
    ("SA Western Standard Time", "America/Puerto_Rico"), ("Atlantic Standard Time", "America/Halifax"),
    ("GMT Standard Time", "Europe/London"), ("W. Europe Standard Time", "Europe/Berlin"), ("Romance Standard Time", "Europe/Paris"),
    ("Central Europe Standard Time", "Europe/Budapest"), ("Central European Standard Time", "Europe/Warsaw"),
    ("E. Europe Standard Time", "Europe/Bucharest"), ("Russian Standard Time", "Europe/Moscow"), ("India Standard Time", "Asia/Kolkata"),
    ("China Standard Time", "Asia/Shanghai"), ("Tokyo Standard Time", "Asia/Tokyo"), ("Singapore Standard Time", "Asia/Singapore"),
    ("AUS Eastern Standard Time", "Australia/Sydney"), ("New Zealand Standard Time", "Pacific/Auckland"),
    ("Hawaiian Standard Time", "Pacific/Honolulu"), ("Alaskan Standard Time", "America/Anchorage"),
    ("US Mountain Standard Time", "America/Phoenix"), ("Israel Standard Time", "Asia/Jerusalem"), ("Arabian Standard Time", "Asia/Dubai"),
    ("UTC", "UTC")]
  for (w, i) in expected do
    check t s!"Windows zone {w} is {i}" (rz w == { tzid := some i })
  check t "every Windows table target is a known zone" (windowsTz.all fun (_, i) => (canonicalTimeZone (some i)).isSome)
  let vtz : VTimezones := [("Customized Time Zone", some 330)]
  check t "path-style ids, spaces, VTIMEZONE fallbacks and prefix offsets" <|
    (rz "/mozilla.org/20050126_1/America/New_York").tzid == some "America/New_York" &&
    (rz "/freeassociation.sourceforge.net/Europe/Berlin").tzid == some "Europe/Berlin" &&
    (rz "America/New York").tzid == some "America/New_York" &&
    resolveTzid (some "Customized Time Zone") (some vtz) == { fixedOffsetMinutes := some 330 } &&
    (resolveTzid (some "\"Customized Time Zone\"") (some vtz)).fixedOffsetMinutes == some 330 &&
    rz "(UTC+03:00) Nairobi" == { fixedOffsetMinutes := some 180 } &&
    rz "(UTC) Coordinated Universal Time" == { tzid := some "UTC" } && rz "(UTC)" == { tzid := some "UTC" } &&
    rz "(UTC-05:00)" == { fixedOffsetMinutes := some (-300) } && rz "UTC+01:00 Amsterdam" == { fixedOffsetMinutes := some 60 } &&
    (rz "(UTC-03:30) Newfoundland").fixedOffsetMinutes == some (-210) && rz "Nowhere" == {} &&
    resolveTzid none == {} && rz "  " == {}
  check t "wall clock to UTC across the Los Angeles DST change" <|
    zonedToUtc "2026-03-02T09:00:00" LA == T "2026-03-02T17:00:00Z" && zonedToUtc "2026-03-09T09:00:00" LA == T "2026-03-09T16:00:00Z" &&
    zonedToUtc "2026-03-08T01:59:59" LA == T "2026-03-08T09:59:59Z" && zonedToUtc "2026-03-08T03:00:00" LA == T "2026-03-08T10:00:00Z" &&
    zonedToUtc "2026-03-08T02:30:00" LA == T "2026-03-08T10:30:00Z" && zonedToUtc "2026-11-01T01:30:00" LA == T "2026-11-01T08:30:00Z" &&
    zonedToUtc "2026-11-01T02:00:00" LA == T "2026-11-01T10:00:00Z" &&
    zonedToUtc "2026-01-15T09:00:00" (some "Australia/Sydney") == T "2026-01-14T22:00:00Z" &&
    zonedToUtc "2026-01-15T09:00:00" none == T "2026-01-15T09:00:00Z" && zonedToUtc "2026-01-15T09:00:00" (some "UTC") == T "2026-01-15T09:00:00Z" &&
    zonedToUtc "2026-01-15" (some "UTC") == T "2026-01-15T00:00:00Z"
  check t "instants format as wall clock" <|
    utcToZoned (T "2026-03-09T16:00:00Z") LA == "2026-03-09T09:00:00" && utcToZoned (T "2026-01-15T00:30:00Z") (some "Asia/Kolkata") == "2026-01-15T06:00:00" &&
    utcToZoned (T "2026-01-15T00:30:00Z") none == "2026-01-15T00:30:00" && utcToZoned (T "2026-07-01T07:00:00Z") LA == "2026-07-01T00:00:00"
  let zs := [LA, some "Europe/Paris", some "Asia/Kolkata", some "Australia/Adelaide", some "Pacific/Auckland", some "America/Puerto_Rico", some "UTC"]
  let ss := ["2026-01-15T12:34:56Z", "2026-03-08T10:30:00Z", "2026-07-04T23:59:59Z", "2026-11-01T10:30:00Z", "2026-12-31T23:00:00Z"]
  check t "wall clock round-trips in seven zones" (zs.all fun tz => ss.all fun s => zonedToUtc (utcToZoned (T s) tz) tz == T s)
  check t "offsets are exact on a transition day" <|
    tzOffsetMinutes (T "2026-03-08T09:59:00Z") "America/Los_Angeles" == -480 && tzOffsetMinutes (T "2026-03-08T10:01:00Z") "America/Los_Angeles" == -420 &&
    tzOffsetMinutes (T "2026-03-08T00:00:00Z") "America/Los_Angeles" == -480 && tzOffsetMinutes (T "2026-03-08T23:59:59Z") "America/Los_Angeles" == -420 &&
    tzOffsetMinutes (T "2026-11-01T08:30:00Z") "America/Los_Angeles" == -420 && tzOffsetMinutes (T "2026-11-01T09:30:00Z") "America/Los_Angeles" == -480 &&
    tzOffsetMinutes (T "2026-06-01T00:00:00Z") "Asia/Kolkata" == 330 && tzOffsetMinutes (T "2026-01-01T00:00:00Z") "Australia/Adelaide" == 630 &&
    tzOffsetMinutes (T "2026-01-01T00:00:00Z") "America/Puerto_Rico" == -240 && tzOffsetMinutes (T "2026-07-01T00:00:00Z") "America/Puerto_Rico" == -240 &&
    tzOffsetMinutes (T "2026-07-01T00:00:00Z") "UTC" == 0

def ex (e : Event) (ovs : List Event) (a b : String) (tz : Option String := none) (maxI : Nat := 2000) : List Occurrence :=
  expandOccurrences e ovs (T a) (T b) tz maxI

def expandUnit (t : IO.Ref Tally) : IO Unit := do
  let dst := master (← fixture "dst-la.ics")
  let occ := ex dst [] "2026-02-01T00:00:00Z" "2026-03-31T00:00:00Z"
  checkEq t "a 9:00 weekly meeting stays at 9:00 local across DST"
    ["2026-02-23T17:00:00.000Z", "2026-03-02T17:00:00.000Z", "2026-03-09T16:00:00.000Z", "2026-03-16T16:00:00.000Z", "2026-03-23T16:00:00.000Z", "2026-03-30T16:00:00.000Z"] (starts occ)
  check t "its instances are 30 minutes, plain, at 09:00 LA" <|
    occ.all fun o => (utcToZoned o.start LA).drop 11 == "09:00:00" && o.end_ - o.start == 30 * 60000 && !o.allDay && !o.isOverride && !o.cancelled && !o.complex && o.uid == dst.uid
  checkEq t "date keys follow the event zone" ["2026-02-23", "2026-03-02", "2026-03-09", "2026-03-16", "2026-03-23", "2026-03-30"] (occ.map (·.dateKey))
  check t "the slot of an instance is its RECURRENCE-ID" <|
    (occ[2]?.bind (·.recurrenceId)) == some (dt "2026-03-09T09:00:00" (some (T "2026-03-09T16:00:00Z")) (tzid := LA) (tzidRaw := LA))
  checkEq t "date keys follow opts.tz" "2026-03-10" ((ex dst [] "2026-03-09T00:00:00Z" "2026-03-10T00:00:00Z" (some "Asia/Tokyo")).headD default).dateKey
  let pr := master (← fixture "puerto-rico.ics")
  let po := ex pr [] "2026-01-01T00:00:00Z" "2026-08-01T00:00:00Z"
  check t "Puerto Rico keeps one UTC time all year" <|
    po.length == 30 && po.all (fun o => (iso o.start).drop 11 == "14:00:00.000Z") && iso (po.headD default).start == "2026-01-05T14:00:00.000Z" &&
    (po[29]?.map (iso ·.start)) == some "2026-07-27T14:00:00.000Z"
  let rm := byUid (parse (← fixture "rrule-matrix.ics"))
  let mx (uid : String) (a := "2026-01-01T00:00:00Z") (b := "2027-06-01T00:00:00Z") := starts (ex (rm uid) [] a b)
  checkEq t "matrix: daily interval 2 count 5" ["2026-01-01T09:00:00.000Z", "2026-01-03T09:00:00.000Z", "2026-01-05T09:00:00.000Z", "2026-01-07T09:00:00.000Z", "2026-01-09T09:00:00.000Z"] (mx "daily-interval-2-count-5")
  checkEq t "matrix: weekly MO,WE until the end of January" ["2026-01-05T07:00:00.000Z", "2026-01-07T07:00:00.000Z", "2026-01-12T07:00:00.000Z", "2026-01-14T07:00:00.000Z", "2026-01-19T07:00:00.000Z", "2026-01-21T07:00:00.000Z", "2026-01-26T07:00:00.000Z", "2026-01-28T07:00:00.000Z"] (mx "weekly-mo-we-until")
  checkEq t "matrix: last day of the month" ["2026-01-31", "2026-02-28", "2026-03-31", "2026-04-30", "2026-05-31", "2026-06-30"] (mx "monthly-last-day" (b := "2026-07-01T00:00:00Z"))
  checkEq t "matrix: second Tuesday at noon Chicago" ["2026-01-13T18:00:00.000Z", "2026-02-10T18:00:00.000Z", "2026-03-10T17:00:00.000Z", "2026-04-14T17:00:00.000Z"] (mx "monthly-second-tuesday" (b := "2026-05-01T00:00:00Z"))
  checkEq t "matrix: last Sunday of March" ["2026-03-29", "2027-03-28", "2028-03-26"] (mx "yearly-last-sunday-march" (b := "2029-01-01T00:00:00Z"))
  checkEq t "matrix: last working day" ["2026-01-30", "2026-02-27", "2026-03-31", "2026-04-30", "2026-05-29", "2026-06-30"] (mx "monthly-last-weekday" (b := "2026-07-01T00:00:00Z"))
  for uid in ["unsupported-byhour", "unsupported-hourly"] do
    let o := ex (rm uid) [] "2026-01-01T00:00:00Z" "2027-06-01T00:00:00Z"
    check t s!"matrix: {uid} degrades to its first instance" <|
      o.length == 1 && (o.headD default).complex && iso (o.headD default).start == "2026-01-01T09:00:00.000Z" &&
      (ex (rm uid) [] "2026-02-01T00:00:00Z" "2027-06-01T00:00:00Z").isEmpty
  let (rmaster, rovs) := load (← fixture "recurring-override.ics")
  let ro := ex rmaster rovs "2026-05-01T00:00:00Z" "2026-06-20T00:00:00Z"
  checkEq t "EXDATE removes a slot and an override replaces another"
    ["2026-05-07T19:00:00.000Z", "2026-05-14T19:00:00.000Z", "2026-05-28T20:00:00.000Z", "2026-06-04T19:00:00.000Z", "2026-06-11T19:00:00.000Z", "2026-06-18T19:00:00.000Z"] (starts ro)
  let moved := ro[2]?.getD default
  check t "the moved instance reports its original slot" <|
    moved.isOverride && (moved.recurrenceId.map (·.loc)) == some "2026-05-28T15:00:00" && moved.end_ == T "2026-05-28T20:45:00Z" && moved.dateKey == "2026-05-28"
  let alone := ex (rovs.headD default) [] "2026-05-01T00:00:00Z" "2026-06-20T00:00:00Z"
  check t "an override handed in as the record is a single occurrence" (alone.length == 1 && (alone.headD default).isOverride)
  checkEq t "overrides for another uid are ignored" ["2026-05-28T19:00:00.000Z"]
    (starts (ex rmaster [{ rovs.headD default with uid := "other" }] "2026-05-27T00:00:00Z" "2026-05-29T00:00:00Z"))
  let cancel := parse "BEGIN:VCALENDAR\nMETHOD:CANCEL\nBEGIN:VEVENT\nUID:weekly-standup-4d2c@google.com\nRECURRENCE-ID;TZID=America/New_York:20260514T150000\nDTSTART;TZID=America/New_York:20260514T150000\nDTEND;TZID=America/New_York:20260514T154500\nSTATUS:CANCELLED\nSEQUENCE:2\nEND:VEVENT\nEND:VCALENDAR"
  checkEq t "a cancelled override removes its slot" ["2026-05-07T19:00:00.000Z", "2026-05-28T19:00:00.000Z"] (starts (ex rmaster cancel "2026-05-01T00:00:00Z" "2026-05-31T00:00:00Z"))
  let series := parse ((← fixture "dst-la.ics").replace "METHOD:REQUEST" "METHOD:CANCEL")
  let co := ex (series.headD default) [] "2026-02-01T00:00:00Z" "2026-03-10T00:00:00Z"
  check t "a cancelled series flags every occurrence" (co.length == 3 && co.all (·.cancelled))
  let taf := parse "BEGIN:VCALENDAR\nBEGIN:VEVENT\nUID:dst-la-monday-9am@google.com\nRECURRENCE-ID;RANGE=THISANDFUTURE;TZID=America/Los_Angeles:20260309T090000\nDTSTART;TZID=America/Los_Angeles:20260309T100000\nDTEND;TZID=America/Los_Angeles:20260309T103000\nEND:VEVENT\nEND:VCALENDAR"
  let to := ex dst taf "2026-02-01T00:00:00Z" "2026-04-01T00:00:00Z"
  check t "a THISANDFUTURE override degrades to the first instance" (starts to == ["2026-02-23T17:00:00.000Z"] && (to.headD default).complex)
  let mo := master "UID:m\nDTSTART:20260101T100000Z\nDTEND:20260101T110000Z\nRRULE:FREQ=MONTHLY"
  let movedIn := (parse "BEGIN:VCALENDAR\nBEGIN:VEVENT\nUID:m\nRECURRENCE-ID:20260701T100000Z\nDTSTART:20260615T100000Z\nDTEND:20260615T110000Z\nEND:VEVENT\nEND:VCALENDAR").headD default
  let mi := ex mo [movedIn] "2026-06-10T00:00:00Z" "2026-06-20T00:00:00Z"
  check t "an override moved into the range shows" <|
    starts mi == ["2026-06-15T10:00:00.000Z"] && (mi.headD default).isOverride && ((mi.headD default).recurrenceId.map (·.loc)) == some "2026-07-01T10:00:00"
  check t "the slot it left is gone" (ex mo [movedIn] "2026-06-25T00:00:00Z" "2026-07-05T00:00:00Z").isEmpty
  let daily := master "UID:d\nDTSTART:20260601T100000Z\nDTEND:20260601T103000Z\nRRULE:FREQ=DAILY"
  let movedOut := (parse "BEGIN:VCALENDAR\nBEGIN:VEVENT\nUID:d\nRECURRENCE-ID:20260612T100000Z\nDTSTART:20260801T100000Z\nDTEND:20260801T103000Z\nEND:VEVENT\nEND:VCALENDAR").headD default
  checkEq t "an override moved out of the range hides its slot" ["2026-06-10T10:00:00.000Z", "2026-06-11T10:00:00.000Z", "2026-06-13T10:00:00.000Z", "2026-06-14T10:00:00.000Z"]
    (starts (ex daily [movedOut] "2026-06-10T00:00:00Z" "2026-06-15T00:00:00Z"))
  let fl := (parse "BEGIN:VCALENDAR\nBEGIN:VEVENT\nUID:dst-la-monday-9am@google.com\nRECURRENCE-ID:20260309T090000\nDTSTART;TZID=America/Los_Angeles:20260309T110000\nDTEND;TZID=America/Los_Angeles:20260309T113000\nSUMMARY:moved\nEND:VEVENT\nEND:VCALENDAR").headD default
  let fo := ex dst [fl] "2026-03-08T00:00:00Z" "2026-03-10T00:00:00Z"
  check t "a floating RECURRENCE-ID matches a zoned master" (starts fo == ["2026-03-09T18:00:00.000Z"] && (fo.headD default).isOverride)
  let rd := master "UID:r\nDTSTART:20260101T100000Z\nDTEND:20260101T110000Z\nRRULE:FREQ=WEEKLY;COUNT=2\nRDATE:20260103T100000Z,20260108T100000Z\nRDATE;VALUE=DATE:20251230"
  checkEq t "RDATEs merge and dedupe" ["2025-12-30T10:00:00.000Z", "2026-01-01T10:00:00.000Z", "2026-01-03T10:00:00.000Z", "2026-01-08T10:00:00.000Z"] (starts (ex rd [] "2025-12-01T00:00:00Z" "2026-02-01T00:00:00Z"))
  let ad := master "UID:a\nDTSTART;VALUE=DATE:20260101\nRRULE:FREQ=MONTHLY;COUNT=4\nEXDATE;VALUE=DATE:20260201\nEXDATE:20260301T000000Z"
  let ao := ex ad [] "2026-01-01T00:00:00Z" "2027-01-01T00:00:00Z"
  check t "all-day COUNT with EXDATEs (date and date-time) counts the excluded slots" <|
    starts ao == ["2026-01-01", "2026-04-01"] && (ao.headD default).start == T "2026-01-01T00:00:00Z" && (ao.headD default).end_ == T "2026-01-02T00:00:00Z" &&
    (ao.headD default).allDay && (ao.headD default).recurrenceId == some (dt "2026-01-01T00:00:00" (date := some "2026-01-01") (allDay := true))
  check t "an RDATE alone makes a record recurring" <|
    isRecurring rd && isRecurring (master "UID:s\nDTSTART:20260101T100000Z\nRDATE:20260105T100000Z") && !isRecurring (master "UID:n\nDTSTART:20260101T100000Z")
  let old := master "UID:old\nDTSTART:20000101T090000Z\nDTEND:20000101T093000Z\nRRULE:FREQ=DAILY"
  let oo := ex old [] "2026-06-01T00:00:00Z" "2026-06-08T00:00:00Z"
  check t "unbounded rules fast-forward" (oo.length == 7 && iso (oo.headD default).start == "2026-06-01T09:00:00.000Z")
  let ow := master "UID:oldw\nDTSTART;TZID=America/Los_Angeles:20000103T090000\nDTEND;TZID=America/Los_Angeles:20000103T093000\nRRULE:FREQ=WEEKLY;INTERVAL=2;BYDAY=MO,FR"
  checkEq t "a biweekly rule from 2000 keeps its phase" ["2026-06-01T16:00:00.000Z", "2026-06-05T16:00:00.000Z", "2026-06-15T16:00:00.000Z", "2026-06-19T16:00:00.000Z"] (starts (ex ow [] "2026-06-01T00:00:00Z" "2026-06-21T00:00:00Z"))
  let om := master "UID:oldm\nDTSTART;VALUE=DATE:20000131\nRRULE:FREQ=MONTHLY;INTERVAL=5;BYMONTHDAY=-1"
  checkEq t "a five-monthly rule from 2000 keeps its phase" ["2026-04-30", "2026-09-30"] (starts (ex om [] "2026-01-01T00:00:00Z" "2027-01-01T00:00:00Z"))
  let cnt := master "UID:c\nDTSTART:20260101T090000Z\nDTEND:20260101T093000Z\nRRULE:FREQ=DAILY;COUNT=10"
  checkEq t "COUNT counts from DTSTART" ["2026-01-08T09:00:00.000Z", "2026-01-09T09:00:00.000Z", "2026-01-10T09:00:00.000Z"] (starts (ex cnt [] "2026-01-08T00:00:00Z" "2026-01-31T00:00:00Z"))
  let cap := master "UID:cap\nDTSTART:20260101T090000Z\nDTEND:20260101T093000Z\nRRULE:FREQ=DAILY"
  check t "maxInstances caps the walk" ((ex cap [] "2026-01-01T00:00:00Z" "2027-01-01T00:00:00Z" (maxI := 10)).length == 10 && (ex cap [] "2026-01-01T00:00:00Z" "2027-01-01T00:00:00Z").length == 365)
  let never := master "UID:never\nDTSTART;VALUE=DATE:20260130\nRRULE:FREQ=MONTHLY;BYMONTH=2;BYMONTHDAY=30"
  check t "a rule that produces nothing does not loop" <|
    starts (ex never [] "2020-01-01T00:00:00Z" "2040-01-01T00:00:00Z") == ["2026-01-30"] &&
    (lastOccurrenceEnd { never with rrule := never.rrule.map ({ · with count := some 3 }) }).isNone
  let e := master "UID:edge\nDTSTART:20260101T100000Z\nDTEND:20260101T110000Z"
  check t "range edges: partial overlap counts, the end is exclusive" <|
    (ex e [] "2026-01-01T10:30:00Z" "2026-01-02T00:00:00Z").length == 1 && (ex e [] "2026-01-01T11:00:00Z" "2026-01-02T00:00:00Z").isEmpty &&
    (ex e [] "2026-01-01T00:00:00Z" "2026-01-01T10:00:00Z").isEmpty
  let z := master "UID:zero\nDTSTART:20260101T100000Z"
  check t "a zero-length event at the range start counts" <|
    z.durationMs == 0 && (ex z [] "2026-01-01T10:00:00Z" "2026-01-02T00:00:00Z").length == 1 && (ex z [] "2026-01-01T10:00:01Z" "2026-01-02T00:00:00Z").isEmpty
  let two := master "UID:two\nDTSTART;VALUE=DATE:20260704\nDTEND;VALUE=DATE:20260706"
  let tw := ex two [] "2026-07-05T12:00:00Z" "2026-07-06T00:00:00Z"
  check t "a two-day all-day event overlaps its second day" <|
    tw.length == 1 && (let o := tw.headD default; o.start == T "2026-07-04T00:00:00Z" && o.end_ == T "2026-07-06T00:00:00Z" && o.allDay && o.dateKey == "2026-07-04" && o.recurrenceId.isNone && !o.isOverride)
  let flt := master "UID:fl\nDTSTART:20260105T090000\nDTEND:20260105T093000\nRRULE:FREQ=WEEKLY;COUNT=3"
  check t "floating events recur in their own wall clock" <|
    flt.start.floating && starts (ex flt [] "2026-01-01T00:00:00Z" "2026-02-01T00:00:00Z") == ["2026-01-05T09:00:00.000Z", "2026-01-12T09:00:00.000Z", "2026-01-19T09:00:00.000Z"]
  let fx := (parse "BEGIN:VCALENDAR\nBEGIN:VTIMEZONE\nTZID:Customized Time Zone\nBEGIN:STANDARD\nDTSTART:16010101T000000\nTZOFFSETFROM:+0530\nTZOFFSETTO:+0530\nEND:STANDARD\nEND:VTIMEZONE\nBEGIN:VEVENT\nUID:fx\nDTSTART;TZID=\"Customized Time Zone\":20260105T090000\nDTEND;TZID=\"Customized Time Zone\":20260105T093000\nRRULE:FREQ=WEEKLY;COUNT=3\nEXDATE;TZID=\"Customized Time Zone\":20260112T090000\nEND:VEVENT\nEND:VCALENDAR").headD default
  check t "fixed-offset events recur in their own wall clock" <|
    fx.start.tzid.isNone && !fx.start.floating && starts (ex fx [] "2026-01-01T00:00:00Z" "2026-02-01T00:00:00Z") == ["2026-01-05T03:30:00.000Z", "2026-01-19T03:30:00.000Z"]
  let d10 (l : List String) := l.map (·.take 10 |>.toString)
  checkEq t "RFC 5545 WKST=MO example" ["1997-08-05", "1997-08-10", "1997-08-19", "1997-08-24"]
    (d10 (starts (ex (master "UID:w1\nDTSTART;TZID=America/New_York:19970805T090000\nRRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=MO") [] "1997-01-01T00:00:00Z" "1998-01-01T00:00:00Z")))
  checkEq t "RFC 5545 WKST=SU example" ["1997-08-05", "1997-08-17", "1997-08-19", "1997-08-31"]
    (d10 (starts (ex (master "UID:w2\nDTSTART;TZID=America/New_York:19970805T090000\nRRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=SU") [] "1997-01-01T00:00:00Z" "1998-01-01T00:00:00Z")))
  checkEq t "yearly 20th Monday" ["1997-05-19", "1998-05-18", "1999-05-17"]
    (starts (ex (master "UID:y\nDTSTART;VALUE=DATE:19970519\nRRULE:FREQ=YEARLY;BYDAY=20MO;COUNT=3") [] "1997-01-01T00:00:00Z" "2000-01-01T00:00:00Z"))
  checkEq t "monthly on the 31st skips short months (all-day DTSTART)" ["2026-01-31", "2026-03-31", "2026-05-31", "2026-07-31"]
    (starts (ex (master "UID:d31\nDTSTART;VALUE=DATE:20260131\nRRULE:FREQ=MONTHLY;COUNT=4") [] "2026-01-01T00:00:00Z" "2027-01-01T00:00:00Z"))
  checkEq t "yearly leap day" ["2024-02-29", "2028-02-29", "2032-02-29"]
    (starts (ex (master "UID:leap\nDTSTART;VALUE=DATE:20240229\nRRULE:FREQ=YEARLY;COUNT=3") [] "2024-01-01T00:00:00Z" "2040-01-01T00:00:00Z"))
  checkEq t "Friday the 13th" ["2026-02-13", "2026-03-13", "2026-11-13"]
    (starts (ex (master "UID:f13\nDTSTART;VALUE=DATE:20260213\nRRULE:FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13") [] "2026-01-01T00:00:00Z" "2027-01-01T00:00:00Z"))
  checkEq t "UNTIL as a date is inclusive" ["2026-01-01T09:00:00.000Z", "2026-01-02T09:00:00.000Z", "2026-01-03T09:00:00.000Z"]
    (starts (ex (master "UID:ud\nDTSTART:20260101T090000Z\nRRULE:FREQ=DAILY;UNTIL=20260103") [] "2026-01-01T00:00:00Z" "2026-02-01T00:00:00Z"))
  checkEq t "UNTIL with Z on an instance is inclusive" 5
    (ex (master "UID:ue\nDTSTART:20260101T090000Z\nRRULE:FREQ=DAILY;UNTIL=20260105T090000Z") [] "2026-01-01T00:00:00Z" "2026-02-01T00:00:00Z").length
  -- edge cases whose expectations were computed with shared/rrule.js
  let lf := master "UID:lf\nDTSTART;TZID=Europe/London:20260130T170000\nDTEND;TZID=Europe/London:20260130T180000\nRRULE:FREQ=MONTHLY;BYDAY=FR;BYSETPOS=-1;COUNT=6"
  checkEq t "BYSETPOS=-1: the last Friday, 17:00 London across BST"
    ["2026-01-30T17:00:00.000Z", "2026-02-27T17:00:00.000Z", "2026-03-27T17:00:00.000Z", "2026-04-24T16:00:00.000Z", "2026-05-29T16:00:00.000Z", "2026-06-26T16:00:00.000Z"]
    (starts (ex lf [] "2026-01-01T00:00:00Z" "2027-01-01T00:00:00Z"))
  checkEq t "BYSETPOS=-1: last end" (some (T "2026-06-26T17:00:00Z")) (lastOccurrenceEnd lf)
  let cx := master "UID:cx\nDTSTART;VALUE=DATE:20260105\nRRULE:FREQ=WEEKLY;BYDAY=MO,WE;COUNT=5\nEXDATE;VALUE=DATE:20260107"
  checkEq t "COUNT with EXDATE: the excluded slot still counts" ["2026-01-05", "2026-01-12", "2026-01-14", "2026-01-19"] (starts (ex cx [] "2026-01-01T00:00:00Z" "2026-03-01T00:00:00Z"))
  checkEq t "COUNT with EXDATE: last end ignores EXDATEs, as the JS does" (some (T "2026-01-20T00:00:00Z")) (lastOccurrenceEnd cx)

def expiryUnit (t : IO.Ref Tally) : IO Unit := do
  let gr := master (← fixture "google-request.ics")
  let ad := master (← fixture "allday.ics")
  check t "firstStart and ridKey" <|
    firstStart gr == T "2026-04-15T17:00:00Z" && firstStart ad == T "2026-07-04T00:00:00Z" && ridKey none == "" &&
    ridKey (some gr.start) == "T" ++ toString (T "2026-04-15T17:00:00Z" / 1000) && ridKey (some ad.start) == "D20260704"
  let rm := byUid (parse (← fixture "rrule-matrix.ics"))
  checkEq t "lastOccurrenceEnd: single" (some (T "2026-04-15T17:30:00Z")) (lastOccurrenceEnd gr)
  checkEq t "lastOccurrenceEnd: all-day" (some (T "2026-07-06T00:00:00Z")) (lastOccurrenceEnd ad)
  checkEq t "lastOccurrenceEnd: COUNT" (some (T "2026-01-09T09:30:00Z")) (lastOccurrenceEnd (rm "daily-interval-2-count-5"))
  checkEq t "lastOccurrenceEnd: UNTIL" (some (T "2026-01-28T08:00:00Z")) (lastOccurrenceEnd (rm "weekly-mo-we-until"))
  checkEq t "lastOccurrenceEnd: unbounded" none (lastOccurrenceEnd (rm "monthly-last-day"))
  checkEq t "lastOccurrenceEnd: complex without UNTIL" none (lastOccurrenceEnd (rm "unsupported-hourly"))
  checkEq t "lastOccurrenceEnd: beyond the horizon" none (lastOccurrenceEnd (rm "weekly-mo-we-until") (horizon := some (T "2026-01-10T00:00:00Z")))
  checkEq t "lastOccurrenceEnd: within the horizon" (some (T "2026-01-28T08:00:00Z")) (lastOccurrenceEnd (rm "weekly-mo-we-until") (horizon := some (T "2026-03-01T00:00:00Z")))
  let hundred := master "UID:h\nDTSTART:20260101T090000Z\nDTEND:20260101T093000Z\nRRULE:FREQ=DAILY;COUNT=100"
  checkEq t "lastOccurrenceEnd: capped" none (lastOccurrenceEnd hundred (maxInstances := 10))
  checkEq t "lastOccurrenceEnd: COUNT=100" (some (T "2026-04-10T09:30:00Z")) (lastOccurrenceEnd hundred)
  checkEq t "lastOccurrenceEnd: with an RDATE" (some (T "2026-03-01T09:30:00Z"))
    (lastOccurrenceEnd (master "UID:rd\nDTSTART:20260101T090000Z\nDTEND:20260101T093000Z\nRRULE:FREQ=DAILY;COUNT=2\nRDATE:20260301T090000Z"))
  checkEq t "lastOccurrenceEnd: complex with UNTIL" (some (T "2026-01-31T23:59:59Z" + 3600000))
    (lastOccurrenceEnd (master "UID:cu\nDTSTART:20260101T090000Z\nDTEND:20260101T100000Z\nRRULE:FREQ=DAILY;BYHOUR=9,17;UNTIL=20260131T235959Z"))
  checkEq t "lastOccurrenceEnd: an override" (some (T "2026-05-28T20:45:00Z")) (lastOccurrenceEnd ((load (← fixture "recurring-override.ics")).2.headD default))
  let rc := T "2026-01-01T00:00:00Z"
  checkEq t "expiry: unbounded keeps two years" (rc + 730 * dayMs) (computeEventExpiry (rm "monthly-last-day") rc)
  checkEq t "expiry: complex keeps two years" (rc + 730 * dayMs) (computeEventExpiry (rm "unsupported-byhour") rc)
  checkEq t "expiry: at least 90 days" (rc + 90 * dayMs) (computeEventExpiry (rm "daily-interval-2-count-5") rc)
  let later := master (← fixture "apple-invite.ics")
  checkEq t "expiry: 30 days after the end" (T "2026-10-03T18:30:00Z" + 30 * dayMs) (computeEventExpiry later rc)
  checkEq t "expiry: 90 days from a late receipt" (T "2026-10-01T00:00:00Z" + 90 * dayMs) (computeEventExpiry later (T "2026-10-01T00:00:00Z"))

def roundTrip (t : IO.Ref Tally) : IO Unit := do
  for s in ["FREQ=WEEKLY;BYDAY=MO,WE;UNTIL=20260131T225959Z;WKST=SU", "FREQ=MONTHLY;INTERVAL=3;BYDAY=2TU",
            "FREQ=YEARLY;BYMONTH=3;BYDAY=-1SU", "FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1;COUNT=7",
            "FREQ=DAILY;COUNT=5;INTERVAL=2", "FREQ=DAILY;UNTIL=20260131", "FREQ=MONTHLY;BYMONTHDAY=-1,15",
            "FREQ=YEARLY;BYMONTH=4,10;BYMONTHDAY=10;UNTIL=20281231T000000Z", "FREQ=DAILY;BYHOUR=9,17;BYMINUTE=0",
            "FREQ=HOURLY;INTERVAL=3", "FREQ=MONTHLY;BYDAY=MO;BYSETPOS=2", "FREQ=WEEKLY;BYDAY=2MO", "FREQ=WEEKLY;BYMONTHDAY=1"] do
    match Core.RRule.parse s with
    | .error e => check t s!"RRULE round-trips {s}" false e
    | .ok r =>
      let back := Core.RRule.parse (toString r)
      check t s!"RRULE round-trips {s}" (match back with | .ok r' => r' == r | .error _ => false) s!"wrote {toString r}"

def main : IO UInt32 := do
  let t ← IO.mkRef ({} : Tally)
  match Json.parse (← IO.FS.readFile "test/fixtures/rrule-vectors.json") with
  | .error e => check t "rrule-vectors.json parses" false e
  | .ok v =>
    zoneVectors t v
    parseVectors t v
    expandVectors t v
  icalUnit t
  icalCalendars t
  zonesUnit t
  expandUnit t
  expiryUnit t
  roundTrip t
  let r ← t.get
  IO.println s!"{r.passed} passed, {r.failed} failed"
  return if r.failed == 0 then 0 else 1
