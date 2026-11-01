import Core.Mime.Date
import Core.Text.Str

/-! # Time zones and recurrence (port of MailVault `shared/rrule.js`, plus `parseRRule`)

Model, as in the JavaScript: a recurring event repeats in *wall-clock* time in its own zone, so
instances are generated as local dates and each one is converted to UTC separately. All-day
events recur in calendar dates. Supported RRULE surface: FREQ=DAILY|WEEKLY|MONTHLY|YEARLY,
INTERVAL, COUNT, UNTIL, BYDAY (ordinals for MONTHLY/YEARLY), BYMONTHDAY (negative allowed),
BYMONTH, WKST and BYSETPOS=1|-1. Everything else lands in `RRule.unsupported` (the rule is then
`isComplex`) and expansion degrades to the first instance flagged `complex`.

Time zones: the JavaScript asks `Intl.DateTimeFormat` (ICU). Lean has no zone database, so this
module carries one as data: each zone is a list of eras (standard offset plus an optional DST rule
written the way tzdata writes them: month, `lastSun` / `Sun>=8`, time, wall/standard/UTC). The
current rules of every zone in the table are exact; history is only kept where it was needed
(US rules before 2007 back to 1987, Indianapolis before 2006, Mexico before Oct 2022, Cairo before
2023, Santiago's 2022 exception). Names are canonicalised the way V8's ICU does it (`Asia/Kolkata` → `Asia/Calcutta`,
`US/Eastern` → `America/New_York`, any case). An IANA name missing from the table is unknown here
where the JavaScript would resolve it; `resolveTzid` then falls back exactly as the JavaScript does
for an unknown name (VTIMEZONE offset, `(UTC±hh:mm)` prefix, else floating). Epoch values are `Int`
milliseconds throughout. -/

namespace Core.RRule

open Core.Text

def dayMs : Int := 86400000
def minuteMs : Int := 60000
/-- Consecutive periods (days/weeks/months/years) examined before a walk gives up. -/
def periodCap : Nat := 100000

/-! ## Small date helpers (proleptic Gregorian, UTC arithmetic only) -/

def pad2 (n : Int) : String := if n < 10 then "0" ++ toString n else toString n

def pad4 (y : Int) : String :=
  let a := y.natAbs
  let s := toString a
  let s := if a < 1000 then String.ofList (List.replicate (4 - s.length) '0') ++ s else s
  if y < 0 then "-" ++ s else s

/-- Days since 1970-01-01, normalising month and day overflow like `Date.setUTCFullYear`. -/
def dayNum (y m d : Int) : Int :=
  let mm := m - 1
  let y' := y + mm / 12
  let m' := mm % 12 + 1
  Core.Mime.Date.daysFromCivil y' m'.toNat 1 + d - 1

def utcMs (y m d : Int) (h mi s : Int := 0) : Int :=
  dayNum y m d * dayMs + h * 3600000 + mi * minuteMs + s * 1000

/-- (year, month, day) of a day number. -/
def dayParts (day : Int) : Int × Int × Int :=
  let (y, m, d) := Core.Mime.Date.civilFromDays day
  (y, m, d)

/-- JS weekday (Sunday = 0) of a day number. -/
def weekday (day : Int) : Int := (day + 4) % 7

def isoDate (day : Int) : String :=
  let (y, m, d) := dayParts day
  pad4 y ++ "-" ++ pad2 m ++ "-" ++ pad2 d

def daysInMonth (y m : Int) : Int := dayNum y (m + 1) 1 - dayNum y m 1

structure Wall where
  y : Int
  m : Int
  d : Int
  h : Int := 0
  mi : Int := 0
  s : Int := 0
  deriving Repr, BEq, Inhabited

private def digitsVal (cs : List Char) : Int := cs.foldl (fun a c => a * 10 + (c.toNat - 48 : Nat)) 0

/-- `^(-?\d{4,6})-(\d{2})-(\d{2})(?:T(\d{2}):(\d{2})(?::(\d{2}))?)?$` -/
def parseLocal? (iso : String) : Option Wall := do
  let cs := iso.toList
  let (neg, cs) := match cs with | '-' :: r => (true, r) | r => (false, r)
  let yd := cs.takeWhile Str.isDigit
  if yd.length < 4 || yd.length > 6 then none
  let cs := cs.drop yd.length
  let two (l : List Char) : Option (Int × List Char) :=
    match l with
    | a :: b :: r => if Str.isDigit a && Str.isDigit b then some (digitsVal [a, b], r) else none
    | _ => none
  let cs ← match cs with | '-' :: r => some r | _ => none
  let (m, cs) ← two cs
  let cs ← match cs with | '-' :: r => some r | _ => none
  let (d, cs) ← two cs
  let y := if neg then -digitsVal yd else digitsVal yd
  match cs with
  | [] => some { y, m, d }
  | 'T' :: r =>
    let (h, r) ← two r
    let r ← match r with | ':' :: r => some r | _ => none
    let (mi, r) ← two r
    match r with
    | [] => some { y, m, d, h, mi }
    | ':' :: r =>
      let (s, r) ← two r
      if r.isEmpty then some { y, m, d, h, mi, s } else none
    | _ => none
  | _ => none

/-- The JavaScript throws on garbage; every caller here passes well-formed values, so garbage
reads as 1970-01-01. -/
def parseLocal (iso : String) : Wall := (parseLocal? iso).getD { y := 1970, m := 1, d := 1 }

def formatLocal (w : Wall) : String :=
  pad4 w.y ++ "-" ++ pad2 w.m ++ "-" ++ pad2 w.d ++ "T" ++ pad2 w.h ++ ":" ++ pad2 w.mi ++ ":" ++ pad2 w.s

def take10 (s : String) : String := String.ofList (s.toList.take 10)

/-- Day number of an ISO date or date-time string. -/
def dayOfIso (iso : String) : Int :=
  let w := parseLocal (if iso.length > 10 then iso else take10 iso)
  dayNum w.y w.m w.d

/-- Wall clock of epoch ms read as UTC (floored to the second). -/
def wallOfUtc (ms : Int) : Wall :=
  let day := ms / dayMs
  let r := ms % dayMs
  let (y, m, d) := dayParts day
  { y, m, d, h := r / 3600000, mi := (r % 3600000) / minuteMs, s := (r % minuteMs) / 1000 }

/-! ## Time-zone data -/

inductive DayRule where
  /-- the last such weekday of the month (Sunday = 0) -/
  | last (wd : Int)
  /-- the first such weekday on or after a day of the month -/
  | geq (wd : Int) (d : Int)
  deriving Repr, BEq

inductive AtKind where
  | wall | std | utc
  deriving Repr, BEq

structure Trans where
  month : Int
  day : DayRule
  /-- minutes after local midnight -/
  at_ : Int
  kind : AtKind := .wall
  deriving Repr, BEq

structure Dst where
  save : Int := 60
  start : Trans
  stop : Trans
  deriving Repr, BEq

structure Era where
  /-- this era applies to instants before this epoch ms (none: forever) -/
  until? : Option Int := none
  std : Int
  dst : Option Dst := none
  deriving Repr, BEq

private def sun : Int := 0
private def thu : Int := 4
private def fri : Int := 5

def usRule : Dst := { start := { month := 3, day := .geq sun 8, at_ := 120 }, stop := { month := 11, day := .geq sun 1, at_ := 120 } }
def usOld : Dst := { start := { month := 4, day := .geq sun 1, at_ := 120 }, stop := { month := 10, day := .last sun, at_ := 120 } }
def euRule : Dst := { start := { month := 3, day := .last sun, at_ := 60, kind := .utc }, stop := { month := 10, day := .last sun, at_ := 60, kind := .utc } }
def auRule : Dst := { start := { month := 10, day := .geq sun 1, at_ := 120, kind := .std }, stop := { month := 4, day := .geq sun 1, at_ := 120, kind := .std } }
def nzRule : Dst := { start := { month := 9, day := .last sun, at_ := 120, kind := .std }, stop := { month := 4, day := .geq sun 1, at_ := 120, kind := .std } }
def lhRule : Dst := { save := 30, start := { month := 10, day := .geq sun 1, at_ := 120 }, stop := { month := 4, day := .geq sun 1, at_ := 120 } }
def mexicoRule : Dst := { start := { month := 4, day := .geq sun 1, at_ := 120 }, stop := { month := 10, day := .last sun, at_ := 120 } }
def israelRule : Dst := { start := { month := 3, day := .geq fri 23, at_ := 120 }, stop := { month := 10, day := .last sun, at_ := 120 } }
def egyptRule : Dst := { start := { month := 4, day := .last fri, at_ := 0 }, stop := { month := 10, day := .last thu, at_ := 1440 } }
def chileRule : Dst := { start := { month := 9, day := .geq sun 2, at_ := 240, kind := .utc }, stop := { month := 4, day := .geq sun 2, at_ := 180, kind := .utc } }

/-- 2007-01-01T00:00:00Z: the US rules changed for 2007. -/
private def us2007 : Int := 1167609600000

private def fixed (m : Int) : List Era := [{ std := m }]
private def us (m : Int) : List Era := [{ until? := some us2007, std := m, dst := some usOld }, { std := m, dst := some usRule }]
private def eu (m : Int) : List Era := [{ std := m, dst := some euRule }]

/-- Canonical (ICU) zone name → eras. -/
def zoneTable : List (String × List Era) := [
  ("America/New_York", us (-300)), ("America/Chicago", us (-360)), ("America/Denver", us (-420)),
  ("America/Los_Angeles", us (-480)), ("America/Anchorage", us (-540)), ("America/Halifax", us (-240)),
  ("America/Toronto", us (-300)), ("America/Vancouver", us (-480)), ("America/Detroit", us (-300)),
  ("America/Winnipeg", us (-360)), ("America/Edmonton", us (-420)),
  ("America/Indianapolis", [{ until? := some 1136073600000, std := -300 },
      { until? := some us2007, std := -300, dst := some usOld }, { std := -300, dst := some usRule }]),
  ("America/Mexico_City", [{ until? := some 1667113200000, std := -360, dst := some mexicoRule }, { std := -360 }]),
  ("America/Santiago", [{ until? := some 1662264000000, std := -240, dst := some chileRule },
      { until? := some 1662868800000, std := -240 }, { std := -240, dst := some chileRule }]),
  ("America/Phoenix", fixed (-420)), ("Pacific/Honolulu", fixed (-600)), ("America/Regina", fixed (-360)),
  ("America/Puerto_Rico", fixed (-240)), ("America/Bogota", fixed (-300)), ("America/Lima", fixed (-300)),
  ("America/Sao_Paulo", fixed (-180)), ("America/Buenos_Aires", fixed (-180)), ("America/Caracas", fixed (-240)),
  ("Atlantic/Reykjavik", fixed 0), ("Europe/Moscow", fixed 180), ("Europe/Istanbul", fixed 180),
  ("Asia/Dubai", fixed 240), ("Asia/Riyadh", fixed 180), ("Asia/Calcutta", fixed 330), ("Asia/Karachi", fixed 300),
  ("Asia/Bangkok", fixed 420), ("Asia/Jakarta", fixed 420), ("Asia/Singapore", fixed 480), ("Asia/Shanghai", fixed 480),
  ("Asia/Hong_Kong", fixed 480), ("Asia/Taipei", fixed 480), ("Asia/Manila", fixed 480), ("Asia/Tokyo", fixed 540),
  ("Asia/Seoul", fixed 540), ("Australia/Brisbane", fixed 600), ("Australia/Perth", fixed 480),
  ("Australia/Darwin", fixed 570), ("Asia/Katmandu", fixed 345), ("Pacific/Kiritimati", fixed 840),
  ("Asia/Dhaka", fixed 360), ("Africa/Johannesburg", fixed 120), ("Africa/Lagos", fixed 60),
  ("Africa/Nairobi", fixed 180), ("Africa/Accra", fixed 0), ("Asia/Kabul", fixed 270), ("Asia/Rangoon", fixed 390),
  ("Asia/Colombo", fixed 330), ("Asia/Saigon", fixed 420), ("Asia/Kuala_Lumpur", fixed 480), ("Pacific/Guam", fixed 600),
  ("Europe/London", eu 0), ("Europe/Dublin", eu 0), ("Europe/Lisbon", eu 0), ("Atlantic/Canary", eu 0),
  ("Europe/Berlin", eu 60), ("Europe/Paris", eu 60), ("Europe/Amsterdam", eu 60), ("Europe/Brussels", eu 60),
  ("Europe/Madrid", eu 60), ("Europe/Rome", eu 60), ("Europe/Vienna", eu 60), ("Europe/Zurich", eu 60),
  ("Europe/Stockholm", eu 60), ("Europe/Oslo", eu 60), ("Europe/Copenhagen", eu 60), ("Europe/Prague", eu 60),
  ("Europe/Budapest", eu 60), ("Europe/Warsaw", eu 60), ("Europe/Bucharest", eu 120), ("Europe/Kiev", eu 120),
  ("Europe/Athens", eu 120), ("Europe/Helsinki", eu 120), ("Europe/Sofia", eu 120), ("Europe/Riga", eu 120),
  ("Europe/Tallinn", eu 120), ("Europe/Vilnius", eu 120),
  ("Asia/Jerusalem", [{ std := 120, dst := some israelRule }]),
  ("Africa/Cairo", [{ until? := some 1672531200000, std := 120 }, { std := 120, dst := some egyptRule }]),
  ("Australia/Sydney", [{ std := 600, dst := some auRule }]), ("Australia/Melbourne", [{ std := 600, dst := some auRule }]),
  ("Australia/Hobart", [{ std := 600, dst := some auRule }]), ("Australia/Adelaide", [{ std := 570, dst := some auRule }]),
  ("Australia/Lord_Howe", [{ std := 630, dst := some lhRule }]), ("Pacific/Auckland", [{ std := 720, dst := some nzRule }])
]

/-- Accepted aliases (lower-cased) and the name ICU resolves them to. -/
def zoneAliases : List (String × String) := [
  ("america/indiana/indianapolis", "America/Indianapolis"), ("america/argentina/buenos_aires", "America/Buenos_Aires"),
  ("asia/istanbul", "Europe/Istanbul"), ("asia/kolkata", "Asia/Calcutta"), ("asia/kathmandu", "Asia/Katmandu"),
  ("asia/yangon", "Asia/Rangoon"), ("asia/ho_chi_minh", "Asia/Saigon"), ("europe/kyiv", "Europe/Kiev"),
  ("us/eastern", "America/New_York"), ("us/central", "America/Chicago"), ("us/mountain", "America/Denver"),
  ("us/pacific", "America/Los_Angeles"), ("us/alaska", "America/Anchorage"), ("us/hawaii", "Pacific/Honolulu"),
  ("us/arizona", "America/Phoenix"), ("gb", "Europe/London"), ("asia/tel_aviv", "Asia/Jerusalem"),
  ("japan", "Asia/Tokyo"), ("universal", "UTC"), ("uct", "UTC")
]

/-- `Etc/GMT+5` style names: (canonical spelling, offset minutes). -/
private def etcGmt? (lower : String) : Option (String × Int) :=
  if !lower.startsWith "etc/gmt" then none else
  let rest := (lower.drop 7).toString
  match rest.toList with
  | sign :: ds =>
    if (sign == '+' || sign == '-') && !ds.isEmpty && ds.length ≤ 2 && ds.all Str.isDigit then
      let n := digitsVal ds
      if n == 0 || (sign == '+' && n > 12) || (sign == '-' && n > 14) then none
      else some ("Etc/GMT" ++ String.singleton sign ++ toString n, if sign == '+' then -n * 60 else n * 60)
    else none
  | _ => none

private def zoneNameOk (s : String) : Bool :=
  match s.toList with
  | c :: rest => Str.isAlpha c && rest.length ≤ 63 &&
      rest.all (fun c => Str.isAlpha c || Str.isDigit c || c == '_' || c == '+' || c == '-' || c == '/')
  | [] => false

/-- The zone name ICU would report for `s`, if the table knows it. -/
private def lookupCanonical (s : String) : Option String :=
  let lower := s.toLower
  match zoneTable.find? (fun (n, _) => n.toLower == lower) with
  | some (n, _) => some n
  | none =>
    match zoneAliases.find? (fun (a, _) => a == lower) with
    | some (_, n) => some n
    | none => (etcGmt? lower).map (·.1)

def isUtcName (tz : String) : Bool :=
  tz == "UTC" || tz == "Etc/UTC" || tz == "GMT" || tz == "Etc/GMT" || tz == "Z"

/-- Canonical IANA name for a zone identifier, or none when it is unknown. Any case; 'UTC',
'GMT', 'Z', 'Zulu' → 'UTC'. -/
def canonicalTimeZone (name : Option String) : Option String :=
  match name with
  | none => none
  | some n =>
    let key := Str.trimJs n
    if key.isEmpty then none
    else
      let l := key.toLower
      if l == "utc" || l == "gmt" || l == "z" || l == "zulu" || l == "etc/utc" || l == "etc/gmt" then some "UTC"
      else if zoneNameOk key then lookupCanonical key
      else none

private def erasOf (tz : String) : Option (List Era) := do
  let c ← lookupCanonical tz
  if c == "UTC" then return fixed 0
  match zoneTable.find? (·.1 == c) with
  | some (_, es) => some es
  | none => (etcGmt? tz.toLower).map fun (_, off) => fixed off

private def transDay (y : Int) (t : Trans) : Int :=
  match t.day with
  | .last wd =>
    let dim := daysInMonth y t.month
    let ld := dayNum y t.month dim
    ld - (weekday ld - wd + 7) % 7
  | .geq wd d =>
    let dn := dayNum y t.month d
    dn + (wd - weekday dn + 7) % 7

/-- UTC instant of a transition in year `y`; `before` is the offset in force just before it. -/
private def transUtc (y : Int) (t : Trans) (std before : Int) : Int :=
  let loc := transDay y t * dayMs + t.at_ * minuteMs
  match t.kind with
  | .utc => loc
  | .std => loc - std * minuteMs
  | .wall => loc - before * minuteMs

private def eraOffset (e : Era) (ms : Int) : Int :=
  match e.dst with
  | none => e.std
  | some r =>
    let (y, _, _) := dayParts (ms / dayMs)
    let s := transUtc y r.start e.std e.std
    let t := transUtc y r.stop e.std (e.std + r.save)
    let inDst := if s < t then s ≤ ms && ms < t else ms ≥ s || ms < t
    if inDst then e.std + r.save else e.std

/-- Offset of `tz` at `ms`, in minutes east of UTC (Los Angeles in winter: -480). Unknown → 0. -/
def tzOffsetMinutes (ms : Int) (tz : String) : Int :=
  if isUtcName tz then 0 else
  match erasOf tz with
  | none => 0
  | some es =>
    match es.find? (fun e => match e.until? with | some u => ms < u | none => true) with
    | some e => eraOffset e ms
    | none => 0

/-- Epoch ms of a wall-clock time in a zone. Two-pass offset resolution: a time inside a
spring-forward gap moves forward by the gap; an ambiguous fall-back time takes the earlier
instant. `tz` none/UTC means the wall clock *is* UTC. -/
def zonedToUtc (localIso : String) (tz : Option String) : Int :=
  let w := parseLocal localIso
  let naive := utcMs w.y w.m w.d w.h w.mi w.s
  match tz with
  | none => naive
  | some tz =>
    if isUtcName tz then naive else
    let off1 := tzOffsetMinutes naive tz
    let guess1 := naive - off1 * minuteMs
    let off2 := tzOffsetMinutes guess1 tz
    if off2 == off1 then guess1 else
    let guess2 := naive - off2 * minuteMs
    let off3 := tzOffsetMinutes guess2 tz
    if off3 == off2 then guess2 else max guess1 guess2

/-- Wall-clock 'YYYY-MM-DDTHH:MM:SS' of an instant in a zone (none/UTC: the UTC clock). -/
def utcToZoned (ms : Int) (tz : Option String) : String :=
  match tz with
  | none => formatLocal (wallOfUtc ms)
  | some tz =>
    if isUtcName tz then formatLocal (wallOfUtc ms)
    else formatLocal (wallOfUtc ((ms / 1000) * 1000 + tzOffsetMinutes ms tz * minuteMs))

/-- Windows / Exchange time-zone identifiers → IANA (keys lower-cased), as in the JavaScript. -/
def windowsTz : List (String × String) := [
  ("utc", "UTC"), ("coordinated universal time", "UTC"), ("gmt standard time", "Europe/London"),
  ("greenwich standard time", "Atlantic/Reykjavik"), ("w. europe standard time", "Europe/Berlin"),
  ("romance standard time", "Europe/Paris"), ("central europe standard time", "Europe/Budapest"),
  ("central european standard time", "Europe/Warsaw"), ("e. europe standard time", "Europe/Bucharest"),
  ("gtb standard time", "Europe/Bucharest"), ("fle standard time", "Europe/Kiev"),
  ("russian standard time", "Europe/Moscow"), ("turkey standard time", "Europe/Istanbul"),
  ("israel standard time", "Asia/Jerusalem"), ("arabian standard time", "Asia/Dubai"),
  ("arab standard time", "Asia/Riyadh"), ("india standard time", "Asia/Kolkata"),
  ("pakistan standard time", "Asia/Karachi"), ("se asia standard time", "Asia/Bangkok"),
  ("singapore standard time", "Asia/Singapore"), ("china standard time", "Asia/Shanghai"),
  ("taipei standard time", "Asia/Taipei"), ("tokyo standard time", "Asia/Tokyo"),
  ("korea standard time", "Asia/Seoul"), ("aus eastern standard time", "Australia/Sydney"),
  ("e. australia standard time", "Australia/Brisbane"), ("cen. australia standard time", "Australia/Adelaide"),
  ("w. australia standard time", "Australia/Perth"), ("new zealand standard time", "Pacific/Auckland"),
  ("hawaiian standard time", "Pacific/Honolulu"), ("alaskan standard time", "America/Anchorage"),
  ("pacific standard time", "America/Los_Angeles"), ("pacific time (us & canada)", "America/Los_Angeles"),
  ("us mountain standard time", "America/Phoenix"), ("mountain standard time", "America/Denver"),
  ("mountain time (us & canada)", "America/Denver"), ("central standard time", "America/Chicago"),
  ("central time (us & canada)", "America/Chicago"), ("central standard time (mexico)", "America/Mexico_City"),
  ("canada central standard time", "America/Regina"), ("eastern standard time", "America/New_York"),
  ("eastern time (us & canada)", "America/New_York"), ("us eastern standard time", "America/Indiana/Indianapolis"),
  ("atlantic standard time", "America/Halifax"), ("sa western standard time", "America/Puerto_Rico"),
  ("sa pacific standard time", "America/Bogota"), ("pacific sa standard time", "America/Santiago"),
  ("e. south america standard time", "America/Sao_Paulo"), ("argentina standard time", "America/Argentina/Buenos_Aires"),
  ("south africa standard time", "Africa/Johannesburg"), ("egypt standard time", "Africa/Cairo"),
  ("w. central africa standard time", "Africa/Lagos")
]

/-- VTIMEZONE id → STANDARD TZOFFSETTO minutes (first definition wins, like the JS `Map`). -/
abbrev VTimezones := List (String × Option Int)

def VTimezones.get? (m : VTimezones) (k : String) : Option (Option Int) :=
  (m.find? (·.1 == k)).map (·.2)

structure Resolved where
  tzid : Option String := none
  fixedOffsetMinutes : Option Int := none
  deriving Repr, BEq

/-- `s.replace(/^"+|"+$/g, "")` -/
def stripQuotes (s : String) : String :=
  let cs := s.toList.dropWhile (· == '"')
  String.ofList (cs.reverse.dropWhile (· == '"')).reverse

private def isWs (c : Char) : Bool := Str.isJsWhitespace c
private def dropWs (l : List Char) : List Char := l.dropWhile isWs

private def utcGmt? (l : List Char) : Option (List Char) :=
  match l with
  | a :: b :: c :: r =>
    let w := (String.ofList [a, b, c]).toUpper
    if w == "UTC" || w == "GMT" then some r else none
  | _ => none

/-- `([+-])\s*(\d{1,2})(?::?(\d{2}))?` in JS preference order, each alternative handed to `k`. -/
private def signedOffsetAlts (l : List Char) : List (Int × List Char) :=
  match l with
  | sign :: r =>
    if sign != '+' && sign != '-' then [] else
    let r := dropWs r
    let sgn : Int := if sign == '-' then -1 else 1
    let ds := r.takeWhile Str.isDigit
    let hs := (if ds.length ≥ 2 then [2] else []) ++ (if ds.length ≥ 1 then [1] else [])
    hs.flatMap fun n =>
      let h := digitsVal (r.take n)
      let after := r.drop n
      let mins : List (Int × List Char) :=
        let withColon := match after with
          | ':' :: a :: b :: rest => if Str.isDigit a && Str.isDigit b then [(digitsVal [a, b], rest)] else []
          | _ => []
        let noColon := match after with
          | a :: b :: rest => if Str.isDigit a && Str.isDigit b then [(digitsVal [a, b], rest)] else []
          | _ => []
        withColon ++ noColon ++ [(0, after)]
      mins.map fun (m, rest) => (sgn * (h * 60 + m), rest)
  | [] => []

/-- `^\(\s*(?:UTC|GMT)\s*(?:([+-])\s*(\d{1,2})(?::?(\d{2}))?)?\s*\)\s*(.*)$` (case-insensitive) or
`^(?:UTC|GMT)\s*([+-])\s*(\d{1,2})(?::?(\d{2}))?\s*(.*)$`: (offset, rest). -/
private def outlookPrefix? (s : String) : Option (Int × String) :=
  let l := s.toList
  let closeParen (rest : List Char) : Option (List Char) :=
    match dropWs rest with
    | ')' :: r => some (dropWs r)
    | _ => none
  let first : Option (Int × String) := do
    let r ← match l with | '(' :: r => some r | _ => none
    let r ← utcGmt? (dropWs r)
    let r := dropWs r
    let alts := (signedOffsetAlts r).map (fun (o, rest) => (o, rest)) ++ [(0, r)]
    let hit := alts.findSome? fun (o, rest) => (closeParen rest).map (o, ·)
    let (o, rest) ← hit
    pure (o, String.ofList rest)
  match first with
  | some x => some x
  | none => do
    let r ← utcGmt? l
    let alts := signedOffsetAlts (dropWs r)
    let (o, rest) ← alts.head?
    pure (o, String.ofList (dropWs rest))

/-- Turn a TZID parameter into something usable, in order: a valid IANA name; the name with
Outlook's "(UTC-05:00) " prefix / quotes / path prefixes stripped; the Windows table; the
VTIMEZONE's STANDARD offset (fixed, no DST); the offset from the prefix itself. -/
def resolveTzid (raw : Option String) (vtimezones : Option VTimezones := none) : Resolved := Id.run do
  let none_ : Resolved := {}
  let some raw := raw | return none_
  let original := Str.trimJs raw
  let mut s := Str.trimJs (stripQuotes original)
  if s.isEmpty then return none_
  if let some c := canonicalTimeZone (some s) then return { tzid := some c }
  let mut prefixOffset : Option Int := none
  if let some (off, rest) := outlookPrefix? s then
    prefixOffset := some off
    s := Str.trimJs rest
    if s.isEmpty then
      return if off == 0 then { tzid := some "UTC" } else { fixedOffsetMinutes := some off }
    if let some c := canonicalTimeZone (some s) then return { tzid := some c }
  if s.contains '/' then
    let segs := (s.splitOn "/").filter (!·.isEmpty)
    for n in [2, 3, 1] do
      if segs.length > n then
        if let some c := canonicalTimeZone (some ("/".intercalate (segs.drop (segs.length - n)))) then
          return { tzid := some c }
  if s.contains ' ' then
    if let some c := canonicalTimeZone (some (Str.collapse isWs "_" s)) then return { tzid := some c }
  if let some (_, w) := windowsTz.find? (·.1 == s.toLower) then return { tzid := some w }
  if let some vt := vtimezones then
    let hit := (vt.get? original).orElse fun _ => (vt.get? s).orElse fun _ => vt.get? (stripQuotes original)
    if let some (some off) := hit then return { fixedOffsetMinutes := some off }
  if let some off := prefixOffset then return { fixedOffsetMinutes := some off }
  return none_

/-! ## Date-time values and recurrence-instance ids -/

/-- A DATE or DATE-TIME value as `shared/ical.js` records it. `utc`: epoch ms (none for all-day).
`local`: wall clock as written. `date`: 'YYYY-MM-DD' for all-day values. `tzid`: resolved zone
('UTC' for 'Z'). `tzidRaw`: the TZID parameter as written. `floating`: no zone could be attached. -/
structure DateTime where
  utc : Option Int := none
  loc : String
  date : Option String := none
  tzid : Option String := none
  tzidRaw : Option String := none
  allDay : Bool := false
  floating : Bool := false
  deriving Repr, BEq, Inhabited

def DateTime.dateOf (dt : DateTime) : String := dt.date.getD (take10 dt.loc)

/-- '' for a master, 'T' + UTC seconds for a timed slot, 'D' + YYYYMMDD for an all-day slot. -/
def ridKey (dt : Option DateTime) : String :=
  match dt with
  | none => ""
  | some dt =>
    if dt.allDay then "D" ++ String.ofList ((dt.dateOf).toList.filter (· != '-'))
    else match dt.utc with
      | some u => "T" ++ toString (u / 1000)
      | none => "L" ++ dt.loc

/-! ## RRULE -/

inductive Freq where
  | daily | weekly | monthly | yearly
  deriving Repr, BEq, Inhabited

def Freq.toString : Freq → String
  | .daily => "DAILY" | .weekly => "WEEKLY" | .monthly => "MONTHLY" | .yearly => "YEARLY"

instance : ToString Freq := ⟨Freq.toString⟩

def weekdays : List String := ["MO", "TU", "WE", "TH", "FR", "SA", "SU"]

/-- JS weekday number (Sunday = 0) of a two-letter day. -/
def weekdayNum? (d : String) : Option Int :=
  match d with
  | "SU" => some 0 | "MO" => some 1 | "TU" => some 2 | "WE" => some 3
  | "TH" => some 4 | "FR" => some 5 | "SA" => some 6 | _ => none

structure ByDay where
  /-- 0 = every such weekday -/
  ord : Int
  day : String
  deriving Repr, BEq, Inhabited

structure Until where
  utc : Option Int
  date : Option String
  deriving Repr, BEq, Inhabited

structure RRule where
  freq : Freq
  interval : Nat := 1
  count : Option Nat := none
  until_ : Option Until := none
  byday : Option (List ByDay) := none
  bymonthday : Option (List Int) := none
  bymonth : Option (List Int) := none
  bysetpos : Option (List Int) := none
  wkst : String := "MO"
  /-- every part this expander cannot honour, verbatim (plus the two combination notes) -/
  unsupported : List String := []
  deriving Repr, BEq, Inhabited

/-- The rule is beyond this expander: expansion yields only the first instance, flagged `complex`. -/
def RRule.isComplex (r : RRule) : Bool := !r.unsupported.isEmpty

/-- `Number(s)` when it is an integer (`Number.isInteger`), JS-style: '' → 0, '+5', '5.0', '1e2',
'0x1F'. -/
def jsInteger? (s : String) : Option Int := Id.run do
  let t := Str.trimJs s
  if t.isEmpty then return some 0
  let cs := t.toList
  if let '0' :: x :: hex := cs then
    if (x == 'x' || x == 'X') && !hex.isEmpty && hex.all Str.isHex then
      return some (hex.foldl (fun a c => a * 16 + Str.hexVal c) 0 : Nat)
  let (sgn, cs) : Int × List Char := match cs with
    | '-' :: r => (-1, r) | '+' :: r => (1, r) | r => (1, r)
  let ip := cs.takeWhile Str.isDigit
  let cs := cs.drop ip.length
  let (fp, cs) := match cs with
    | '.' :: r => let f := r.takeWhile Str.isDigit; (some f, r.drop f.length)
    | r => (none, r)
  let fpd := fp.getD []
  if ip.isEmpty && fpd.isEmpty then return none
  let (ex, cs) : Int × List Char := match cs with
    | e :: r =>
      if e == 'e' || e == 'E' then
        let (es, r) : Int × List Char := match r with | '-' :: r => (-1, r) | '+' :: r => (1, r) | r => (1, r)
        let ed := r.takeWhile Str.isDigit
        if ed.isEmpty then (0, e :: r) else (es * digitsVal ed, r.drop ed.length)
      else (0, e :: r)
    | [] => (0, [])
  if !cs.isEmpty then return none
  let mant := digitsVal (ip ++ fpd)
  let e := ex - fpd.length
  if e ≥ 0 then return some (sgn * mant * 10 ^ e.toNat)
  let d : Int := 10 ^ (-e).toNat
  if mant % d == 0 then return some (sgn * (mant / d)) else return none

/-- `^(\d{4})(\d{2})(\d{2})(?:T(\d{2})(\d{2})(\d{2})?(Z)?)?$` (case-insensitive): the digit groups
as strings and whether a 'Z' ended it. -/
def matchDateTime? (v : String) : Option (String × String × String × Option (String × String × Option String) × Bool) := do
  let cs := v.toList
  let dg (n : Nat) (l : List Char) : Option (String × List Char) :=
    let p := l.take n
    if p.length == n && p.all Str.isDigit then some (String.ofList p, l.drop n) else none
  let (y, cs) ← dg 4 cs
  let (m, cs) ← dg 2 cs
  let (d, cs) ← dg 2 cs
  match cs with
  | [] => pure (y, m, d, none, false)
  | t :: r =>
    if t != 'T' && t != 't' then none
    let (h, r) ← dg 2 r
    let (mi, r) ← dg 2 r
    let (s, r) := match dg 2 r with | some (s, r) => (some s, r) | none => (none, r)
    match r with
    | [] => pure (y, m, d, some (h, mi, s), false)
    | [z] => if z == 'Z' || z == 'z' then pure (y, m, d, some (h, mi, s), true) else none
    | _ => none

/-- `^([+-]?\d{1,2})?(MO|TU|WE|TH|FR|SA|SU)$` (case-insensitive) → (ordinal text, day). -/
private def matchByDay? (p : String) : Option (Option String × String) :=
  let cs := p.toList
  if cs.length < 2 then none else
  let day := (String.ofList (cs.drop (cs.length - 2))).toUpper
  let pre := cs.take (cs.length - 2)
  if !weekdays.contains day then none else
  if pre.isEmpty then some (none, day) else
  let digits := match pre with | '+' :: r => r | '-' :: r => r | r => r
  if !digits.isEmpty && digits.length ≤ 2 && digits.all Str.isDigit then some (some (String.ofList pre), day) else none

private def intList (val : String) (lo hi : Int) (noZero : Bool) : Option (List Int) := Id.run do
  let mut out : Array Int := #[]
  let mut ok := true
  for piece in val.splitOn "," do
    match jsInteger? piece with
    | some n =>
      if n < lo || n > hi || (noZero && n == 0) then ok := false
      else if !out.contains n then out := out.push n
    | none => ok := false
  return if ok && out.size > 0 then some out.toList else none

/-- RFC 5545 3.3.10 RRULE, as `parseRRule` in `shared/ical.js`. Sub-daily frequencies,
BYHOUR/BYMINUTE/BYSECOND/BYWEEKNO/BYYEARDAY, BYSETPOS other than ±1, ordinals outside
MONTHLY/YEARLY, BYMONTHDAY with WEEKLY and any unknown part are reported in `unsupported`.
`tz` is the zone of DTSTART, used to anchor a floating UNTIL. None when there is no FREQ. -/
def parseRule? (str : String) (tz : Option String := none) : Option RRule := Id.run do
  let mut unsupported : Array String := #[]
  let mut freq : Option Freq := none
  let mut interval : Nat := 1
  let mut count : Option Nat := none
  let mut until_ : Option Until := none
  let mut byday : Option (List ByDay) := none
  let mut bymonthday : Option (List Int) := none
  let mut bymonth : Option (List Int) := none
  let mut bysetpos : Option (List Int) := none
  let mut wkst := "MO"
  for part in str.splitOn ";" do
    let p := Str.trimJs part
    if p.isEmpty then continue
    let pcs := p.toList
    let eq := pcs.findIdx (· == '=')
    if eq == 0 || eq ≥ pcs.length then
      unsupported := unsupported.push p
      continue
    let key := (Str.trimJs (String.ofList (pcs.take eq))).toUpper
    let val := Str.trimJs (String.ofList (pcs.drop (eq + 1)))
    match key with
    | "FREQ" =>
      match val.toUpper with
      | "DAILY" => freq := some .daily
      | "WEEKLY" => freq := some .weekly
      | "MONTHLY" => freq := some .monthly
      | "YEARLY" => freq := some .yearly
      | _ => freq := some .daily; unsupported := unsupported.push p
    | "INTERVAL" =>
      match jsInteger? val with
      | some n => if n ≥ 1 then interval := n.toNat else unsupported := unsupported.push p
      | none => unsupported := unsupported.push p
    | "COUNT" =>
      match jsInteger? val with
      | some n => if n ≥ 1 then count := some n.toNat else unsupported := unsupported.push p
      | none => unsupported := unsupported.push p
    | "UNTIL" =>
      match matchDateTime? val with
      | none => unsupported := unsupported.push p
      | some (y, m, d, time, z) =>
        let date := y ++ "-" ++ m ++ "-" ++ d
        match time with
        | none => until_ := some { utc := none, date := some date }
        | some (h, mi, s) =>
          let loc := date ++ "T" ++ h ++ ":" ++ mi ++ ":" ++ s.getD "00"
          let utc := if z then zonedToUtc loc none else zonedToUtc loc (canonicalTimeZone tz)
          until_ := some { utc := some utc, date := some (if z then take10 (utcToZoned utc none) else date) }
    | "BYDAY" =>
      let mut list : Array ByDay := #[]
      let mut ok := true
      for piece in val.splitOn "," do
        match matchByDay? (Str.trimJs piece) with
        | none => ok := false
        | some (ordS, day) =>
          let ord := match ordS with | none => 0 | some o => (jsInteger? o).getD 0
          if ord == 0 && ordS.isSome then ok := false
          else list := list.push { ord, day }
      if ok && list.size > 0 then byday := some list.toList else unsupported := unsupported.push p
    | "BYMONTHDAY" =>
      match intList val (-31) 31 true with
      | some l => bymonthday := some l
      | none => unsupported := unsupported.push p
    | "BYMONTH" =>
      match intList val 1 12 true with
      | some l => bymonth := some l
      | none => unsupported := unsupported.push p
    | "BYSETPOS" =>
      let l := intList val (-366) 366 true
      if l.isSome then bysetpos := l
      match l with
      | none => unsupported := unsupported.push p
      | some l => if l.any (fun n => n != 1 && n != -1) then unsupported := unsupported.push p
    | "WKST" =>
      let w := val.toUpper
      if weekdays.contains w then wkst := w else unsupported := unsupported.push p
    | _ => unsupported := unsupported.push p
  let some f := freq | return none
  if f == .weekly && bymonthday.isSome then unsupported := unsupported.push "BYMONTHDAY with FREQ=WEEKLY"
  if (f == .weekly || f == .daily) && (byday.getD []).any (·.ord != 0) then
    unsupported := unsupported.push ("BYDAY ordinal with FREQ=" ++ f.toString)
  return some { freq := f, interval, count, until_, byday, bymonthday, bymonth, bysetpos, wkst, unsupported := unsupported.toList }

/-- `parseRule?` with an error for a rule without a usable FREQ. -/
def parse (s : String) (tz : Option String := none) : Except String RRule :=
  match parseRule? s tz with
  | some r => .ok r
  | none => .error ("rrule: no FREQ in " ++ s)

private def compactUtc (ms : Int) : String :=
  let w := wallOfUtc ms
  pad4 w.y ++ pad2 w.m ++ pad2 w.d ++ "T" ++ pad2 w.h ++ pad2 w.mi ++ pad2 w.s ++ "Z"

/-- Back to RFC 5545 text. `parse (toString r) = r` for rules whose UNTIL is a date or UTC and
whose FREQ came first; unsupported parts are written back verbatim (a non-standard FREQ replaces
the DAILY it was read as), the two combination notes are left for `parse` to re-derive. -/
def RRule.toString (r : RRule) : String :=
  let isNote (u : String) := u == "BYMONTHDAY with FREQ=WEEKLY" || u.startsWith "BYDAY ordinal with FREQ="
  let rawFreq := r.unsupported.find? (fun u => (u.toUpper.startsWith "FREQ="))
  let freq := rawFreq.getD ("FREQ=" ++ r.freq.toString)
  let ints (l : List Int) := ",".intercalate (l.map ToString.toString)
  let parts : List String :=
    [freq] ++
    (if r.interval != 1 then ["INTERVAL=" ++ ToString.toString r.interval] else []) ++
    (match r.count with | some c => ["COUNT=" ++ ToString.toString c] | none => []) ++
    (match r.until_ with
      | some { utc := some u, .. } => ["UNTIL=" ++ compactUtc u]
      | some { utc := none, date := some d } => ["UNTIL=" ++ String.ofList (d.toList.filter (· != '-'))]
      | _ => []) ++
    (match r.byday with
      | some l => ["BYDAY=" ++ ",".intercalate (l.map fun b => (if b.ord == 0 then "" else ToString.toString b.ord) ++ b.day)]
      | none => []) ++
    (match r.bymonthday with | some l => ["BYMONTHDAY=" ++ ints l] | none => []) ++
    (match r.bymonth with | some l => ["BYMONTH=" ++ ints l] | none => []) ++
    (match r.bysetpos with | some l => ["BYSETPOS=" ++ ints l] | none => []) ++
    (if r.wkst != "MO" then ["WKST=" ++ r.wkst] else []) ++
    (r.unsupported.filter fun u => !isNote u && some u != rawFreq &&
      !(u.toUpper.startsWith "BYSETPOS=" && r.bysetpos.isSome))
  ";".intercalate parts

instance : ToString RRule := ⟨RRule.toString⟩

/-! ## Expansion engine -/

/-- What expansion needs from an event record. -/
structure Series where
  start : DateTime
  end_ : DateTime
  durationMs : Int
  rrule : Option RRule := none
  rdates : List DateTime := []
  exdates : List DateTime := []
  deriving Repr, Inhabited

structure Ctx where
  series : Series
  allDay : Bool
  tz : Option String
  fixedOffset : Int
  seed : Wall
  seedDay : Int
  durationMs : Int
  days : Int
  slackDays : Int

structure Instance where
  day : Int
  loc : String
  date : Option String
  start : Int
  end_ : Int
  deriving Repr, Inhabited

def context (r : Series) : Ctx :=
  let s := r.start
  let allDay := s.allDay
  let tz := canonicalTimeZone s.tzid
  let seed := parseLocal (if allDay then s.dateOf ++ "T00:00:00" else s.loc)
  let seedDay := dayNum seed.y seed.m seed.d
  let fixedOffset := match allDay, tz, s.utc with
    | false, none, some u => (utcMs seed.y seed.m seed.d seed.h seed.mi seed.s - u + 30000) / minuteMs
    | _, _, _ => 0
  let dur := max 0 r.durationMs
  let (days, dur) :=
    if allDay then
      let endDay := if !r.end_.loc.isEmpty then dayOfIso r.end_.dateOf else seedDay + 1
      let days := max 1 (endDay - seedDay)
      (days, days * dayMs)
    else (1, dur)
  let slackDays := if allDay then days else (dur + dayMs - 1) / dayMs
  { series := r, allDay, tz, fixedOffset, seed, seedDay, durationMs := dur, days, slackDays }

def dayStartMs (day : Int) (tz : Option String) : Int :=
  match tz with
  | some _ => zonedToUtc (isoDate day ++ "T00:00:00") tz
  | none => day * dayMs

/-- The instance that starts on calendar day `day` at the seed's time of day. -/
def instanceForDay (ctx : Ctx) (day : Int) : Instance :=
  if ctx.allDay then
    let date := isoDate day
    { day, loc := date ++ "T00:00:00", date := some date, start := dayStartMs day ctx.tz, end_ := dayStartMs (day + ctx.days) ctx.tz }
  else
    let (y, m, d) := dayParts day
    let w : Wall := { y, m, d, h := ctx.seed.h, mi := ctx.seed.mi, s := ctx.seed.s }
    let loc := formatLocal w
    let start := match ctx.tz with
      | some _ => zonedToUtc loc ctx.tz
      | none => utcMs w.y w.m w.d w.h w.mi w.s - ctx.fixedOffset * minuteMs
    { day, loc, date := none, start, end_ := start + ctx.durationMs }

/-- Local calendar day (in the event zone) containing an instant. -/
def localDayOf (ctx : Ctx) (ms : Int) : Int :=
  match ctx.tz with
  | some _ => dayOfIso (utcToZoned ms ctx.tz)
  | none => (ms + ctx.fixedOffset * minuteMs) / dayMs

def rdateInstance (ctx : Ctx) (dt : DateTime) : Instance :=
  if ctx.allDay || dt.allDay then instanceForDay ctx (dayOfIso dt.dateOf) else
  let start :=
    if dt.floating && ctx.tz.isSome then zonedToUtc dt.loc ctx.tz
    else match dt.utc with
      | some u => u
      | none => zonedToUtc dt.loc ctx.tz
  let loc := match ctx.tz with
    | some _ => utcToZoned start ctx.tz
    | none => utcToZoned (start + ctx.fixedOffset * minuteMs) none
  { day := dayOfIso loc, loc, date := none, start, end_ := start + ctx.durationMs }

def sortedRdates (ctx : Ctx) : Array Instance :=
  let out := (ctx.series.rdates.map (rdateInstance ctx)).mergeSort (fun a b => decide (a.start ≤ b.start))
  let rec dedupe : List Instance → Option Int → List Instance
    | [], _ => []
    | x :: xs, prev => if some x.start == prev then dedupe xs prev else x :: dedupe xs (some x.start)
  (dedupe out none).toArray

structure ExIndex where
  utc : List Int := []
  loc : List String := []
  dates : List String := []

def exdateIndex (ctx : Ctx) : ExIndex := Id.run do
  let mut ex : ExIndex := {}
  let eventTz := ctx.series.start.tzid
  for x in ctx.series.exdates do
    if x.allDay then
      ex := { ex with dates := x.dateOf :: ex.dates }
      continue
    if x.floating && ctx.tz.isSome then ex := { ex with utc := zonedToUtc x.loc ctx.tz :: ex.utc }
    else if let some u := x.utc then ex := { ex with utc := u :: ex.utc }
    if x.floating || x.tzid == eventTz then ex := { ex with loc := x.loc :: ex.loc }
    if ctx.allDay then ex := { ex with dates := take10 x.loc :: ex.dates }
  return ex

def isExcluded (ex : ExIndex) (inst : Instance) (ctx : Ctx) : Bool :=
  if ctx.allDay then ex.dates.contains (inst.date.getD "")
  else ex.utc.contains inst.start || ex.loc.contains inst.loc || ex.dates.contains (take10 inst.loc)

def beyondUntil (inst : Instance) (until_ : Option Until) (ctx : Ctx) : Bool :=
  match until_ with
  | none => false
  | some u =>
    if ctx.allDay then
      let limit := match u.date with
        | some d => some d
        | none => u.utc.map fun ms => take10 (utcToZoned ms none)
      match limit with
      | some l => decide (l < inst.date.getD "")
      | none => false
    else match u.utc with
      | some ms => inst.start > ms
      | none => match u.date with
        | some d => decide (d < take10 inst.loc)
        | none => false

private def sortInts (l : List Int) : List Int := l.mergeSort (fun a b => decide (a ≤ b))

def applySetPos (cands : List Int) (bysetpos : Option (List Int)) : List Int :=
  match bysetpos with
  | none => cands
  | some ps =>
    if cands.isEmpty then cands else
    let arr := cands.toArray
    let out := ps.foldl (fun (acc : List Int) pos =>
      let idx : Int := if pos > 0 then pos - 1 else arr.size + pos
      if idx < 0 || idx ≥ arr.size then acc else
      let v := arr[idx.toNat]!
      if acc.contains v then acc else acc ++ [v]) []
    sortInts out

/-- Days-of-month (1-based, ascending) selected by BYDAY inside a span of `dim` days. -/
def bydayInSpan (first dim : Int) (byday : List ByDay) : List Int := Id.run do
  let mut out : List Int := []
  let firstWd := weekday first
  for b in byday do
    let some wd := weekdayNum? b.day | continue
    let mut all : Array Int := #[]
    let mut d := 1 + (wd - firstWd + 7) % 7
    while d ≤ dim do
      all := all.push d
      d := d + 7
    if b.ord == 0 then
      for x in all do if !out.contains x then out := out ++ [x]
    else
      let idx : Int := if b.ord > 0 then b.ord - 1 else all.size + b.ord
      if idx ≥ 0 && idx < all.size then
        let x := all[idx.toNat]!
        if !out.contains x then out := out ++ [x]
  return sortInts out

def monthCandidates (y m seedD : Int) (bymonthday : Option (List Int)) (byday : Option (List ByDay)) : List Int :=
  let dim := daysInMonth y m
  let first := dayNum y m 1
  let days : List Int :=
    match bymonthday with
    | some mds =>
      let ds := mds.foldl (fun (acc : List Int) md =>
        let d := if md > 0 then md else dim + 1 + md
        if d ≥ 1 && d ≤ dim && !acc.contains d then acc ++ [d] else acc) []
      match byday with
      | some bd => let limit := bydayInSpan first dim bd; ds.filter limit.contains
      | none => ds
    | none =>
      match byday with
      | some bd => bydayInSpan first dim bd
      | none => if seedD ≤ dim then [seedD] else []
  (sortInts days).map (first + · - 1)

private def nonEmpty {α} (o : Option (List α)) : Option (List α) :=
  match o with | some (x :: xs) => some (x :: xs) | _ => none

/-- Candidate days for period `p` of a rule (ascending), or none once the period starts after
`limitDay` (none: no limit). -/
def periodDays (rule : RRule) (ctx : Ctx) (limitDay : Option Int) (p : Int) : Option (List Int) :=
  let interval : Int := max 1 rule.interval
  let bymonth := nonEmpty rule.bymonth
  let bymonthday := nonEmpty rule.bymonthday
  let byday := nonEmpty rule.byday
  let bysetpos := nonEmpty rule.bysetpos
  let wkst := (weekdayNum? rule.wkst.toUpper).getD 1
  let past (d : Int) : Bool := match limitDay with | some l => decide (d > l) | none => false
  match rule.freq with
  | .daily =>
    let day := ctx.seedDay + p * interval
    if past day then none else
    let (y, m, d) := dayParts day
    let okMonth := match bymonth with | some l => l.contains m | none => true
    let okMd := match bymonthday with
      | some l => let dim := daysInMonth y m; l.any (fun md => (if md > 0 then md else dim + 1 + md) == d)
      | none => true
    let okWd := match byday with
      | some l => l.any (fun b => weekdayNum? b.day == some (weekday day))
      | none => true
    some (if okMonth && okMd && okWd then [day] else [])
  | .weekly =>
    let weekStart0 := ctx.seedDay - (weekday ctx.seedDay - wkst + 7) % 7
    let wds := match byday with
      | some l => l.filterMap (fun (b : ByDay) => weekdayNum? b.day)
      | none => [weekday ctx.seedDay]
    let offsets := sortInts (wds.foldl (fun (acc : List Int) wd =>
      let o := (wd - wkst + 7) % 7
      if acc.contains o then acc else acc ++ [o]) [])
    let ws := weekStart0 + p * 7 * interval
    if past ws then none else
    let cands := offsets.map (ws + ·)
    let cands := match bymonth with
      | some l => cands.filter (fun d => l.contains (dayParts d).2.1)
      | none => cands
    some (applySetPos cands bysetpos)
  | .monthly =>
    let mi0 := ctx.seed.y * 12 + ctx.seed.m - 1
    let mi := mi0 + p * interval
    let y := mi / 12
    let m := mi - y * 12 + 1
    if past (dayNum y m 1) then none else
    if (match bymonth with | some l => !l.contains m | none => false) then some [] else
    some (applySetPos (monthCandidates y m ctx.seed.d bymonthday byday) bysetpos)
  | .yearly =>
    let y := ctx.seed.y + p * interval
    if past (dayNum y 1 1) then none else
    let cands :=
      if bymonth.isSome || bymonthday.isSome || byday.isNone then
        let months := match bymonth with | some l => sortInts l.eraseDups | none => [ctx.seed.m]
        months.flatMap fun m => monthCandidates y m ctx.seed.d bymonthday byday
      else
        let first := dayNum y 1 1
        let diy := dayNum (y + 1) 1 1 - first
        (bydayInSpan first diy (byday.getD [])).map (first + · - 1)
    some (applySetPos cands bysetpos)

/-- The first period index worth generating for `fromDay` (none: -∞, nothing may be skipped). -/
def firstPeriod (rule : RRule) (ctx : Ctx) (fromDay : Option Int) : Int :=
  let interval : Int := max 1 rule.interval
  match fromDay with
  | none => 0
  | some f =>
    match rule.freq with
    | .daily => if f > ctx.seedDay then (f - ctx.seedDay) / interval else 0
    | .weekly =>
      let wkst := (weekdayNum? rule.wkst.toUpper).getD 1
      let weekStart0 := ctx.seedDay - (weekday ctx.seedDay - wkst + 7) % 7
      if f > weekStart0 then (f - weekStart0) / (7 * interval) else 0
    | .monthly =>
      if f > ctx.seedDay then
        let (fy, fm, _) := dayParts f
        let fmi := fy * 12 + fm - 1
        let mi0 := ctx.seed.y * 12 + ctx.seed.m - 1
        if fmi > mi0 then (fmi - mi0) / interval else 0
      else 0
    | .yearly =>
      if f > ctx.seedDay then
        let (fy, _, _) := dayParts f
        if fy > ctx.seed.y then (fy - ctx.seed.y) / interval else 0
      else 0

/-- Walk the recurrence set in ascending start order: the seed (DTSTART is always the first
instance and counts towards COUNT), RRULE instances, and RDATEs merged in. COUNT and UNTIL are
applied here; EXDATEs are not. `visit` returns false to stop. Returns (capped, state). -/
def walkInstances {σ : Type} (ctx : Ctx) (fromDay limitDay : Option Int) (maxInstances : Nat)
    (init : σ) (visit : Instance → σ → Bool × σ) : Bool × σ := Id.run do
  let rdates := sortedRdates ctx
  let mut ri := 0
  let mut st := init
  -- flush RDATEs strictly before `before`; false when the visitor stopped
  let flush (before : Int) (ri : Nat) (st : σ) : Bool × Nat × σ := Id.run do
    let mut ri := ri
    let mut st := st
    while h : ri < rdates.size do
      let r := rdates[ri]
      if r.start ≥ before then break
      ri := ri + 1
      let (go, st') := visit r st
      st := st'
      if !go then return (false, ri, st)
    return (true, ri, st)
  let seed := instanceForDay ctx ctx.seedDay
  let (go, ri', st') := flush seed.start ri st
  ri := ri'; st := st'
  if !go then return (false, st)
  if h : ri < rdates.size then
    if rdates[ri].start == seed.start then ri := ri + 1
  let mut counted : Nat := 1
  let mut generated : Nat := 1
  let (go, st') := visit seed st
  st := st'
  if !go then return (false, st)
  let mut capped := false
  if let some rule := ctx.series.rrule then
    if !(match rule.count with | some c => decide (counted ≥ c) | none => false) then
      let fromDay' := if rule.count.isNone then fromDay else none
      let p0 := firstPeriod rule ctx fromDay'
      let mut exhausted := true
      for i in [0:periodCap] do
        match periodDays rule ctx limitDay (p0 + i) with
        | none => exhausted := false; break
        | some days =>
          let mut stop := false
          for day in days do
            if day ≤ ctx.seedDay then continue
            let inst := instanceForDay ctx day
            if beyondUntil inst rule.until_ ctx then stop := true; break
            let (go, ri', st') := flush inst.start ri st
            ri := ri'; st := st'
            if !go then return (capped, st)
            if h : ri < rdates.size then
              if rdates[ri].start == inst.start then ri := ri + 1
            counted := counted + 1
            generated := generated + 1
            if generated > maxInstances then
              capped := true; stop := true; break
            let (go, st') := visit inst st
            st := st'
            if !go then return (capped, st)
            if (match rule.count with | some c => decide (counted ≥ c) | none => false) then stop := true; break
          if stop then exhausted := false; break
      if exhausted then capped := true
  if !capped then
    while h : ri < rdates.size do
      let r := rdates[ri]
      ri := ri + 1
      let (go, st') := visit r st
      st := st'
      if !go then return (capped, st)
  return (capped, st)

/-- Start instants (epoch ms) of a rule's instances whose span intersects
[windowStart, windowEnd), DTSTART first: the plain-rule view of `Core.Ical.expandOccurrences`
(no RDATE, EXDATE or overrides). A complex rule yields only DTSTART. `durationMs` is the length
of one instance (all-day: whole days). -/
def expand (rule : RRule) (dtstart : DateTime) (windowStart windowEnd : Int) (durationMs : Int := 0)
    (maxInstances : Nat := 2000) : List Int :=
  let endDt : DateTime :=
    if dtstart.allDay then
      let d := isoDate (dayOfIso dtstart.dateOf + max 1 (durationMs / dayMs))
      { dtstart with loc := d ++ "T00:00:00", date := some d }
    else dtstart
  let ctx := context { start := dtstart, end_ := endDt, durationMs, rrule := some rule }
  let inRange (s e : Int) := s < windowEnd && (e > windowStart || s ≥ windowStart)
  if rule.isComplex then
    let seed := instanceForDay ctx ctx.seedDay
    if inRange seed.start seed.end_ then [seed.start] else []
  else
    let fromDay := localDayOf ctx windowStart - ctx.slackDays - 1
    let limitDay := localDayOf ctx windowEnd + 1
    let (_, out) := walkInstances ctx (some fromDay) (some limitDay) maxInstances ([] : List Int) fun inst acc =>
      if inst.start ≥ windowEnd then (false, acc)
      else (true, if inRange inst.start inst.end_ then inst.start :: acc else acc)
    out.reverse

end Core.RRule
