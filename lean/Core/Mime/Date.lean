import Core.Text.Str

/-! # Dates: RFC 5322 `Date` headers to epoch milliseconds, and back

`parse?` reads RFC 5322 dates (optional day name, 1–2 digit day, month name, 2/3/4 digit year,
`HH:MM[:SS]`, zone as `±HHMM`, `±HH:MM`, a named zone, `GMT+0200`, or a military letter), tolerates
comments and extra whitespace, and also the forms JavaScript's `Date.parse` accepts that mail
clients emit: `Sep 13 2026`, asctime order, `AM`/`PM`, and ISO 8601. A date with no zone is read
as UTC (JavaScript would use the local zone). Dates before 1970 are `none`. Calendar arithmetic is
Howard Hinnant's days-from-civil. -/

namespace Core.Mime.Date

open Core.Text Str

/-- Days since 1970-01-01 of a proleptic Gregorian date. -/
def daysFromCivil (y : Int) (m d : Nat) : Int :=
  let y := if m ≤ 2 then y - 1 else y
  let era := (if y ≥ 0 then y else y - 399) / 400
  let yoe := y - era * 400
  let mp : Int := if m > 2 then m - 3 else m + 9
  let doy := (153 * mp + 2) / 5 + d - 1
  let doe := yoe * 365 + yoe / 4 - yoe / 100 + doy
  era * 146097 + doe - 719468

/-- The (year, month, day) of a day count since 1970-01-01. -/
def civilFromDays (z : Int) : Int × Nat × Nat :=
  let z := z + 719468
  let era := (if z ≥ 0 then z else z - 146096) / 146097
  let doe := z - era * 146097
  let yoe := (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
  let y := yoe + era * 400
  let doy := doe - (365 * yoe + yoe / 4 - yoe / 100)
  let mp := (5 * doy + 2) / 153
  let d := doy - (153 * mp + 2) / 5 + 1
  let m := if mp < 10 then mp + 3 else mp - 9
  ((if m ≤ 2 then y + 1 else y), m.toNat, d.toNat)

def isLeap (y : Int) : Bool := (y % 4 == 0 && y % 100 != 0) || y % 400 == 0

def daysInMonth (y : Int) (m : Nat) : Nat :=
  match m with
  | 2 => if isLeap y then 29 else 28
  | 4 | 6 | 9 | 11 => 30
  | _ => 31

private def monthNames : Array String :=
  #["january", "february", "march", "april", "may", "june", "july", "august", "september",
    "october", "november", "december"]

private def dayNames : Array String :=
  #["sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday"]

/-- A month name, full or abbreviated to at least three letters (`sep`, `sept`, `september`). -/
private def monthOf (w : String) : Option Nat :=
  if w.length < 3 then none else
  (monthNames.findIdx? fun n => n.startsWith w).map (· + 1)

private def isDayName (w : String) : Bool :=
  w.length ≥ 3 && dayNames.any fun n => n.startsWith w

/-- Named zones, offset in minutes east of UTC. -/
private def namedZone (w : String) : Option Int :=
  match w with
  | "ut" | "gmt" | "utc" | "z" | "uct" => some 0
  | "est" => some (-300) | "edt" => some (-240)
  | "cst" => some (-360) | "cdt" => some (-300)
  | "mst" => some (-420) | "mdt" => some (-360)
  | "pst" => some (-480) | "pdt" => some (-420)
  | _ =>
    -- RFC 5322: military zones are unreliable, read as +0000
    if w.length == 1 && w != "j" && (w.toList.headD ' ').isAlpha then some 0 else none

private def digitsVal (cs : List Char) : Option Nat :=
  if !cs.isEmpty && cs.all isDigit then some (cs.foldl (fun n c => n * 10 + (c.toNat - 48)) 0) else none

/-- `±HHMM`, `±HH:MM` or `±HH` (sign included in the input). -/
private def offsetOf (cs : List Char) : Option Int :=
  match cs with
  | s :: rest =>
    if s != '+' && s != '-' then none else
    let rest := rest.filter (· != ':')
    let v : Option Nat :=
      if rest.length == 4 then do
        let h ← digitsVal (rest.take 2)
        let m ← digitsVal (rest.drop 2)
        if h ≤ 23 && m ≤ 59 then some (h * 60 + m) else none
      else if rest.length ≤ 2 then do
        let h ← digitsVal rest
        if h ≤ 23 then some (h * 60) else none
      else none
    v.map fun v => if s == '-' then -(v : Int) else v
  | [] => none

/-- Remove `(comments)`, which may nest. -/
private def stripComments (s : String) : String := Id.run do
  let mut out := ""
  let mut depth := 0
  for c in s.toList do
    if c == '(' then depth := depth + 1; out := out.push ' '
    else if c == ')' && depth > 0 then depth := depth - 1
    else if depth == 0 then out := out.push c
  return out

/-- Milliseconds since the epoch, or `none` before 1970 or for an invalid date. -/
def toEpochMs (y : Int) (mo d h mi s ms : Nat) (offsetMin : Int) : Option Nat :=
  if mo < 1 || mo > 12 || d < 1 || d > daysInMonth y mo || h > 23 || mi > 59 || s > 59 then none else
  let days := daysFromCivil y mo d
  let total : Int := ((days * 24 + h) * 60 + mi - offsetMin) * 60000 + s * 1000 + ms
  if total < 0 then none else some total.toNat

/-- `HH:MM[:SS[.fff]]` as (hour, minute, second, millisecond). -/
private def timeOf (cs : List Char) : Option (Nat × Nat × Nat × Nat) := do
  let (main, frac) := match cs.splitOn '.' with
    | [a] => (a, [])
    | [a, f] => (a, f)
    | _ => ([], [])
  let parts := main.splitOn ':'
  let nums ← parts.mapM fun p => if p.length ≤ 2 then digitsVal p else none
  let ms ← if frac.isEmpty then some 0 else
    (digitsVal frac).map fun _ => (digitsVal ((frac ++ ['0', '0', '0']).take 3)).getD 0
  match nums with
  | [h, m] => some (h, m, 0, ms)
  | [h, m, s] => some (h, m, s, ms)
  | _ => none

/-- ISO 8601: `YYYY-MM-DD[(T| )HH:MM[:SS[.fff]][Z|±HH:MM|±HHMM]]`. -/
private def parseIso (s : String) : Option Nat := do
  let cs := (Str.trim s).toList
  if cs.length < 10 then none
  let y ← digitsVal (cs.take 4)
  if cs[4]? != some '-' || cs[7]? != some '-' then none
  let mo ← digitsVal ((cs.drop 5).take 2)
  let d ← digitsVal ((cs.drop 8).take 2)
  let rest := cs.drop 10
  match rest with
  | [] => toEpochMs y mo d 0 0 0 0 0
  | sep :: rest =>
    if sep != 'T' && sep != 't' && sep != ' ' then none
    -- the time runs until a zone designator
    let timePart := rest.takeWhile fun c => isDigit c || c == ':' || c == '.'
    let zone := rest.drop timePart.length
    let (h, mi, sec, ms) ← timeOf timePart
    let off ← match zone with
      | [] => some 0
      | ['Z'] | ['z'] => some 0
      | z => offsetOf z
    toEpochMs y mo d h mi sec ms off

private structure Acc where
  day : Option Nat := none
  month : Option Nat := none
  year : Option Int := none
  time : Option (Nat × Nat × Nat × Nat) := none
  zone : Option Int := none
  pm : Option Bool := none

private def fullYear (digits : Nat) (v : Nat) : Int :=
  if digits ≤ 2 then (if v < 50 then 2000 + v else 1900 + v)
  else if digits == 3 then 1900 + v
  else v

/-- Classify one word of an RFC 5322-style date. -/
private def step (a : Acc) (w : String) : Option Acc :=
  let cs := w.toList
  let lw := w.toLower
  match cs with
  | [] => some a
  | c :: _ =>
    if c == '+' || (c == '-' && cs.length > 1) then
      (offsetOf cs).map fun o => { a with zone := some o }
    else if isDigit c then
      if cs.contains ':' then
        -- a time, possibly with a zone glued on: 14:32:12+0000, 14:32Z
        let t := cs.takeWhile fun c => isDigit c || c == ':' || c == '.'
        let z := cs.drop t.length
        match timeOf t with
        | none => none
        | some tm =>
          let a := { a with time := some tm }
          if z.isEmpty then some a
          else if z == ['Z'] || z == ['z'] then some { a with zone := some 0 }
          else (offsetOf z).map fun o => { a with zone := some o }
      else
        match digitsVal cs with
        | none => none
        | some v =>
          if a.day.isNone && cs.length ≤ 2 then some { a with day := some v }
          else if a.year.isNone then some { a with year := some (fullYear cs.length v) }
          else if a.day.isNone then some { a with day := some v }
          else none
    else if c.isAlpha then
      -- GMT+0200, UTC-05:00
      let letters := cs.takeWhile Char.isAlpha
      let tail := cs.drop letters.length
      let name := (String.ofList letters).toLower
      if !tail.isEmpty then
        match namedZone name, offsetOf tail with
        | some _, some o => some { a with zone := some o }
        | _, _ => none
      else if let some m := monthOf lw then
        if a.month.isNone then some { a with month := some m } else none
      else if isDayName lw then some a
      else if lw == "am" then some { a with pm := some false }
      else if lw == "pm" then some { a with pm := some true }
      else if let some z := namedZone lw then some { a with zone := some z }
      else none
    else none

/-- Parse an RFC 5322 date (or one of the broken forms mail carries) to epoch milliseconds. -/
def parse? (s : String) : Option Nat :=
  match parseIso s with
  | some v => some v
  | none => do
    let cleaned := stripComments s
    let words := (cleaned.toList.map fun c => if c == ',' || isSpaceLike c then ' ' else c)
      |> String.ofList |>.splitOn " " |>.filter (!·.isEmpty)
    let a ← words.foldlM step {}
    let day ← a.day
    let month ← a.month
    let year ← a.year
    let (h, mi, sec, ms) := a.time.getD (0, 0, 0, 0)
    let h ← match a.pm with
      | none => some h
      | some pm => if h < 1 || h > 12 then none else some ((h % 12) + if pm then 12 else 0)
    toEpochMs year month day h mi sec ms (a.zone.getD 0)

private def pad (width : Nat) (n : Nat) : String :=
  let s := toString n
  repeatChar '0' (width - s.length) ++ s

private def parts (ms : Nat) : Int × Nat × Nat × Nat × Nat × Nat × Nat × Nat :=
  let days := ms / 86400000
  let rem := ms % 86400000
  let (y, m, d) := civilFromDays days
  (y, m, d, rem / 3600000, rem / 60000 % 60, rem / 1000 % 60, rem % 1000, (days + 4) % 7)

private def shortDays : Array String := #["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
private def shortMonths : Array String :=
  #["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

/-- `Sun, 13 Sep 2026 14:32:12 +0000`, in UTC. -/
def formatRfc5322 (ms : Nat) : String :=
  let (y, m, d, h, mi, s, _, wd) := parts ms
  s!"{shortDays[wd]!}, {pad 2 d} {shortMonths[m - 1]!} {pad 4 y.toNat} {pad 2 h}:{pad 2 mi}:{pad 2 s} +0000"

/-- `2026-09-13T14:32:12.000Z`, as JavaScript's `toISOString`. -/
def formatIso (ms : Nat) : String :=
  let (y, m, d, h, mi, s, frac, _) := parts ms
  s!"{pad 4 y.toNat}-{pad 2 m}-{pad 2 d}T{pad 2 h}:{pad 2 mi}:{pad 2 s}.{pad 3 frac}Z"

/-- postal-mime's `formatDate`: `Intl.DateTimeFormat('default', {year, month, day, hour, minute,
second: 'numeric', hour12: false})` under the `en-US` locale in UTC, as `9/13/2026, 14:32:12` (month
and day unpadded, the time zero-padded on a 24-hour clock with midnight as `00`). -/
def formatIntl (ms : Nat) : String :=
  let (y, m, d, h, mi, s, _, _) := parts ms
  s!"{m}/{d}/{y}, {pad 2 h}:{pad 2 mi}:{pad 2 s}"

end Core.Mime.Date
