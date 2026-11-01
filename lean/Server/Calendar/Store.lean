import Lean.Data.Json
import Core.Ical
import Core.Mime
import Core.Ulid
import Server.Db
import Server.Crypto
import Server.Log

/-! # Calendar: the `events` table

A port of MailVault's `src/calendar/extract.ts` and `store.ts` without the sealing. Every VEVENT
of a message's calendar parts (`Core.Ical.findCalendarText`) is one row keyed by
`Core.Ical.rowKey uid recurrenceId`, its record in `data` as `Core.Ical.Event.toJson`.

Ordering is iTIP's: a write lands only when its SEQUENCE is higher, or equal with a DTSTAMP not
older (`upsertUnlocked`, the old `ON CONFLICT … WHERE` guard, in SQL as well as in the read
before it). A CANCEL marks the row cancelled and keeps the record it had, so the calendar still
says what was cancelled. A row the person kept (`kept = 1`) keeps its expiry. Methods other than
PUBLISH, REQUEST, ADD and CANCEL (a REPLY, a COUNTER) are not events and are skipped.

`eventOfJson` reads a stored record back; it is the decoder of `test/IcalTest.lean`, copied here
because `ical-test` builds against `Core` only and must not link the server. -/

namespace Server.Calendar

open Lean Server.Db

/-! ## Records from JSON -/

def optStr (j : Json) (k : String) : Option String := (j.getObjValAs? (Option String) k).toOption.join
def optInt (j : Json) (k : String) : Option Int := (j.getObjValAs? (Option Int) k).toOption.join
def jstr (j : Json) (k : String) : String := (j.getObjValAs? String k).toOption.getD ""
def jint (j : Json) (k : String) : Int := (j.getObjValAs? Int k).toOption.getD 0
def jbool (j : Json) (k : String) : Bool := (j.getObjValAs? Bool k).toOption.getD false
def jarr (j : Json) (k : String) : List Json := ((j.getObjValAs? (Array Json) k).toOption.getD #[]).toList
def isNullField (j : Json) (k : String) : Bool :=
  match j.getObjVal? k with | .ok .null => true | .ok _ => false | .error _ => true

def dtOfJson (j : Json) : Core.RRule.DateTime :=
  { utc := optInt j "utc", loc := jstr j "local", date := optStr j "date", tzid := optStr j "tzid",
    tzidRaw := optStr j "tzidRaw", allDay := jbool j "allDay", floating := jbool j "floating" }

def ruleOfJson (j : Json) : Core.RRule.RRule :=
  let ints (k : String) : Option (List Int) := (j.getObjValAs? (Array Int) k).toOption.map (·.toList)
  { freq := match jstr j "freq" with | "WEEKLY" => .weekly | "MONTHLY" => .monthly | "YEARLY" => .yearly | _ => .daily
    interval := (jint j "interval").toNat
    count := (j.getObjValAs? Nat "count").toOption
    until_ := (j.getObjVal? "until").toOption.map fun u => { utc := optInt u "utc", date := optStr u "date" }
    byday := (j.getObjValAs? (Array Json) "byday").toOption.map fun l => l.toList.map fun b => { ord := jint b "ord", day := jstr b "day" }
    bymonthday := ints "bymonthday", bymonth := ints "bymonth", bysetpos := ints "bysetpos"
    wkst := jstr j "wkst"
    unsupported := ((j.getObjValAs? (Array String) "unsupported").toOption.getD #[]).toList }

/-- A stored record (`Core.Ical.Event.toJson`) back as an `Event`. -/
def eventOfJson (j : Json) : Core.Ical.Event :=
  let sub (k : String) : Option Json := if isNullField j k then none else (j.getObjVal? k).toOption
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

/-- The record of a row's `data`; `none` when it does not parse. -/
def eventOfData (s : String) : Option Core.Ical.Event :=
  match Json.parse s with
  | .ok j@(.obj _) => some (eventOfJson j)
  | _ => none

/-! ## Extraction (`extract.ts`) -/

def createMethods : List String := ["PUBLISH", "REQUEST", "ADD"]

structure Extracted where
  rowKey : String
  event : Core.Ical.Event
  method : Option String
  cancelled : Bool
  expiresAt : Int
  deriving Inhabited

def rowKeyOf (e : Core.Ical.Event) : String := Core.Ical.rowKey e.uid (e.recurrenceId.map (·.toDateTime))

/-- The events of a message's calendar parts. Never throws. -/
def extract (atts : Array Core.Mime.Attachment) (receivedAt : Nat) : List Extracted := Id.run do
  let parts : List Core.Ical.Part := atts.toList.map fun a =>
    { mimeType := some a.mimeType, filename := a.filename, method := a.method, content := a.content }
  let mut out : Array Extracted := #[]
  let mut seen : List String := []
  for (text, m) in Core.Ical.findCalendarText parts do
    let cal := Core.Ical.parseCalendar text m
    let method := cal.method
    if let some meth := method then
      if !(createMethods.contains meth || meth == "CANCEL") then continue
    for rec in cal.events do
      let rk := rowKeyOf rec
      let dk := rk ++ ":" ++ toString rec.sequence
      if seen.contains dk then continue
      seen := dk :: seen
      out := out.push { rowKey := rk, event := rec, method
                        cancelled := method == some "CANCEL" || Core.Ical.isCancelled rec
                        expiresAt := Core.Ical.computeEventExpiry rec receivedAt }
  return out.toList

/-- Whether a message's attachments hold any calendar part at all. -/
def hasCalendarPart (atts : Array Core.Mime.Attachment) : Bool :=
  !(Core.Ical.findCalendarText (atts.toList.map fun a =>
    { mimeType := some a.mimeType, filename := a.filename, method := a.method, content := a.content })).isEmpty

/-! ## Upsert (`store.ts`) -/

inductive Outcome where
  | upserted (id : String)
  | cancelled (id : String)
  | skipped
  deriving Repr, BEq, Inhabited

def newId (now : Nat) : IO String := do return Core.Ulid.encode now (← Crypto.randomBytes 10)

/-- One extracted event into `events`, inside the caller's transaction (the `Unlocked` calls).
`source` is `mail`, `manual` or `import`; `messageId` the message row it came in. -/
def upsertUnlocked (db : Db) (ev : Extracted) (source : String) (messageId : Option String) (receivedAt now : Nat)
    (kept : Bool := false) : IO Outcome := do
  let rec_ := ev.event
  let seq := rec_.sequence
  let stamp := rec_.stamp
  let existing := (← db.queryUnlocked "SELECT id, sequence, stamp FROM events WHERE row_key = ?" #[ev.rowKey])[0]?
  let data := rec_.toJson
  let expires : Option Int := if kept then none else some ev.expiresAt
  if ev.cancelled then
    match existing with
    | some r =>
      if seq < r.nat "sequence" then return .skipped
      -- the record stays as it was: the calendar still says what was cancelled
      db.execUnlocked "UPDATE events SET cancelled = 1, sequence = MAX(sequence, ?1), stamp = MAX(stamp, ?2),
          message_id = COALESCE(?3, message_id), updated_at = ?4, deleted = 0, deleted_at = NULL WHERE id = ?5"
        #[seq, stamp, (messageId : Value), now, r.text "id"]
      return .cancelled (r.text "id")
    | none =>
      let id : String ← newId now
      db.execUnlocked "INSERT INTO events (id, row_key, uid, sequence, stamp, cancelled, source, message_id, received_at,
          expires_at, kept, data, updated_at) VALUES (?, ?, ?, ?, ?, 1, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT(row_key) DO NOTHING"
        #[id, ev.rowKey, rec_.uid, seq, stamp, source, (messageId : Value), receivedAt, (expires : Value), kept, data, now]
      return .cancelled id
  if let some r := existing then
    if seq < r.nat "sequence" || (seq == r.nat "sequence" && stamp < (r.int "stamp").toInt) then return .skipped
  let id : String ← match existing with
    | some r => pure (r.text "id")
    | none => newId now
  db.execUnlocked "INSERT INTO events (id, row_key, uid, sequence, stamp, cancelled, source, message_id, received_at,
      expires_at, kept, data, updated_at, deleted)
    VALUES (?1, ?2, ?3, ?4, ?5, 0, ?6, ?7, ?8, ?9, ?10, ?11, ?12, 0)
    ON CONFLICT(row_key) DO UPDATE SET
      uid = excluded.uid, sequence = excluded.sequence, stamp = excluded.stamp, cancelled = 0, data = excluded.data,
      message_id = COALESCE(excluded.message_id, events.message_id),
      expires_at = CASE WHEN events.kept = 1 THEN events.expires_at ELSE excluded.expires_at END,
      updated_at = excluded.updated_at, deleted = 0, deleted_at = NULL
    WHERE excluded.sequence > events.sequence OR (excluded.sequence = events.sequence AND excluded.stamp >= events.stamp)"
    #[id, ev.rowKey, rec_.uid, seq, stamp, source, (messageId : Value), receivedAt, (expires : Value), kept, data, now]
  return .upserted id

/-- Every event of a message, inside the ingest transaction. The number written. -/
def storeFromMessageUnlocked (db : Db) (messageId : String) (atts : Array Core.Mime.Attachment) (receivedAt now : Nat) :
    IO Nat := do
  let evs := extract atts receivedAt
  let mut n : Nat := 0
  for ev in evs do
    match ← upsertUnlocked db ev "mail" (some messageId) receivedAt now with
    | .skipped => pure ()
    | _ => n := n + 1
  if !evs.isEmpty then
    Log.info "calendar.upsert" [("events", Json.num evs.length), ("written", Json.num n)]
  return n

end Server.Calendar
