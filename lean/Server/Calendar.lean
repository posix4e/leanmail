import Lean.Data.Json
import Core.Ical
import Core.RRule
import Server.State
import Server.Router
import Server.Clock
import Server.Log
import Server.Calendar.Store

/-! # Calendar: occurrences and the events API

| route | auth | |
|---|---|---|
| `GET /api/events?from&to&tz` | token `read` | occurrences in [from, to) (epoch ms or `YYYY-MM-DD`, UTC), with the person's answers |
| `GET /api/events/:id` | token `read` | one row: its record, its override, its message |
| `POST /api/events` | token `act` | a manual event `{summary, start, end?, all_day?, location?, description?, rrule?, tz?, kept?}` |
| `PUT /api/events/:id` | token `act` | replace a manual event (same body), SEQUENCE + 1 |
| `PATCH /api/events/:id` | token `act` | `{kept?, expires_at?, partstat?, notes?, hidden?}` |
| `DELETE /api/events/:id` | token `act` | soft delete |
| `POST /api/events/batch` | token `act` | `{events:[{record, received_at?, expires_at?, kept?, cancelled?}]}`, ≤ 200, source `import` |

Occurrences come from `Core.Ical.expandOccurrences`: each master row (no RECURRENCE-ID) with the
override rows of the same UID; an override row whose master is not held shows by itself. The
person's own answer (`event_overrides.partstat`) wins over the invitation's PARTSTAT for them;
`hidden` occurrences are left out unless asked for. Nothing is sent to the organizer: an RSVP is
kept here only, as it was in MailVault. -/

namespace Server.Calendar

open Lean Server.Db Server.Router

def dayMs : Int := 86400000
def windowMax : Int := 400 * dayMs
def partstats : List String := ["ACCEPTED", "TENTATIVE", "DECLINED", "NEEDS-ACTION"]

/-! ## Reading -/

/-- An events row, its record, and its override columns. -/
structure Held where
  row : Row
  ev : Core.Ical.Event

def heldSql : String :=
  "SELECT e.*, o.partstat AS o_partstat, o.notes AS o_notes, o.hidden AS o_hidden FROM events e
   LEFT JOIN event_overrides o ON o.event_id = e.id WHERE e.deleted = 0"

def loadHeld (st : State) (extra : String := "") (args : Array Value := #[]) : IO (Array Held) := do
  let rows ← st.db.query (heldSql ++ extra) args
  return rows.filterMap fun r => (eventOfData (r.text "data")).map fun ev => { row := r, ev }

def heldById (st : State) (id : String) : IO (Option Held) := do
  return (← loadHeld st " AND e.id = ?" #[id])[0]?

/-- The account's own addresses, lower-case: who "me" is among an invitation's attendees. -/
def ownAddresses (st : State) : IO (List String) := do
  let acct ← st.db.queryOne "SELECT email, default_from FROM account WHERE id = 1"
  let rows ← st.db.query "SELECT address FROM addresses WHERE address NOT LIKE '*@%'"
  return ([(acct.map (·.text "email")).getD "", (acct.map (·.text "default_from")).getD ""] ++
    rows.toList.map (·.text "address")).map (·.toLower) |>.filter (· ≠ "") |>.eraseDups

/-- The attendee that is the account, if any. -/
def me? (ev : Core.Ical.Event) (own : List String) : Option Core.Ical.Attendee :=
  ev.attendees.find? fun a => own.contains a.email.toLower

/-- The person's answer: their own override, else the invitation's PARTSTAT for them. -/
def partstatOf (h : Held) (own : List String) : Option String :=
  match h.row.text? "o_partstat" with
  | some p => if p.isEmpty then none else some p
  | none => (me? h.ev own).map (·.partstat)

structure Occ where
  held : Held
  occ : Core.Ical.Occurrence
  partstat : Option String

/-- Every occurrence in [from, to), sorted by start. `tz` names the zone of each `dateKey`. -/
def occurrences (st : State) (from_ to_ : Int) (tz : Option String := some "UTC") (withHidden : Bool := false) :
    IO (Array Occ) := do
  let held ← loadHeld st
  let own ← ownAddresses st
  let masters := held.filter (·.ev.recurrenceId.isNone)
  let overrides := held.filter (·.ev.recurrenceId.isSome)
  let rid (h : Held) := Core.RRule.ridKey (h.ev.recurrenceId.map (·.toDateTime))
  let mut out : Array Occ := #[]
  for m in masters do
    let mine := overrides.filter (·.ev.uid == m.ev.uid)
    for o in Core.Ical.expandOccurrences m.ev (mine.toList.map (·.ev)) from_ to_ tz do
      let h := if o.isOverride then ((mine.find? fun x => rid x == Core.RRule.ridKey o.recurrenceId).getD m) else m
      out := out.push { held := h, occ := o, partstat := partstatOf h own }
  for o in overrides do
    if masters.any (·.ev.uid == o.ev.uid) then continue
    for x in Core.Ical.expandOccurrences o.ev [] from_ to_ tz do
      out := out.push { held := o, occ := x, partstat := partstatOf o own }
  let shown := if withHidden then out else out.filter (!·.held.row.bool "o_hidden")
  return shown.qsort fun a b => a.occ.start < b.occ.start || (a.occ.start == b.occ.start && a.occ.end_ < b.occ.end_)

/-- The first occurrence of a record from its first start, for "when" on one event. -/
def firstOccurrence (ev : Core.Ical.Event) : Option Core.Ical.Occurrence :=
  let s := Core.Ical.firstStart ev
  (Core.Ical.expandOccurrences ev [] (s - dayMs) (s + 800 * dayMs) (some "UTC") 1).head?

def optJ (o : Option String) : Json := match o with | some s => Json.str s | none => Json.null
def optN (o : Option Nat) : Json := match o with | some n => Json.num n | none => Json.null

def occJson (o : Occ) : Json :=
  let r := o.held.row
  let e := o.held.ev
  Json.mkObj [
    ("event_id", Json.str (r.text "id")), ("uid", Json.str e.uid),
    ("start", toJson o.occ.start), ("end", toJson o.occ.end_), ("all_day", Json.bool o.occ.allDay),
    ("date", Json.str o.occ.dateKey), ("summary", Json.str e.summary), ("location", Json.str e.location),
    ("cancelled", Json.bool (o.occ.cancelled || r.bool "cancelled")), ("complex", Json.bool o.occ.complex),
    ("is_override", Json.bool o.occ.isOverride), ("recurrence_id", Json.str (Core.RRule.ridKey o.occ.recurrenceId)),
    ("message_id", optJ (r.text? "message_id")), ("source", Json.str (r.text "source")),
    ("organizer", optJ (e.organizer.map (·.email))),
    ("partstat", optJ o.partstat), ("notes", Json.str (r.text "o_notes")), ("hidden", Json.bool (r.bool "o_hidden"))]

def heldJson (h : Held) (own : List String) : Json :=
  let r := h.row
  Json.mkObj [
    ("id", Json.str (r.text "id")), ("row_key", Json.str (r.text "row_key")), ("uid", Json.str (r.text "uid")),
    ("sequence", Json.num (r.nat "sequence")), ("cancelled", Json.bool (r.bool "cancelled")),
    ("source", Json.str (r.text "source")), ("message_id", optJ (r.text? "message_id")),
    ("received_at", Json.num (r.nat "received_at")), ("expires_at", optN (r.nat? "expires_at")),
    ("kept", Json.bool (r.bool "kept")), ("updated_at", Json.num (r.nat "updated_at")),
    ("record", (Json.parse (r.text "data")).toOption.getD Json.null),
    ("override", Json.mkObj [("partstat", optJ (partstatOf h own)), ("notes", Json.str (r.text "o_notes")),
      ("hidden", Json.bool (r.bool "o_hidden"))])]

/-! ## Writing -/

/-- Merge into the event's override; `none` leaves a field as it is. `some ""` clears a partstat. -/
def setOverride (st : State) (eventId : String) (partstat : Option String) (notes : Option String) (hidden : Option Bool) :
    IO Bool := do
  let now ← Clock.nowMs
  st.db.transaction do
    if (← st.db.queryUnlocked "SELECT 1 AS x FROM events WHERE id = ? AND deleted = 0" #[eventId]).isEmpty then return false
    let cur := (← st.db.queryUnlocked "SELECT partstat, notes, hidden FROM event_overrides WHERE event_id = ?" #[eventId])[0]?
    let p : Option String := match partstat with
      | some "" => none
      | some x => some x
      | none => cur.bind (·.text? "partstat")
    let n := notes.getD ((cur.map (·.text "notes")).getD "")
    let hd := hidden.getD ((cur.map (·.bool "hidden")).getD false)
    st.db.execUnlocked "INSERT INTO event_overrides (event_id, partstat, notes, hidden, updated_at) VALUES (?1, ?2, ?3, ?4, ?5)
      ON CONFLICT(event_id) DO UPDATE SET partstat = ?2, notes = ?3, hidden = ?4, updated_at = ?5"
      #[eventId, (p : Value), n, hd, now]
    st.db.execUnlocked "UPDATE events SET updated_at = ? WHERE id = ?" #[now, eventId]
    return true

/-- `YYYY-MM-DD` → its UTC midnight. -/
def dayOf? (s : String) : Option Int :=
  if s.length == 10 && (Core.RRule.parseLocal? (s ++ "T00:00:00")).isSome then some (Core.RRule.dayOfIso s * dayMs) else none

/-- A window bound: epoch ms or `YYYY-MM-DD`. -/
def boundOf? (s : String) : Option Int :=
  match s.toNat? with
  | some n => some n
  | none => dayOf? s

/-- A manual event from the API's body. `uid` and `sequence` come from the row it replaces. -/
def manualEvent (j : Json) (uid : String) (sequence : Nat) (now : Nat) : Except String Core.Ical.Event := do
  let summary := Core.Text.Str.trim ((jstr? j "summary").getD "")
  if summary.isEmpty then throw "summary required"
  let tz := (jstr? j "tz").bind fun t => Core.RRule.canonicalTimeZone (some t)
  let allDay := (jbool? j "all_day").getD false
  let startJ := (j.getObjVal? "start").toOption.getD .null
  let endJ := (j.getObjVal? "end").toOption.getD .null
  let msOf (v : Json) : Option Int := match v with
    | .num _ => (v.getNat?).toOption.map Int.ofNat
    | .str s =>
      if s.length == 10 then dayOf? s
      else if (Core.RRule.parseLocal? s).isSome then some (Core.RRule.zonedToUtc (String.ofList (s.toList.take 19)) tz)
      else none
    | _ => none
  let some s := msOf startJ | throw "bad start"
  let dt (ms : Int) : Core.RRule.DateTime :=
    if allDay then
      let d := Core.RRule.take10 (Core.RRule.utcToZoned ms none)
      { loc := d ++ "T00:00:00", date := some d, allDay := true }
    else { utc := some ms, loc := Core.RRule.utcToZoned ms tz, tzid := some (tz.getD "UTC") }
  let s := if allDay then (s / dayMs) * dayMs else s
  let e : Int := match msOf endJ with
    | some e => if allDay then max ((e / dayMs) * dayMs) (s + dayMs) else max e s
    | none => if allDay then s + dayMs else s + 3600000
  let rrule ← match jstr? j "rrule" with
    | some r =>
      let r := Core.Text.Str.trim r
      if r.isEmpty then pure none else
      match Core.RRule.parse (if r.toUpper.startsWith "RRULE:" then String.ofList (r.toList.drop 6) else r) tz with
      | .ok x => pure (some (x, r))
      | .error err => throw s!"bad rrule: {err}"
    | none => pure none
  return { uid, sequence, stamp := now / 1000, summary
           location := (jstr? j "location").getD "", description := (jstr? j "description").getD ""
           start := dt s, end_ := dt e, durationMs := e - s
           rrule := rrule.map (·.1), rruleRaw := rrule.map (·.2) }

/-- `expires_at` from a body: `never`/`null` keeps forever; a number is that time. -/
def expiryOf? (j : Json) : Option (Option Nat) :=
  match j.getObjVal? "expires_at" with
  | .ok .null => some none
  | .ok (.str "never") => some none
  | .ok v => (v.getNat?).toOption.map some
  | .error _ => none

/-! ## Routes -/

def list (c : Ctx) (req : Req) : IO Resp := do
  let now ← Clock.nowMs
  let today : Int := (now / 86400000 : Nat) * dayMs
  let some f := ((req.param? "from").map boundOf?).getD (some today) | return Resp.error 400 "bad from"
  let some t := ((req.param? "to").map boundOf?).getD (some (f + 31 * dayMs)) | return Resp.error 400 "bad to"
  if t ≤ f || t - f > windowMax then return Resp.error 400 "bad window"
  let tz := (req.param? "tz").bind fun z => Core.RRule.canonicalTimeZone (some z)
  let occs ← occurrences c.st f t (tz.orElse fun _ => some "UTC") (req.param? "hidden" == some "1")
  return Resp.ok (Json.mkObj [("from", toJson f), ("to", toJson t), ("occurrences", Json.arr (occs.map occJson))])

def getOne (c : Ctx) (_ : Req) : IO Resp := do
  let some h ← heldById c.st (c.param "id") | return Resp.notFound
  return Resp.ok (heldJson h (← ownAddresses c.st))

def create (c : Ctx) (req : Req) : IO Resp := do
  let now ← Clock.nowMs
  let j := req.json
  let id ← newId now
  match manualEvent j (id ++ "@leanmail") 0 now with
  | .error e => return Resp.error 400 e
  | .ok ev =>
    let kept := expiryOf? j == some none || jbool? j "kept" == some true
    let exp : Int := match expiryOf? j with
      | some (some e) => e
      | _ => Core.Ical.computeEventExpiry ev now
    let x : Extracted := { rowKey := rowKeyOf ev, event := ev, method := none, cancelled := false, expiresAt := exp }
    let out ← c.st.db.transaction (upsertUnlocked c.st.db x "manual" none now now kept)
    match out with
    | .upserted i => return Resp.json 201 (Json.mkObj [("id", Json.str i), ("row_key", Json.str x.rowKey)])
    | _ => return Resp.error 409 "conflict"

def replace (c : Ctx) (req : Req) : IO Resp := do
  let now ← Clock.nowMs
  let id := c.param "id"
  let some h ← heldById c.st id | return Resp.notFound
  if h.row.text "source" != "manual" then return Resp.error 409 "not_manual"
  match manualEvent req.json h.ev.uid (h.ev.sequence + 1) now with
  | .error e => return Resp.error 400 e
  | .ok ev =>
    let n ← c.st.db.execCount "UPDATE events SET data = ?, sequence = ?, stamp = ?, cancelled = 0,
        expires_at = CASE WHEN kept = 1 THEN expires_at ELSE ? END, updated_at = ?
      WHERE id = ? AND source = 'manual' AND deleted = 0"
      #[ev.toJson, ev.sequence, ev.stamp, Core.Ical.computeEventExpiry ev now, now, id]
    if n == 0 then return Resp.notFound
    return Resp.ok (Json.mkObj [("id", Json.str id), ("sequence", Json.num ev.sequence)])

def patch (c : Ctx) (req : Req) : IO Resp := do
  let now ← Clock.nowMs
  let id := c.param "id"
  let j := req.json
  let some _ ← heldById c.st id | return Resp.notFound
  if let some e := expiryOf? j then
    discard <| c.st.db.execCount "UPDATE events SET expires_at = ?, kept = ?, updated_at = ? WHERE id = ? AND deleted = 0"
      #[(e : Value), e.isNone || jbool? j "kept" == some true, now, id]
  else if let some k := jbool? j "kept" then
    discard <| c.st.db.execCount "UPDATE events SET kept = ?, updated_at = ? WHERE id = ? AND deleted = 0" #[k, now, id]
  let partstat : Option String ← match j.getObjVal? "partstat" with
    | .ok .null => pure (some "")
    | .ok (.str p) =>
      let p := p.toUpper
      if partstats.contains p then pure (some p) else return Resp.error 400 "bad partstat"
    | _ => pure none
  let notes := (jstr? j "notes").map fun s => String.ofList (s.toList.take 4000)
  let hidden := jbool? j "hidden"
  if partstat.isSome || notes.isSome || hidden.isSome then
    discard <| setOverride c.st id partstat notes hidden
  let some h ← heldById c.st id | return Resp.notFound
  return Resp.ok (heldJson h (← ownAddresses c.st))

def remove (c : Ctx) (_ : Req) : IO Resp := do
  let now ← Clock.nowMs
  let n ← c.st.db.execCount "UPDATE events SET deleted = 1, deleted_at = ?1, updated_at = ?1 WHERE id = ?2 AND deleted = 0"
    #[now, c.param "id"]
  if n == 0 then return Resp.notFound
  return Resp.ok (Json.mkObj [("id", Json.str (c.param "id")), ("deleted_at", Json.num now)])

def batch (c : Ctx) (req : Req) : IO Resp := do
  let now ← Clock.nowMs
  let some items := (req.json.getObjValAs? (Array Json) "events").toOption | return Resp.error 400 "events required"
  if items.isEmpty || items.size > 200 then return Resp.error 400 "1 to 200 events"
  let mut upserted : Nat := 0
  let mut ignored : Nat := 0
  let mut ids : Array Json := #[]
  for it in items do
    let some rj := (it.getObjVal? "record").toOption | return Resp.error 400 "record required"
    let ev := eventOfJson rj
    if ev.uid.isEmpty || ev.start.loc.isEmpty then return Resp.error 400 "bad record"
    let receivedAt := (it.getObjValAs? Nat "received_at").toOption.getD now
    let kept := expiryOf? it == some none || jbool? it "kept" == some true
    let exp : Int := match expiryOf? it with
      | some (some e) => e
      | _ => Core.Ical.computeEventExpiry ev receivedAt
    let x : Extracted := { rowKey := rowKeyOf ev, event := ev, method := ev.method
                           cancelled := (jbool? it "cancelled").getD (Core.Ical.isCancelled ev), expiresAt := exp }
    match ← c.st.db.transaction (upsertUnlocked c.st.db x "import" none receivedAt now kept) with
    | .skipped => ignored := ignored + 1
    | .upserted i | .cancelled i => upserted := upserted + 1; ids := ids.push (Json.str i)
  Log.info "events.batch" [("upserted", Json.num upserted), ("ignored", Json.num ignored)]
  return Resp.ok (Json.mkObj [("upserted", Json.num upserted), ("ignored", Json.num ignored), ("ids", Json.arr ids)])

def routes : Array Route := #[
  route "GET" "/api/events" (.token "read") list,
  route "POST" "/api/events" (.token "act") create,
  route "POST" "/api/events/batch" (.token "act") batch,
  route "GET" "/api/events/:id" (.token "read") getOne,
  route "PUT" "/api/events/:id" (.token "act") replace,
  route "PATCH" "/api/events/:id" (.token "act") patch,
  route "DELETE" "/api/events/:id" (.token "act") remove
]

end Server.Calendar
