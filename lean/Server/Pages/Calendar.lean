import Core.Mime.Date
import Server.Pages.Common
import Server.Pages.Settings
import Server.Pages.Message
import Server.Pages.Invite
import Server.Calendar

/-! # Pages: the calendar

`/calendar[?month=YYYY-MM]` is a month grid (Sunday first, six weeks, UTC days) and an agenda of
the next 14 days; every occurrence links to its event, and to the message it came in when there is
one; mail events carry RSVP buttons that set the person's answer (`event_overrides.partstat`,
kept here only: nothing is sent to the organizer). `/calendar/<id>` is one event with its
guests, notes, RSVP, keep-forever, hide and (manual events) delete. `POST /calendar/new` adds a
manual event from a small form. -/

namespace Server.Pages

open Lean Core.Html Server.Db Server.Router

/-- `YYYY-MM` → (year, month), else none. -/
def parseMonth? (s : String) : Option (Int × Nat) := do
  let ps := s.splitOn "-"
  if ps.length != 2 then none
  let y ← ps[0]!.toNat?
  let m ← ps[1]!.toNat?
  if m < 1 || m > 12 || y < 1970 || y > 9999 then none
  some (y, m)

def monthKey (y : Int) (m : Nat) : String := s!"{y}-{pad2 m}"
def prevMonth (y : Int) (m : Nat) : Int × Nat := if m == 1 then (y - 1, 12) else (y, m - 1)
def nextMonth (y : Int) (m : Nat) : Int × Nat := if m == 12 then (y + 1, 1) else (y, m + 1)

def occItem (o : Calendar.Occ) (back : String) : Node :=
  let r := o.held.row
  let id := r.text "id"
  let e := o.held.ev
  let cancelled := o.occ.cancelled || r.bool "cancelled"
  let title := if e.summary.isEmpty then "(no title)" else e.summary
  el "li" [("class", if cancelled then "occ cancelled" else "occ"), ("data-event", id)] [
    elt "span" [("class", "occ-time")] (calWhen o.occ),
    elt "a" [("href", s!"/calendar/{id}"), ("class", "occ-title")] (if cancelled then s!"Cancelled: {title}" else title),
    when (!e.location.isEmpty) (elt "span" [("class", "occ-where")] e.location),
    match r.text? "message_id" with
    | some m => elt "a" [("href", s!"/m/{m}"), ("class", "occ-msg")] "Message"
    | none => empty,
    when (o.partstat.isSome) (badge "rsvp" (partLabel (o.partstat.getD ""))),
    when (isInvite o.held && !cancelled) (rsvpForm id o.partstat back)]

def monthGrid (y : Int) (m : Nat) (occs : Array Calendar.Occ) (today : Int) : Node := Id.run do
  let d0 := Core.Mime.Date.daysFromCivil y m 1
  let wd := (d0 + 4) % 7
  let start := d0 - wd
  let mut weeks : Array Node := #[]
  for w in [0:6] do
    let mut cells : Array Node := #[]
    for i in [0:7] do
      let day := start + (w * 7 + i : Nat)
      let (_, dm, dd) := Core.Mime.Date.civilFromDays day
      let key := Core.RRule.isoDate day
      let here := occs.filter (·.occ.dateKey == key)
      let items := (here.extract 0 3).toList.map fun o =>
        el "li" [] [elt "a" [("href", s!"/calendar/{o.held.row.text "id"}")]
          ((if o.occ.allDay then "" else hhmm o.occ.start.toNat ++ " ") ++ (if o.held.ev.summary.isEmpty then "(no title)" else o.held.ev.summary))]
      let cls := "day" ++ (if dm != m then " other" else "") ++ (if day == today then " today" else "")
      cells := cells.push <| el "td" [("class", cls)] [
        elt "span" [("class", "dnum")] (toString dd),
        if items.isEmpty then empty else el "ul" [] items,
        when (here.size > 3) (elt "span" [("class", "more")] s!"+{here.size - 3} more")]
    weeks := weeks.push (el "tr" [] cells.toList)
  return el "table" [("class", "month"), ("aria-label", "Month")] [
    el "thead" [] [el "tr" [] (weekdays.toList.map fun d => elt "th" [("scope", "col")] d)],
    el "tbody" [] weeks.toList]

def calendarPage (p : Page) : IO Resp := do
  let now ← Clock.nowMs
  let (ty, tm, _) := ymd now
  let (y, m) := ((p.req.param? "month").bind parseMonth?).getD (ty, tm)
  let d0 := Core.Mime.Date.daysFromCivil y m 1
  let gridStart := (d0 - (d0 + 4) % 7) * calDayMs
  let gridOccs ← Calendar.occurrences p.c.st gridStart (gridStart + 42 * calDayMs)
  let today : Int := (now / dayMs : Nat)
  let agendaFrom := today * calDayMs
  let agenda ← Calendar.occurrences p.c.st agendaFrom (agendaFrom + 14 * calDayMs)
  let back := "/calendar" ++ (match p.req.param? "month" with | some x => s!"?month={x}" | none => "")
  let (py, pm) := prevMonth y m
  let (ny, nm) := nextMonth y m
  let mut groups : Array (String × Array Calendar.Occ) := #[]
  for o in agenda do
    match groups.back? with
    | some (k, os) => if k == o.occ.dateKey then groups := groups.pop.push (k, os.push o) else groups := groups.push (o.occ.dateKey, #[o])
    | none => groups := groups.push (o.occ.dateKey, #[o])
  let agendaNode : Node :=
    if groups.isEmpty then emptyNote "Nothing in the next 14 days."
    else el "div" [("class", "agenda")] <| groups.toList.map fun (k, os) =>
      let ms := (Core.RRule.dayOfIso k * calDayMs).toNat
      el "section" [("class", "day")] [elt "h3" [("class", "day-h")] (dayLabel ms now),
        el "ul" [("class", "occs")] (os.toList.map (occItem · back))]
  let saved := match p.req.param? "saved" with
    | some "rsvp" => flash "ok" "Answer kept. Nothing was sent to the organizer."
    | some "created" => flash "ok" "Event added."
    | some "deleted" => flash "ok" "Event deleted."
    | some "error" => flash "err" "That event could not be saved. Check the title, the date and the time."
    | _ => empty
  return page p "Calendar" (pageKind := "calendar") [
    elt "h1" [] "Calendar", saved,
    el "nav" [("class", "month-nav"), ("aria-label", "Months")] [
      elt "a" [("href", href "/calendar" [("month", monthKey py pm)]), ("rel", "prev")] "‹ Previous",
      elt "strong" [] s!"{months[m - 1]!} {y}",
      elt "a" [("href", href "/calendar" [("month", monthKey ny nm)]), ("rel", "next")] "Next ›",
      elt "a" [("href", "/calendar")] "Today"],
    monthGrid y m gridOccs today,
    el "section" [("class", "card"), ("id", "agenda")] [elt "h2" [] "Next 14 days", agendaNode],
    el "section" [("class", "card"), ("id", "new-event")] [
      elt "h2" [] "Add an event",
      el "form" [("method", "post"), ("action", "/calendar/new"), ("class", "stack")] [
        field "Title" "summary" "" "text" [("required", "required"), ("maxlength", "200")],
        field "Date" "date" "" "date" [("required", "required")],
        field "Start (UTC, empty for all day)" "time" "" "time",
        field "Length in minutes" "minutes" "60" "number" [("min", "5"), ("max", "1440")],
        field "Where" "location" "" "text" [("maxlength", "200")],
        elt "button" [("type", "submit"), ("class", "primary")] "Add"]]]

def eventPage (p : Page) : IO Resp := do
  let st := p.c.st
  let id := p.c.param "id"
  let some h ← Calendar.heldById st id | return notFoundPage p
  let own ← Calendar.ownAddresses st
  let r := h.row
  let e := h.ev
  let ps := Calendar.partstatOf h own
  let title := if e.summary.isEmpty then "(no title)" else e.summary
  let cancelled := r.bool "cancelled"
  let whenNode : Node := match Calendar.firstOccurrence e with
    | some o => txt (calLongWhen o ++ (if Core.Ical.isRecurring e then s!", repeating ({e.rruleRaw.getD "custom"})" else ""))
    | none => txt "—"
  let guests := e.attendees.take 50 |>.map fun a =>
    el "li" [] [txt (if a.name.isEmpty then a.email else s!"{a.name} <{a.email}>"), txt " · ", txt (partLabel a.partstat)]
  let back := s!"/calendar/{id}"
  return page p title (pageKind := "event") [
    el "article" [("class", "event"), ("data-id", id)] [
      elt "h1" [] (if cancelled then s!"Cancelled: {title}" else title),
      (match p.req.param? "saved" with
       | some "rsvp" => flash "ok" "Answer kept. Nothing was sent to the organizer."
       | some "saved" => flash "ok" "Saved."
       | _ => empty),
      el "dl" [("class", "hdrs")] (
        hdrRow "When" whenNode ++
        (if e.location.isEmpty then [] else hdrRow "Where" (txt e.location)) ++
        (match e.organizer with
         | some o => hdrRow "Organizer" (txt (if o.name.isEmpty then o.email else s!"{o.name} <{o.email}>"))
         | none => []) ++
        (match r.text? "message_id" with
         | some m => hdrRow "Invitation" (elt "a" [("href", s!"/m/{m}")] "The message it came in")
         | none => []) ++
        hdrRow "Your answer" (txt ((ps.map partLabel).getD "—")) ++
        (match r.nat? "expires_at" with
         | some t => hdrRow "Kept until" (timeEl t (fullDate t) "full")
         | none => hdrRow "Kept until" (txt "forever"))),
      when (isInvite h && !cancelled) (rsvpForm id ps back),
      when (!guests.isEmpty) (el "section" [("class", "guests")] [elt "h2" [] s!"Guests ({e.attendees.length})", el "ul" [] guests]),
      when (!e.description.isEmpty) (elt "pre" [("class", "body-text")] e.description),
      el "form" [("method", "post"), ("action", s!"/calendar/{id}/notes"), ("class", "stack")] [
        el "label" [("class", "field")] [elt "span" [] "Notes (yours only)",
          elt "textarea" [("name", "notes"), ("rows", "3"), ("maxlength", "4000")] (r.text "o_notes")],
        elt "button" [("type", "submit")] "Save notes"],
      el "div" [("class", "row-buttons")] [
        when (r.nat? "expires_at" |>.isSome) (postButton s!"/calendar/{id}/keep" "Keep forever"),
        postButton s!"/calendar/{id}/hide" (if r.bool "o_hidden" then "Show on the calendar" else "Hide from the calendar"),
        when (r.text "source" == "manual") (postButton s!"/calendar/{id}/delete" "Delete" "danger")]]]

/-- Only a local path is a place to go back to. -/
def safeBack (s : String) (d : String) : String :=
  if s.startsWith "/" && !s.startsWith "//" && !s.contains '\\' then s else d

def withQuery (path k v : String) : String := path ++ (if path.contains '?' then "&" else "?") ++ k ++ "=" ++ v

def rsvpPost (p : Page) : IO Resp := do
  let f := parseForm p.req.body
  let id := p.c.param "id"
  let ps := formField f "partstat"
  if !Calendar.partstats.contains ps then return errorPage 400 (some p.me)
  if !(← Calendar.setOverride p.c.st id (some ps) none none) then return notFoundPage p
  return redirect (withQuery (safeBack (formField f "back") s!"/calendar/{id}") "saved" "rsvp") 303

def eventNotesPost (p : Page) : IO Resp := do
  let id := p.c.param "id"
  let notes := String.ofList ((formField (parseForm p.req.body) "notes").toList.take 4000)
  if !(← Calendar.setOverride p.c.st id none (some notes) none) then return notFoundPage p
  return redirect s!"/calendar/{id}?saved=saved" 303

def eventHidePost (p : Page) : IO Resp := do
  let id := p.c.param "id"
  let some h ← Calendar.heldById p.c.st id | return notFoundPage p
  discard <| Calendar.setOverride p.c.st id none none (some !(h.row.bool "o_hidden"))
  return redirect s!"/calendar/{id}?saved=saved" 303

def eventKeepPost (p : Page) : IO Resp := do
  let id := p.c.param "id"
  let n ← p.c.st.db.execCount "UPDATE events SET expires_at = NULL, kept = 1, updated_at = ? WHERE id = ? AND deleted = 0" #[← Clock.nowMs, id]
  if n == 0 then return notFoundPage p
  return redirect (safeBack (formField (parseForm p.req.body) "back") s!"/calendar/{id}?saved=saved") 303

def eventDeletePost (p : Page) : IO Resp := do
  let r ← Calendar.remove p.c p.req
  if r.status != 200 then return notFoundPage p
  return redirect "/calendar?saved=deleted" 303

def eventNewPost (p : Page) : IO Resp := do
  let f := parseForm p.req.body
  let date := Core.Text.Str.trim (formField f "date")
  let time := Core.Text.Str.trim (formField f "time")
  let mins := ((formField f "minutes").toNat?).getD 60
  let body : Json :=
    if time.isEmpty then Json.mkObj [("summary", Json.str (formField f "summary")), ("start", Json.str date),
      ("all_day", Json.bool true), ("location", Json.str (formField f "location"))]
    else
      let start := Core.RRule.zonedToUtc s!"{date}T{String.ofList (time.toList.take 5)}:00" (some "UTC")
      Json.mkObj [("summary", Json.str (formField f "summary")), ("start", toJson start), ("end", toJson (start + mins * 60000)),
        ("location", Json.str (formField f "location"))]
  let r ← Calendar.create p.c { p.req with body := body.compress.toUTF8 }
  if r.status != 201 then return redirect "/calendar?saved=error" 303
  return redirect "/calendar?saved=created" 303

def calendarRoutes : Array Route := #[
  route "GET" "/calendar" .«public» (guarded false calendarPage),
  route "POST" "/calendar/new" .«public» (guarded true eventNewPost),
  route "GET" "/calendar/:id" .«public» (guarded false eventPage),
  route "POST" "/calendar/:id/rsvp" .«public» (guarded true rsvpPost),
  route "POST" "/calendar/:id/notes" .«public» (guarded true eventNotesPost),
  route "POST" "/calendar/:id/hide" .«public» (guarded true eventHidePost),
  route "POST" "/calendar/:id/keep" .«public» (guarded true eventKeepPost),
  route "POST" "/calendar/:id/delete" .«public» (guarded true eventDeletePost)
]

end Server.Pages
