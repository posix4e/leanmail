import Core.Junk
import Server.Pages.Common
import Server.Pages.Inbox
import Server.Calendar
import Server.Blocks
import Server.Junk

/-! # Pages: what the message page shows of the calendar and of the junk check

The invite block (summary, when, where, organizer, the person's answer and the RSVP buttons) for
a message that brought calendar events, and the junk row: the check's verdict with its reasons
in words (`Core.Junk.reasonText`), "Report spam" / "Report phishing" or "Not junk", "Block
sender" (or the block that filed it), and "Unsubscribe" when the message offers a way out. -/

namespace Server.Pages

open Lean Core.Html Server.Db Server.Router

def calDayMs : Int := 86400000

def calWhen (o : Core.Ical.Occurrence) : String :=
  if o.allDay then "All day" else
  let s := o.start.toNat
  let e := o.end_.toNat
  if e > s then s!"{hhmm s}–{hhmm e} UTC" else s!"{hhmm s} UTC"

def calLongWhen (o : Core.Ical.Occurrence) : String :=
  let s := o.start.toNat
  if o.allDay then
    let days := ((o.end_ - o.start) / calDayMs).toNat
    let (y, m, d) := ymd s
    s!"{weekday s} {d} {months[m - 1]!} {y}, all day" ++ (if days > 1 then s!" ({days} days)" else "")
  else s!"{fullDate s} – {hhmm o.end_.toNat} UTC"

def partLabel (p : String) : String :=
  match p with
  | "ACCEPTED" => "Accepted" | "TENTATIVE" => "Maybe" | "DECLINED" => "Declined" | "NEEDS-ACTION" => "Not answered"
  | x => x

/-- The RSVP buttons for an event; the current answer is marked. -/
def rsvpForm (eventId : String) (current : Option String) (back : String) : Node :=
  el "form" [("method", "post"), ("action", s!"/calendar/{eventId}/rsvp"), ("class", "rsvp inline")] (
    [el "input" [("type", "hidden"), ("name", "back"), ("value", back)]] ++
    (["ACCEPTED", "TENTATIVE", "DECLINED"].map fun p =>
      elt "button" ([("type", "submit"), ("name", "partstat"), ("value", p)] ++
        (if current == some p then [("class", "on"), ("aria-pressed", "true")] else [("aria-pressed", "false")])) (partLabel p)))

def isInvite (h : Calendar.Held) : Bool := h.row.text "source" == "mail" && !h.ev.attendees.isEmpty


/-- The invite block for a message's events, or nothing: the events its own calendar parts name
(`rowKeys`; a later update or cancellation moves an event's `message_id` to the newer message),
and any whose `message_id` is this message. -/
def inviteBlock (st : State) (msgId : String) (rowKeys : List String := []) : IO Node := do
  let ks := rowKeys.eraseDups.take 50
  let qs := ", ".intercalate (ks.map fun _ => "?")
  let held ← Calendar.loadHeld st
    (if ks.isEmpty then " AND e.message_id = ? ORDER BY e.id" else s!" AND (e.message_id = ? OR e.row_key IN ({qs})) ORDER BY e.id")
    (#[Value.text msgId] ++ ks.toArray.map Value.text)
  if held.isEmpty then return empty
  let own ← Calendar.ownAddresses st
  let back := s!"/m/{msgId}"
  return el "section" [("class", "invite card"), ("aria-label", "Invitation")] <| held.toList.map fun h =>
    let e := h.ev
    let id := h.row.text "id"
    let cancelled := h.row.bool "cancelled"
    let ps := Calendar.partstatOf h own
    let title := if e.summary.isEmpty then "(no title)" else e.summary
    el "div" [("class", "invite-event"), ("data-event", id)] [
      el "h2" [] [elt "a" [("href", s!"/calendar/{id}")] (if cancelled then s!"Cancelled: {title}" else title)],
      el "dl" [("class", "hdrs")] (
        [elt "dt" [] "When", el "dd" [] [txt ((Calendar.firstOccurrence e).map calLongWhen |>.getD "—")]] ++
        (if e.location.isEmpty then [] else [elt "dt" [] "Where", el "dd" [] [txt e.location]]) ++
        (match e.organizer with
         | some o => [elt "dt" [] "Organizer", el "dd" [] [txt (if o.name.isEmpty then o.email else s!"{o.name} <{o.email}>")]]
         | none => []) ++
        [elt "dt" [] "Your answer", el "dd" [] [txt ((ps.map partLabel).getD "—")]]),
      when (!cancelled && isInvite h) (rsvpForm id ps back)]

/-- The junk row and the check's reasons. -/
def junkPanel (st : State) (id : String) (r : Row) (unsub : Option Server.Junk.Unsub) : IO Node := do
  if r.text "source" == "sent" then return empty
  let verdict := (r.text? "junk_verdict").bind fun s => (Json.parse s).toOption
  let kind := (verdict.map Blocks.kindOfJson).getD "clean"
  let reasons := (verdict.map Blocks.reasonsOfJson).getD []
  let k : Core.Junk.Kind := match kind with
    | "phishing" => .phishing | "spam" => .spam | "suspicious" => .suspicious | _ => .clean
  let blocks ← Server.Junk.blocksFor st (r.text "from_addr")
  let junk := (r.nat? "junk_at").isSome
  let check : Node := if reasons.isEmpty then empty else
    el "div" [("class", s!"verdict {kind}"), ("role", "note")] [
      elt "p" [("class", "verdict-h")] (Core.Junk.verdictText k),
      el "ul" [] (reasons.map fun x => elt "li" [] (Core.Junk.reasonText x))]
  let b (action label : String) (hidden : List (String × String) := []) (cls := "") := postButton s!"/m/{id}/{action}" label cls hidden
  let fromAddr := r.text "from_addr"
  return el "section" [("class", "junk-row"), ("aria-label", "Junk")] [
    check,
    el "div" [("class", "row-buttons")] [
      if junk then b "notjunk" "Not junk" else frag [
        b "report" "Report spam" [("kind", "spam")] "danger",
        b "report" "Report phishing" [("kind", "phishing")] "danger"],
      if !blocks.isEmpty then
        el "span" [("class", "blocked-note")] [txt s!"Sender blocked ({(blocks[0]!).text "value"}) ",
          postButton s!"/settings/blocks/{(blocks[0]!).text "id"}/delete" "Unblock" "" [("back", s!"/m/{id}")]]
      else if fromAddr.isEmpty then empty else b "block" "Block sender",
      match unsub with
      | some u =>
        if u.oneClick && u.url.isSome then b "unsubscribe" "Unsubscribe"
        else match u.url, u.mailto with
          | some url, _ => elt "a" [("href", url), ("class", "btn"), ("rel", "noopener noreferrer"), ("target", "_blank")] "Unsubscribe"
          | none, some m => elt "a" [("href", m), ("class", "btn")] "Unsubscribe by mail"
          | none, none => empty
      | none => empty]]

end Server.Pages
