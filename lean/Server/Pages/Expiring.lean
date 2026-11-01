import Server.Pages.Common
import Server.Pages.Inbox
import Server.Pages.Calendar
import Server.Expiry

/-! # Pages: what expires in the next 7 days (`/expiring`)

Mail and calendar events whose retention runs out within a week, soonest first, each with a
"Keep forever" button, and one for everything shown. -/

namespace Server.Pages

open Lean Core.Html Server.Db Server.Router

def expiringPage (p : Page) : IO Resp := do
  let st := p.c.st
  let now ← Clock.nowMs
  let until_ := now + 7 * dayMs
  let msgs ← st.db.query "SELECT * FROM messages WHERE deleted = 0 AND expires_at IS NOT NULL AND expires_at > ? AND expires_at <= ?
    ORDER BY expires_at, id LIMIT 200" #[now, until_]
  let evs ← Calendar.loadHeld st " AND e.kept = 0 AND e.expires_at IS NOT NULL AND e.expires_at > ? AND e.expires_at <= ? ORDER BY e.expires_at LIMIT 200"
    #[now, until_]
  let msgItems := msgs.toList.map fun r =>
    let id := r.text "id"
    let t := r.nat "expires_at"
    el "li" [("class", "item"), ("data-id", id)] [
      el "div" [] [elt "a" [("href", s!"/m/{id}")] (if (r.text "subject").isEmpty then "(no subject)" else r.text "subject"),
        elt "span" [("class", "from")] (" · " ++ fromLabel r)],
      el "div" [("class", "meta")] [txt "Expires ", timeEl t (fullDate t) "full"],
      postButton "/expiring/keep" "Keep forever" "" [("id", id)]]
  let evItems := evs.toList.map fun h =>
    let id := h.row.text "id"
    let t := h.row.nat "expires_at"
    el "li" [("class", "item"), ("data-event", id)] [
      el "div" [] [elt "a" [("href", s!"/calendar/{id}")] (if h.ev.summary.isEmpty then "(no title)" else h.ev.summary)],
      el "div" [("class", "meta")] [txt "Expires ", timeEl t (fullDate t) "full"],
      postButton s!"/calendar/{id}/keep" "Keep forever" "" [("back", "/expiring?saved=kept")]]
  return page p "Expiring" (pageKind := "expiring") [
    elt "h1" [] "Expiring in the next 7 days",
    (match p.req.param? "saved" with | some "kept" => flash "ok" "Kept forever." | _ => empty),
    el "section" [("class", "card"), ("id", "expiring-mail")] [
      elt "h2" [] s!"Mail ({msgs.size})",
      if msgItems.isEmpty then emptyNote "No mail expires this week." else frag [
        el "ul" [("class", "items")] msgItems,
        el "form" [("method", "post"), ("action", "/expiring/keep")]
          ((msgs.toList.map fun r => el "input" [("type", "hidden"), ("name", "id"), ("value", r.text "id")]) ++
            [elt "button" [("type", "submit")] "Keep all of these forever"])]],
    el "section" [("class", "card"), ("id", "expiring-events")] [
      elt "h2" [] s!"Calendar ({evs.size})",
      if evItems.isEmpty then emptyNote "No events expire this week." else el "ul" [("class", "items")] evItems]]

def expiringKeepPost (p : Page) : IO Resp := do
  let ids := (parseForm p.req.body).filterMap fun (k, v) => if k == "id" then some v else none
  if ids.isEmpty then return errorPage 400 (some p.me)
  discard <| Expiry.setExpiry p.c.st ids none
  return redirect "/expiring?saved=kept" 303

def expiringRoutes : Array Route := #[
  route "GET" "/expiring" .«public» (guarded false expiringPage),
  route "POST" "/expiring/keep" .«public» (guarded true expiringKeepPost)
]

end Server.Pages
