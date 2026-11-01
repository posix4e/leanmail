import Core.Mime
import Server.Pages.Common
import Server.Pages.Inbox
import Server.Api
import Server.Actions
import Server.Pages.Invite
import Server.Junk
import Server.Ingest
import Server.Pages.AiSettings

/-! # Pages: one message, one thread, and the action form

`/m/<id>` shows the headers, the attachments (downloads from `/api/msg/<id>/attachments/<idx>`),
the body, the rest of the thread, and the action buttons. The HTML body is never inlined: it is
an `<iframe sandbox>` on `/api/msg/<id>/html`, which sends its own strict CSP; `?view=text` shows
the stored plain text instead. Whether a message has HTML is known only by parsing its raw, which
the store keeps (the database holds text only). -/

namespace Server.Pages

open Lean Core.Html Server.Db Server.Router

/-- `[{name, address}]` as `Name <address>` strings. -/
def mailboxes (s : String) : List String :=
  match Json.parse s with
  | .ok (.arr xs) => xs.toList.filterMap fun j =>
    let n := (j.getObjValAs? String "name").toOption.getD ""
    let a := (j.getObjValAs? String "address").toOption.getD ""
    if a.isEmpty && n.isEmpty then none else some (if n.isEmpty then a else if a.isEmpty then n else s!"{n} <{a}>")
  | _ => []

def hdrRow (label : String) (value : Node) : List Node := [elt "dt" [] label, el "dd" [] [value]]

/-- The action buttons, one form; each button names its action. -/
def actionForm (id : String) (r : Row) (now : Nat) : Node :=
  let b (v label : String) (cls : String := "") :=
    elt "button" [("type", "submit"), ("name", "action"), ("value", v), ("class", cls)] label
  let archived := (r.nat? "archived_at").isSome
  let snoozed := (r.nat? "snoozed_until").any (· > now)
  let toread := (r.nat? "to_read_at").isSome
  el "form" [("method", "post"), ("action", s!"/m/{id}/action"), ("class", "actions"), ("data-id", id)] [
    if archived then b "inbox" "Move to inbox" else b "done" "Done" "primary",
    if snoozed then b "unsnooze" "Unsnooze" else
      el "span" [("class", "group"), ("role", "group"), ("aria-label", "Snooze")] [
        elt "span" [("class", "group-label")] "Snooze",
        b "snooze_1d" "1 day", b "snooze_3d" "3 days", b "snooze_week" "Next week"],
    if toread then b "untoread" "Not to read" else b "toread" "To read",
    when (r.nat? "expires_at").isSome (b "forever" "Keep forever")]

def stateLine (r : Row) (now : Nat) : Node :=
  let parts : List Node :=
    (match r.nat? "archived_at" with | some t => [el "span" [] [txt "Done ", timeEl t (shortTime t now)]] | none => []) ++
    (match r.nat? "snoozed_until" with
     | some t => if t > now then [el "span" [] [txt "Snoozed until ", timeEl t (fullDate t) "full"]] else []
     | none => []) ++
    (if (r.nat? "to_read_at").isSome then [txt "To read"] else []) ++
    (match r.text? "junk_kind" with | some k => [txt s!"Junk ({k})"] | none => []) ++
    (match r.nat? "expires_at" with
     | some t => [el "span" [] [txt "Expires ", timeEl t (fullDate t) "full"]]
     | none => [txt "Kept forever"])
  el "p" [("class", "state")] (parts.intersperse (txt " · "))

def threadNav (now : Nat) (current : String) (rows : Array Row) : Node :=
  if rows.size ≤ 1 then empty else
  el "nav" [("class", "thread"), ("aria-label", "Thread")] [
    elt "h2" [] s!"In this thread ({rows.size})",
    el "ol" [] <| rows.toList.map fun r =>
      let id := r.text "id"
      let ts := r.nat "received_at"
      let inner := [elt "span" [("class", "from")] (fromLabel r), timeEl ts (shortTime ts now),
                    elt "span" [("class", "snip")] (r.text "snippet")]
      if id == current then el "li" [("class", "cur"), ("aria-current", "true")] inner
      else el "li" [] [el "a" [("href", s!"/m/{id}")] inner]]

def message (p : Page) : IO Resp := do
  let st := p.c.st
  let now ← Clock.nowMs
  let id := p.c.param "id"
  let some r ← Api.messageRow st id | return notFoundPage p
  let text := ((← st.db.queryOne "SELECT body FROM message_text WHERE message_id = ?" #[id]).map (·.text "body")).getD ""
  let atts ← st.db.query "SELECT idx, filename, mime, size, inline FROM attachments WHERE message_id = ? ORDER BY idx" #[id]
  let rawMsg ← (do
    let some (_, raw) ← Api.loadRaw st id | pure none
    pure (some (Core.Mime.parse raw))) <|> pure none
  let hasHtml := (rawMsg.bind (·.html)).isSome
  let unsub := (rawMsg.bind (·.rawHeader? "list-unsubscribe")).map fun v =>
    Server.Junk.parseUnsub v (rawMsg.bind (·.rawHeader? "list-unsubscribe-post"))
  let rowKeys := ((rawMsg.map fun m => Calendar.extract m.attachments 0).getD []).map (·.rowKey)
  let invite ← inviteBlock st id rowKeys
  let junkRow ← junkPanel st id r unsub
  let view := if hasHtml && p.req.param? "view" != some "text" then "html" else "text"
  let thread ← match r.text? "thread_id" with
    | some t => st.db.query "SELECT * FROM messages WHERE thread_id = ? AND deleted = 0 ORDER BY received_at ASC, id ASC" #[t]
    | none => pure #[]
  let subj := r.text "subject"
  let ts := r.nat "received_at"
  let fromAddr := r.text "from_addr"
  let fromName := r.text "from_name"
  let fromStr := if fromName.isEmpty then fromAddr else if fromAddr.isEmpty then fromName else s!"{fromName} <{fromAddr}>"
  let to := mailboxes (r.text "to_json")
  let cc := mailboxes (r.text "cc_json")
  let hdrs := hdrRow "From" (txt fromStr) ++
    (if to.isEmpty then [] else hdrRow "To" (txt (", ".intercalate to))) ++
    (if cc.isEmpty then [] else hdrRow "Cc" (txt (", ".intercalate cc))) ++
    (match r.text? "reply_to" with | some rt => if rt.isEmpty then [] else hdrRow "Reply-To" (txt rt) | none => []) ++
    hdrRow "Date" (timeEl ts (fullDate ts) "full") ++
    hdrRow "Subject" (txt (if subj.isEmpty then "(no subject)" else subj))
  let attList : Node := if atts.isEmpty then empty else
    el "ul" [("class", "atts"), ("aria-label", "Attachments")] <| atts.toList.map fun a =>
      let name := a.text "filename"
      el "li" [] [
        elt "a" [("href", s!"/api/msg/{id}/attachments/{a.nat "idx"}"), ("download", "")] (if name.isEmpty then "attachment" else name),
        elt "span" [("class", "att-meta")] s!"{a.text "mime"}, {humanSize (a.nat "size")}{if a.bool "inline" then ", inline" else ""}"]
  let toggle : Node := if !hasHtml then empty else
    el "nav" [("class", "viewtoggle"), ("aria-label", "Body view")] [
      if view == "html" then elt "span" [("class", "on")] "HTML" else elt "a" [("href", s!"/m/{id}")] "HTML",
      if view == "text" then elt "span" [("class", "on")] "Text" else elt "a" [("href", s!"/m/{id}?view=text")] "Text"]
  let body : Node :=
    if view == "html" then
      el "iframe" [("sandbox", ""), ("src", s!"/api/msg/{id}/html"), ("class", "body-html"), ("title", "Message body"),
                   ("referrerpolicy", "no-referrer"), ("loading", "lazy")] []
    else if text.isEmpty then emptyNote "This message has no text."
    else elt "pre" [("class", "body-text")] text
  let summary ← summarySection st id (r.text? "thread_id") (p.req.param? "summary")
  let truncated := when (r.bool "text_truncated") (elt "p" [("class", "hint")] "The stored text was shortened; the original is kept in full.")
  return page p (if subj.isEmpty then "(no subject)" else subj) (pageKind := "message") [
    el "article" [("class", "msg"), ("data-id", id)] [
      elt "h1" [("class", "subject")] (if subj.isEmpty then "(no subject)" else subj),
      el "nav" [("class", "compose-links"), ("aria-label", "Reply")] [
        elt "a" [("href", s!"/compose?reply={id}"), ("class", "btn primary")] "Reply",
        elt "a" [("href", s!"/compose?reply={id}&all=1"), ("class", "btn")] "Reply all",
        elt "a" [("href", s!"/compose?forward={id}"), ("class", "btn")] "Forward"],
      (match p.req.param? "sent" with
       | some "sent" => flash "ok" "Sent."
       | some "failed" => flash "err" "Sending failed. The copy is kept in Sent; try again from a new message."
       | some s => flash "ok" s!"Send status: {s}."
       | none => empty),
      (match p.req.param? "junk" with
       | some "reported" => flash "ok" "Reported. It is in Junk."
       | some "notjunk" => flash "ok" "Back from Junk; blocks on this sender are lifted."
       | some "blocked" => flash "ok" "Sender blocked. Their mail now goes to Junk."
       | some "unsubscribed" => flash "ok" "Unsubscribed."
       | some "unsub_failed" => flash "err" "The sender's unsubscribe link did not answer. Try the link instead."
       | some "unsub_manual" => flash "ok" "This list has no one-click unsubscribe; use the link below."
       | some "error" => flash "err" "That did not work."
       | _ => empty),
      actionForm id r now,
      stateLine r now,
      junkRow,
      invite,
      summary,
      el "dl" [("class", "hdrs")] hdrs,
      attList, toggle, body, truncated,
      el "p" [("class", "raw-link")] [elt "a" [("href", s!"/api/msg/{id}/raw"), ("download", s!"{id}.eml")] "Download original"]],
    threadNav now id thread]

def threadPage (p : Page) : IO Resp := do
  let now ← Clock.nowMs
  let rows ← p.c.st.db.query "SELECT * FROM messages WHERE thread_id = ? AND deleted = 0 ORDER BY received_at DESC, id DESC" #[p.c.param "id"]
  let some first := rows.back? | return notFoundPage p
  let subj := first.text "subject"
  return page p (if subj.isEmpty then "Thread" else subj) (pageKind := "list") [
    elt "h1" [("class", "subject")] (if subj.isEmpty then "(no subject)" else subj),
    elt "p" [("class", "state")] s!"{rows.size} messages",
    groupedList now rows]

/-- `POST /m/<id>/action` (form): apply, then back to the inbox when the message leaves it. -/
def actionPost (p : Page) : IO Resp := do
  let id := p.c.param "id"
  let action := formField (parseForm p.req.body) "action"
  match ← Actions.apply p.c.st id action with
  | .error _ => return errorPage 400 (some p.me)
  | .ok none => return notFoundPage p
  | .ok (some _) => return redirect (if Actions.leavesList action then "/inbox" else s!"/m/{id}") 303

/-! ## The junk row's forms -/

def junkRedirect (id : String) (r : Except (Nat × String) Json) (ok : String) : Resp :=
  match r with
  | .ok _ => redirect s!"/m/{id}?junk={ok}" 303
  | .error _ => redirect s!"/m/{id}?junk=error" 303

def reportPost (p : Page) : IO Resp := do
  let id := p.c.param "id"
  let kind := formField (parseForm p.req.body) "kind"
  let r ← Server.Junk.report p.c.st id kind none
  match r with
  | .ok _ => return redirect "/inbox" 303
  | .error (404, _) => return notFoundPage p
  | .error _ => return junkRedirect id r "reported"

def notJunkPost (p : Page) : IO Resp := do
  let id := p.c.param "id"
  let r ← Server.Junk.notJunk p.c.st id
  if let .error (404, _) := r then return notFoundPage p
  return junkRedirect id r "notjunk"

def blockPost (p : Page) : IO Resp := do
  let id := p.c.param "id"
  let some row ← Api.messageRow p.c.st id | return notFoundPage p
  let r ← Server.Junk.report p.c.st id "spam" (some ("address", row.text "from_addr"))
  return junkRedirect id r "blocked"

def unsubscribePost (p : Page) : IO Resp := do
  let id := p.c.param "id"
  match ← Server.Junk.unsubscribe p.c.st id with
  | .ok j =>
    let st := (j.getObjValAs? String "status").toOption.getD ""
    return redirect s!"/m/{id}?junk={if st == "unsubscribed" then "unsubscribed" else if st == "manual" then "unsub_manual" else "unsub_failed"}" 303
  | .error (404, _) => return redirect s!"/m/{id}?junk=error" 303
  | .error _ => return redirect s!"/m/{id}?junk=error" 303

def messageJunkRoutes : Array Route := #[
  route "POST" "/m/:id/report" .«public» (guarded true reportPost),
  route "POST" "/m/:id/notjunk" .«public» (guarded true notJunkPost),
  route "POST" "/m/:id/block" .«public» (guarded true blockPost),
  route "POST" "/m/:id/unsubscribe" .«public» (guarded true unsubscribePost)
]

end Server.Pages
