import Server.Pages.Common
import Server.Pages.Settings
import Server.Gmail

/-! # Pages: Gmail settings (`/settings/gmail`)

The owner's OAuth client (id and secret), the Connect link (`GET /api/gmail/authorize`, which
sends the browser to Google), the connection's state (address, last poll, last push, the watch
and its expiry, the last error), the backfill with its progress and controls, the mirror and spam
switches with what each lane has done, the Pub/Sub topic and the push URL to give its
subscription, and Disconnect. Every form posts to a `guarded` route (form CSRF rule) that calls
the same functions as `/api/gmail/*`. -/

namespace Server.Pages

open Lean Core.Html Server.Db Server.Router

private def jS (j : Json) (k : String) : Option String := (j.getObjValAs? String k).toOption
private def jN (j : Json) (k : String) : Option Nat := (j.getObjValAs? Nat k).toOption
private def jB (j : Json) (k : String) : Bool := ((j.getObjValAs? Bool k).toOption).getD false
private def jO (j : Json) (k : String) : Json := (j.getObjVal? k).toOption.getD Json.null

private def whenAt (label : String) (t : Option Nat) (now : Nat) : Node :=
  el "li" [] [elt "span" [("class", "k")] label, txt " ",
    match t with
    | some ms => timeEl ms (shortTime ms now)
    | none => txt "never"]

private def checkbox (name label : String) (on : Bool) : Node :=
  el "label" [("class", "check")] [
    el "input" ([("type", "checkbox"), ("name", name), ("value", "1")] ++ (if on then [("checked", "checked")] else [])),
    txt label]

private def countsLine (j : Json) (keys : List String) : String :=
  ", ".intercalate (keys.filterMap fun k => (jN j k).map fun n => s!"{n} {k}")

def gmailNotice (req : Req) : Node :=
  match req.param? "connected", req.param? "error", req.param? "saved" with
  | some _, _, _ => flash "ok" "Gmail connected."
  | _, some "denied", _ => flash "err" "Google did not grant access."
  | _, some "state", _ => flash "err" "That authorization link expired or was already used. Connect again."
  | _, some e, _ => flash "err" s!"Gmail said no: {e}."
  | _, _, some "client" => flash "ok" "OAuth client saved. Now connect."
  | _, _, some "prefs" => flash "ok" "Saved."
  | _, _, some "backfill" => flash "ok" "Backfill updated."
  | _, _, some "watch" => flash "ok" "Push notifications are on."
  | _, _, some "disconnected" => flash "ok" "Gmail disconnected. Mail already stored stays."
  | _, _, _ => empty

def gmailPage (p : Page) (notice : Node := empty) (status : Nat := 200) : IO Resp := do
  let st := p.c.st
  let s ← Gmail.statusJson st
  let now ← Clock.nowMs
  let connected := jB s "connected"
  let prefs := jO s "prefs"
  let job := jO s "backfill"
  let jobState := (jS job "state").getD ""
  let client : Node :=
    el "form" [("method", "post"), ("action", "/settings/gmail/client"), ("class", "stack")] [
      el "p" [("class", "hint")] [
        txt "Create an OAuth client (type Web application) in your Google Cloud project with this redirect URI: ",
        elt "code" [("class", "mono")] (Gmail.OAuth.redirectUri st.config)],
      field "Client ID" "client_id" ((jS s "client_id").getD "") "text" [("autocomplete", "off"), ("placeholder", "….apps.googleusercontent.com")],
      field "Client secret" "client_secret" "" "password" [("autocomplete", "off")],
      elt "button" [("type", "submit")] "Save client"]
  let connection : Node :=
    if !jB s "client_configured" then elt "p" [("class", "hint")] "Save an OAuth client first."
    else frag [
      if connected then
        el "p" [] [txt "Connected as ", elt "strong" [] ((jS s "email").getD "?")]
      else elt "p" [("class", "hint")] "Not connected.",
      when (jB s "needs_reauth") (flash "err" "Google refused the stored grant. Connect again."),
      el "ul" [("class", "facts")] [
        whenAt "Last poll" (jN s "last_poll_at") now,
        whenAt "Last push" (jN s "last_push_at") now,
        el "li" [] [elt "span" [("class", "k")] "Push watch", txt " ",
          match jN s "watch_expiration" with
          | some e => if jB s "watch_active" then frag [txt "until ", timeEl e (fullDate e) "full"] else txt "expired"
          | none => txt "off"],
        when ((jS s "last_error").isSome) (el "li" [] [elt "span" [("class", "k")] "Last error", txt " ",
          elt "code" [] ((jS s "last_error").getD "")])],
      el "div" [("class", "row-buttons")] [
        elt "a" [("class", "button primary"), ("href", "/api/gmail/authorize")] (if connected then "Connect again" else "Connect Gmail"),
        when connected (postButton "/settings/gmail/disconnect" "Disconnect" "danger")]]
  let progress : Node :=
    if job.isNull then elt "p" [("class", "hint")] "No backfill yet."
    else
      let seen := (jN job "seen").getD 0
      let total := jN job "planned_total"
      el "div" [] [
        el "p" [] [txt s!"State: {jobState}. Seen {seen}", txt (match total with | some t => s!" of about {t}" | none => ""),
          txt s!"; imported {(jN job "imported").getD 0}, duplicates {(jN job "duplicates").getD 0}, failed {(jN job "failed").getD 0}."],
        match total with
        | some t => el "progress" [("max", toString (Nat.max t 1)), ("value", toString (Nat.min seen t))] []
        | none => empty,
        when ((jS job "error_code").isSome) (el "p" [("class", "hint")] [txt "Stopped on ", elt "code" [] ((jS job "error_code").getD "")])]
  let backfill : Node := frag [
    progress,
    el "div" [("class", "row-buttons")] [
      when (jobState == "running") (postButton "/settings/gmail/backfill/pause" "Pause"),
      when (jobState == "paused") (postButton "/settings/gmail/backfill/resume" "Resume"),
      when (jobState == "running" || jobState == "paused") (postButton "/settings/gmail/backfill/cancel" "Cancel" "danger"),
      when ((jN s "failures").getD 0 > 0) (postButton "/settings/gmail/backfill/retry-failed" s!"Retry {(jN s "failures").getD 0} failed")],
    when (connected && jobState != "running" && jobState != "paused")
      (el "form" [("method", "post"), ("action", "/settings/gmail/backfill"), ("class", "stack")] [
        el "p" [("class", "hint")] [txt "Copies the whole mailbox into LeanMail. It runs in the background, a few minutes at a time."],
        checkbox "keep_forever" "Keep imported mail forever" false,
        elt "button" [("type", "submit"), ("class", "primary")] "Start backfill"])]
  let switches : Node :=
    el "form" [("method", "post"), ("action", "/settings/gmail/prefs"), ("class", "stack")] [
      checkbox "mirror_archive" "Marking mail done here archives it in Gmail" (jB prefs "mirror_archive"),
      elt "p" [("class", "hint")] (countsLine (jO s "mirror") ["archived", "restored", "removing", "restoring", "skipped", "failed"]),
      checkbox "report_spam" "Reporting junk here marks it as spam in Gmail" (jB prefs "report_spam"),
      elt "p" [("class", "hint")] (countsLine (jO s "spam") ["reported", "restored", "reporting", "restoring", "skipped", "failed"]),
      checkbox "backfill_spam_trash" "The backfill includes Spam and Trash" (jB prefs "backfill_spam_trash"),
      field "Pub/Sub topic for push (projects/…/topics/…)" "pubsub_topic" ((jS s "topic").getD "") "text" [("autocomplete", "off")],
      elt "button" [("type", "submit")] "Save"]
  let push : Node := frag [
    match jS s "push_url" with
    | some u => el "p" [("class", "hint")] [txt "Give the topic a push subscription to ", elt "code" [("class", "mono")] u]
    | none => elt "p" [("class", "hint")] "Push needs the Worker in front (no internal secret here).",
    when (connected && (jS s "topic").isSome) (postButton "/settings/gmail/watch" (if jB s "watch_active" then "Renew watch" else "Turn on push"))]
  return page p "Gmail" (pageKind := "settings") (status := status) [
    el "p" [] [elt "a" [("href", "/settings")] "← Settings"],
    elt "h1" [] "Gmail", notice, gmailNotice p.req,
    el "section" [("class", "card"), ("id", "gmail-connection")] [elt "h2" [] "Connection", connection],
    el "section" [("class", "card"), ("id", "gmail-client")] [elt "h2" [] "OAuth client", client],
    el "section" [("class", "card"), ("id", "gmail-backfill")] [elt "h2" [] "Backfill", backfill],
    el "section" [("class", "card"), ("id", "gmail-switches")] [elt "h2" [] "Gmail follows LeanMail", switches],
    el "section" [("class", "card"), ("id", "gmail-push")] [elt "h2" [] "Push", push]]

def gmailSettings (p : Page) : IO Resp := gmailPage p

def gmailClientPost (p : Page) : IO Resp := do
  let f := parseForm p.req.body
  match ← Gmail.OAuth.saveClient p.c.st (formField f "client_id") (formField f "client_secret") with
  | .ok () => return redirect "/settings/gmail?saved=client" 303
  | .error field => gmailPage p (flash "err" s!"Not saved: check the {field.replace "_" " "}.") (status := 400)

def gmailPrefsPost (p : Page) : IO Resp := do
  let f := parseForm p.req.body
  let on (k : String) := Json.bool (formField f k == "1")
  Gmail.applyPrefs p.c.st (Json.mkObj [("mirror_archive", on "mirror_archive"), ("report_spam", on "report_spam"),
    ("backfill_spam_trash", on "backfill_spam_trash"), ("pubsub_topic", Json.str (formField f "pubsub_topic"))])
  return redirect "/settings/gmail?saved=prefs" 303

def gmailBackfillPost (p : Page) : IO Resp := do
  let f := parseForm p.req.body
  match ← (try Gmail.Backfill.start p.c.st (formField f "keep_forever" == "1") catch e => pure (.error (Gmail.codeOf e))) with
  | .ok _ => return redirect "/settings/gmail?saved=backfill" 303
  | .error code => gmailPage p (flash "err" s!"The backfill did not start: {code}.") (status := 400)

def gmailBackfillActionPost (p : Page) : IO Resp := do
  let r ← Gmail.backfillAction p.c p.req
  if r.status == 200 then return redirect "/settings/gmail?saved=backfill" 303
  gmailPage p (flash "err" "That did not work.") (status := r.status)

def gmailWatchPost (p : Page) : IO Resp := do
  match ← Gmail.Watch.start p.c.st with
  | .ok _ => return redirect "/settings/gmail?saved=watch" 303
  | .error code => gmailPage p (flash "err" s!"Push not turned on: {code}.") (status := 400)

def gmailDisconnectPost (p : Page) : IO Resp := do
  Gmail.OAuth.forget p.c.st
  return redirect "/settings/gmail?saved=disconnected" 303

def gmailRoutes : Array Route := #[
  route "GET" "/settings/gmail" .«public» (guarded false gmailSettings),
  route "POST" "/settings/gmail/client" .«public» (guarded true gmailClientPost),
  route "POST" "/settings/gmail/prefs" .«public» (guarded true gmailPrefsPost),
  route "POST" "/settings/gmail/backfill" .«public» (guarded true gmailBackfillPost),
  route "POST" "/settings/gmail/backfill/:action" .«public» (guarded true gmailBackfillActionPost),
  route "POST" "/settings/gmail/watch" .«public» (guarded true gmailWatchPost),
  route "POST" "/settings/gmail/disconnect" .«public» (guarded true gmailDisconnectPost)
]

end Server.Pages
