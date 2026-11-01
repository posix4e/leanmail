import Server.Pages.Common
import Server.Pages.Settings
import Server.AI
import Server.Ingest

/-! # Pages: AI and privileged mail (`/settings/ai`)

Whether the optional AI is configured (`AI_BASE_URL`, `AI_API_KEY`, `AI_MODEL`; the key is never
shown), the model and endpoint host, the owner's switch (`prefs.ai.enabled`), what it has done
(messages triaged, summaries cached, the lane's last run and error), and the privileged lists:
words (one per line; empty for the built-in legal and medical phrases) and senders (addresses or
domains). Privileged mail is withheld from MCP and never sent to the AI, configured or not, so
the lists are editable either way. Saving the lists re-marks every message.

`POST /m/:id/summary` is the message page's "Summarize thread" button without the script: it
asks for (or reuses) the summary and goes back to the message, which shows it. -/

namespace Server.Pages

open Lean Core.Html Server.Db Server.Router

private def linesOf (s : String) : List String :=
  ((s.replace "\r" "").splitOn "\n").map Core.Text.Str.trim |>.filter (· ≠ "")

private def listPref (st : State) (key : String) : IO (Option (List String)) := do
  return (← AI.prefValue? st key).bind Core.Mcp.Privilege.listOf

private def textarea (label name value : String) (rows : Nat := 6) : Node :=
  el "label" [("class", "field")] [
    elt "span" [] label,
    elt "textarea" [("name", name), ("rows", toString rows), ("class", "mono"), ("spellcheck", "false")] value]

def aiPage (p : Page) (notice : Node := empty) (status : Nat := 200) : IO Resp := do
  let st := p.c.st
  let cfg := st.config
  let now ← Clock.nowMs
  let on ← AI.prefOn st
  let words ← listPref st Ingest.prefPrivilegedWords
  let senders ← listPref st Ingest.prefPrivilegedSenders
  let triaged := ((← st.db.queryOne "SELECT count(*) AS n FROM ai_triage").map (·.nat "n")).getD 0
  let summaries := ((← st.db.queryOne "SELECT count(*) AS n FROM ai_summaries").map (·.nat "n")).getD 0
  let (own, all) ← AI.withheldCounts st
  let laneRow ← st.db.queryOne "SELECT last_run_at, last_error FROM jobs WHERE name = ?" #[AI.lane]
  let saved := match p.req.param? "saved" with
    | some "ai" => flash "ok" "Saved."
    | some "lists" => flash "ok" s!"Saved. {(p.req.param? "changed").getD "0"} messages re-marked."
    | _ => empty
  let status_ : Node :=
    if !cfg.aiEnabled then
      el "div" [] [
        el "p" [] [txt "The AI is ", elt "strong" [] "not configured", txt " on this server, so it is off."],
        el "p" [("class", "hint")] [txt "Set ", elt "code" [] "AI_BASE_URL", txt ", ", elt "code" [] "AI_API_KEY", txt " and ",
          elt "code" [] "AI_MODEL", txt " (any endpoint speaking the OpenAI chat-completions format) to turn it on."]]
    else frag [
      el "ul" [("class", "facts")] [
        el "li" [] [elt "span" [("class", "k")] "Model", txt " ", elt "code" [] cfg.aiModel],
        el "li" [] [elt "span" [("class", "k")] "Endpoint", txt " ", elt "code" [] (Config.originHost cfg.aiBaseUrl)],
        el "li" [] [elt "span" [("class", "k")] "Triaged", txt s!" {triaged} messages"],
        el "li" [] [elt "span" [("class", "k")] "Summaries", txt s!" {summaries} cached"],
        el "li" [] [elt "span" [("class", "k")] "Last triage run", txt " ",
          match laneRow.bind (·.nat? "last_run_at") with
          | some t => timeEl t (shortTime t now)
          | none => txt "never"],
        when ((laneRow.bind (·.text? "last_error")).isSome)
          (el "li" [] [elt "span" [("class", "k")] "Last error", txt " ", elt "code" [] ((laneRow.bind (·.text? "last_error")).getD "")])],
      el "form" [("method", "post"), ("action", "/settings/ai"), ("class", "stack")] [
        el "label" [("class", "check")] [
          el "input" ([("type", "checkbox"), ("name", "enabled"), ("value", "1")] ++ (if on then [("checked", "checked")] else [])),
          txt "Use the AI: triage new mail, summarize threads, suggest replies"],
        elt "button" [("type", "submit")] "Save"]]
  let lists : Node :=
    el "form" [("method", "post"), ("action", "/settings/ai/privileged"), ("class", "stack")] [
      el "p" [("class", "hint")] [
        txt "Privileged mail is never sent to the AI and is withheld from MCP clients. A message is privileged when its sender is listed or its subject, the start of its text or its sender carries one of the words; one privileged message withholds its whole thread. ",
        txt s!"Now {own} messages are privileged and {all} are withheld."],
      textarea "Words (one per line; leave empty for the built-in legal and medical phrases)" "words"
        ("\n".intercalate (words.getD [])) 8,
      when words.isNone (el "p" [("class", "hint")] [txt "Built in: ", txt (", ".intercalate Core.Mcp.Privilege.defaultWords)]),
      textarea "Senders (addresses or domains, one per line)" "senders" ("\n".intercalate (senders.getD [])) 4,
      elt "button" [("type", "submit"), ("class", "primary")] "Save the lists"]
  return page p "AI" (pageKind := "settings") (status := status) [
    el "p" [] [elt "a" [("href", "/settings")] "← Settings"],
    elt "h1" [] "AI and privileged mail", notice, saved,
    el "section" [("class", "card"), ("id", "ai-status")] [elt "h2" [] "AI", status_],
    el "section" [("class", "card"), ("id", "ai-privileged")] [elt "h2" [] "Privileged mail", lists]]

def aiSettings (p : Page) : IO Resp := aiPage p

def aiSettingsPost (p : Page) : IO Resp := do
  let f := parseForm p.req.body
  AI.setPrefValue p.c.st AI.prefEnabled (if formField f "enabled" == "1" then "1" else "0")
  return redirect "/settings/ai?saved=ai" 303

def aiPrivilegedPost (p : Page) : IO Resp := do
  let f := parseForm p.req.body
  let words := (linesOf (formField f "words")).map (·.toLower) |>.eraseDups |>.take 200
  let senders := (linesOf (formField f "senders")).map (·.toLower) |>.eraseDups |>.take 500
  let st := p.c.st
  let enc (xs : List String) := (Json.arr (xs.toArray.map Json.str)).compress
  if words.isEmpty then AI.delPref st Ingest.prefPrivilegedWords else AI.setPrefValue st Ingest.prefPrivilegedWords (enc words)
  if senders.isEmpty then AI.delPref st Ingest.prefPrivilegedSenders else AI.setPrefValue st Ingest.prefPrivilegedSenders (enc senders)
  let n ← AI.recomputePrivileged st
  return redirect s!"/settings/ai?saved=lists&changed={n}" 303

/-- `POST /m/:id/summary` (form): summarize the message's thread, back to the message. -/
def messageSummaryPost (p : Page) : IO Resp := do
  let id := p.c.param "id"
  let st := p.c.st
  if !(← AI.enabled st) then return notFoundPage p
  let some r ← st.db.queryOne "SELECT thread_id FROM messages WHERE id = ? AND deleted = 0" #[id] | return notFoundPage p
  let some t := r.text? "thread_id" | return redirect s!"/m/{id}?summary=none" 303
  let refresh := formField (parseForm p.req.body) "refresh" == "1"
  match ← AI.summarize st t refresh with
  | .ok _ => return redirect s!"/m/{id}#ai-summary" 303
  | .error (403, _) => return redirect s!"/m/{id}?summary=privileged" 303
  | .error _ => return redirect s!"/m/{id}?summary=failed" 303

/-- The message page's summary section (`Pages.message`): nothing while the AI is off or for a
message without a thread; for a withheld thread a note; else the cached summary and the button. -/
def summarySection (st : State) (id : String) (threadId : Option String) (flag : Option String) : IO Node := do
  if !(← AI.enabled st) then return empty
  let some t := threadId | return empty
  if ← AI.threadWithheld st t then
    return el "section" [("class", "ai-summary"), ("id", "ai-summary")] [
      elt "p" [("class", "hint")] "This thread is privileged, so it is not sent to the AI."]
  let cached ← AI.cachedSummary? st t
  let note : Node := match flag with
    | some "failed" => flash "err" "The summary did not come back. Try again."
    | some "privileged" => flash "err" "This thread is privileged."
    | _ => empty
  return el "section" [("class", "ai-summary card"), ("id", "ai-summary"), ("data-thread", t)] [
    note,
    match cached with
    | some r => el "div" [("class", "ai-summary-text")] [
        elt "p" [] (r.text "summary"),
        elt "p" [("class", "hint")] s!"Summary by {r.text "model"}"]
    | none => el "div" [("class", "ai-summary-text")] [],
    el "form" [("method", "post"), ("action", s!"/m/{id}/summary"), ("class", "inline ai-summarize")] [
      when cached.isSome (el "input" [("type", "hidden"), ("name", "refresh"), ("value", "1")]),
      elt "button" [("type", "submit"), ("data-thread", t)] (if cached.isSome then "Summarize again" else "Summarize thread")]]

def aiRoutes : Array Route := #[
  route "GET" "/settings/ai" .«public» (guarded false aiSettings),
  route "POST" "/settings/ai" .«public» (guarded true aiSettingsPost),
  route "POST" "/settings/ai/privileged" .«public» (guarded true aiPrivilegedPost),
  route "POST" "/m/:id/summary" .«public» (guarded true messageSummaryPost)
]

end Server.Pages
