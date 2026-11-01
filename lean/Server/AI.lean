import Lean.Data.Json
import Core.Prompts
import Core.Mcp
import Core.Mime.Date
import Core.Text.Str
import Server.State
import Server.Router.Types
import Server.Clock
import Server.Log
import Server.Ingest

/-! # The optional AI

One HTTP client in the OpenAI chat-completions format: `POST {AI_BASE_URL}/chat/completions` with
`{model, messages: [system, user], temperature: 0, max_tokens}` and `Authorization: Bearer
{AI_API_KEY}`, the reply read leniently from `choices[0].message.content` (a string, or text
parts). That covers Workers AI, Anthropic's OpenAI-compatible endpoint, OpenAI, Ollama and AI
Gateway with no provider code. The request goes through `State.ai`, so tests answer with a fake.

It runs only when all three settings are present (`Config.aiEnabled`) and the owner's
`ai.enabled` pref is on (the default); otherwise its routes answer 404 and the triage lane stays
idle. Three things use it:

* **Triage** (`triageStep`, the `ai` lane in `Server.Jobs`): each new message from Email Routing
  or Gmail, not junk and not withheld, gets a label (`Core.Prompts.triageLabels`), a priority 1–5
  and a one-sentence reason in `ai_triage`, 20 per tick, after a rowid cursor in `jobs`. The
  inbox shows the label as a chip (`Api.withTriage`).
* **Thread summary**: `POST /api/threads/:id/summary` (token `read`), cached in `ai_summaries`
  until a newer message arrives in the thread (`?refresh=1` forces a new one).
* **Reply suggestion**: `POST /api/ai/draft {thread_id?, message_id?, intent}` (token `draft`)
  answers `{text}`; nothing is saved.

**Privileged mail is never sent.** A message is privileged when `Core.Mcp.Privilege` matches it
at ingest (`messages.privileged`); one privileged message withholds its whole thread
(`withheldSql`), from the AI here and from MCP (`Server.Mcp`). Nothing else is redacted. Logs
carry no mail content and no key. -/

namespace Server.AI

open Lean Server.Db Server.Router Core.Text

/-! ## Settings -/

/-- The owner's switch (`prefs`); default on once the AI is configured. -/
def prefEnabled : String := "ai.enabled"

def prefValue? (st : State) (key : String) : IO (Option String) := do
  return (← st.db.queryOne "SELECT value FROM prefs WHERE key = ?" #[key]).map (·.text "value")

def setPrefValue (st : State) (key value : String) : IO Unit := do
  st.db.exec "INSERT INTO prefs(key, value, updated_at) VALUES (?, ?, ?)
    ON CONFLICT(key) DO UPDATE SET value = excluded.value, updated_at = excluded.updated_at" #[key, value, ← Clock.nowMs]

def delPref (st : State) (key : String) : IO Unit := st.db.exec "DELETE FROM prefs WHERE key = ?" #[key]

def prefOn (st : State) : IO Bool := do
  match ← prefValue? st prefEnabled with
  | some v => return !["0", "false", "off", "no"].contains v.toLower
  | none => return true

/-- Configured and switched on. -/
def enabled (st : State) : IO Bool := do
  if !st.config.aiEnabled then return false
  prefOn st

/-! ## The client -/

/-- The text of `choices[0].message.content`: a string, or the `text` of its parts. -/
def contentOf (j : Json) : Option String := do
  let choices ← (j.getObjValAs? (Array Json) "choices").toOption
  let c ← choices[0]?
  let msg ← (c.getObjVal? "message").toOption <|> (c.getObjVal? "delta").toOption
  match msg.getObjVal? "content" with
  | .ok (.str s) => some s
  | .ok (.arr parts) =>
    some <| String.join <| parts.toList.filterMap fun p =>
      match p with
      | .str s => some s
      | _ => (p.getObjValAs? String "text").toOption
  | _ => (c.getObjValAs? String "text").toOption

/-- One chat completion: the reply's text, or a short error code (`ai_off`, `ai_transport`,
`ai_http_<status>`, `ai_reply`). -/
def chat (st : State) (system user : String) (maxTokens : Nat := 400) : IO (Except String String) := do
  let c := st.config
  if !c.aiEnabled then return .error "ai_off"
  let body := Json.mkObj [
    ("model", Json.str c.aiModel),
    ("messages", Json.arr #[
      Json.mkObj [("role", Json.str "system"), ("content", Json.str system)],
      Json.mkObj [("role", Json.str "user"), ("content", Json.str user)]]),
    ("temperature", Json.num 0), ("max_tokens", Json.num maxTokens)]
  let req : Core.Http.Request := {
    method := "POST", url := c.aiUrl, timeoutMs := 90000, body := body.compress.toUTF8
    headers := [("Content-Type", "application/json"), ("Accept", "application/json"),
                ("Authorization", s!"Bearer {c.aiApiKey}")] }
  let t0 ← IO.monoMsNow
  let resp ← try pure (some (← st.ai.request req)) catch _ => pure none
  let ms := (← IO.monoMsNow) - t0
  let some r := resp | do
    Log.warn "ai.failed" [("error", Json.str "ai_transport"), ("ms", Json.num ms)]
    return .error "ai_transport"
  if !r.isOk then
    Log.warn "ai.failed" [("error", Json.str s!"ai_http_{r.status}"), ("ms", Json.num ms)]
    return .error s!"ai_http_{r.status}"
  let text := (String.fromUTF8? r.body).getD ""
  match (Json.parse text).toOption.bind contentOf with
  | some s =>
    Log.info "ai.chat" [("ms", Json.num ms), ("bytes", Json.num s.utf8ByteSize)]
    return .ok s
  | none =>
    Log.warn "ai.failed" [("error", Json.str "ai_reply"), ("ms", Json.num ms)]
    return .error "ai_reply"

/-- A chat whose reply should hold a JSON object: the first `{…}` in it, parsed. -/
def json (st : State) (system user : String) (maxTokens : Nat := 400) : IO (Except String Json) := do
  match ← chat st system user maxTokens with
  | .error e => return .error e
  | .ok s =>
    match (Core.Prompts.firstObject s).bind fun o => (Json.parse o).toOption with
    | some j => return .ok j
    | none => return .error "ai_reply"

/-! ## Privileged mail -/

/-- Whether the message `m` is withheld: privileged itself, or in a thread with a privileged
message. The one predicate MCP, the triage, the summaries and the suggestions use. -/
def withheldSql (m : String := "m") : String :=
  s!"({m}.privileged = 1 OR ({m}.thread_id IS NOT NULL AND EXISTS (SELECT 1 FROM messages pv WHERE pv.thread_id = {m}.thread_id AND pv.privileged = 1 AND pv.deleted = 0)))"

def isWithheld (st : State) (id : String) : IO Bool := do
  return (← st.db.queryOne s!"SELECT 1 AS x FROM messages m WHERE m.id = ? AND {withheldSql}" #[id]).isSome

def threadWithheld (st : State) (threadId : String) : IO Bool := do
  return (← st.db.queryOne "SELECT 1 AS x FROM messages WHERE thread_id = ? AND privileged = 1 AND deleted = 0" #[threadId]).isSome

/-- Live messages withheld: `(privileged themselves, withheld in all)`. -/
def withheldCounts (st : State) : IO (Nat × Nat) := do
  let own := ((← st.db.queryOne "SELECT count(*) AS n FROM messages WHERE deleted = 0 AND privileged = 1").map (·.nat "n")).getD 0
  let all := ((← st.db.queryOne s!"SELECT count(*) AS n FROM messages m WHERE m.deleted = 0 AND {withheldSql}").map (·.nat "n")).getD 0
  return (own, all)

/-- Recompute `messages.privileged` for every message under the current rules (after the lists
change), 500 at a time. A message that becomes privileged loses its triage and its thread's
summary. The number changed. -/
def recomputePrivileged (st : State) : IO Nat := do
  let rules ← Ingest.privilegeRules st
  let mut cursor : Int := 0
  let mut changed := 0
  repeat
    let rows ← st.db.query "SELECT m.rowid AS rid, m.id, m.thread_id, m.from_addr, m.from_name, m.subject, m.privileged,
        COALESCE(t.body, '') AS body FROM messages m LEFT JOIN message_text t ON t.message_id = m.id
      WHERE m.rowid > ? ORDER BY m.rowid LIMIT 500" #[.int cursor.toInt64]
    if rows.isEmpty then break
    let flips := rows.filterMap fun r =>
      let now := Core.Mcp.Privilege.isPrivileged rules (r.text "from_addr") (r.text "from_name") (r.text "subject") (r.text "body")
      if now != r.bool "privileged" then some (r, now) else none
    if !flips.isEmpty then
      st.db.transaction do
        for (r, on) in flips do
          st.db.execUnlocked "UPDATE messages SET privileged = ? WHERE id = ?" #[on, r.text "id"]
          if on then
            st.db.execUnlocked "DELETE FROM ai_triage WHERE message_id = ?" #[r.text "id"]
            if let some t := r.text? "thread_id" then
              st.db.execUnlocked "DELETE FROM ai_summaries WHERE thread_id = ?" #[t]
    changed := changed + flips.size
    cursor := ((rows.back?.map fun (r : Row) => r.int "rid").getD 0).toInt
    if rows.size < 500 then break
  return changed

/-! ## Triage -/

def lane : String := "ai"
def triageBatch : Nat := 20
/-- On the first run the lane starts with mail received in this window, not the whole archive. -/
def seedWindowMs : Nat := 3 * 86400000

def cursor? (st : State) : IO (Option Int) := do
  let v := (← st.db.queryOne "SELECT cursor FROM jobs WHERE name = ?" #[lane]).bind (·.text? "cursor")
  return v.bind String.toInt?

def setCursor (st : State) (c : Int) : IO Unit := do
  st.db.exec "INSERT OR IGNORE INTO jobs (name) VALUES (?)" #[lane]
  st.db.exec "UPDATE jobs SET cursor = ? WHERE name = ?" #[toString c, lane]

/-- Triage up to `triageBatch` new messages. The number labelled. A transport or HTTP failure
stops the run with the cursor before that message (the lane records the error and the next tick
retries); a reply that is not a verdict is passed over. -/
def triageStep (st : State) : IO Nat := do
  if !(← enabled st) then return 0
  let now ← Clock.nowMs
  let start ← match ← cursor? st with
    | some c => pure c
    | none =>
      let seed := ((← st.db.queryOne "SELECT COALESCE(MAX(rowid), 0) AS r FROM messages WHERE received_at < ?"
        #[(now - seedWindowMs : Nat)]).map (·.int "r")).getD 0
      setCursor st seed.toInt
      pure seed.toInt
  let top := ((← st.db.queryOne "SELECT COALESCE(MAX(rowid), 0) AS r FROM messages").map (·.int "r")).getD 0
  let rows ← st.db.query s!"SELECT m.rowid AS rid, m.*, COALESCE(t.body, '') AS body FROM messages m
      LEFT JOIN message_text t ON t.message_id = m.id
    WHERE m.rowid > ? AND m.deleted = 0 AND m.source IN ('route', 'gmail') AND m.folder = 'inbox' AND m.junk_at IS NULL
      AND NOT {withheldSql} AND m.received_at >= ?
      AND NOT EXISTS (SELECT 1 FROM ai_triage a WHERE a.message_id = m.id)
    ORDER BY m.rowid LIMIT ?" #[.int start.toInt64, (now - seedWindowMs * 3 : Nat), triageBatch]
  let mut n := 0
  let mut cur := start
  for r in rows do
    let user := Core.Prompts.triageUser (r.text "from_name") (r.text "from_addr") (r.text "subject") (r.text? "list_id") (r.text "body")
    match ← json st Core.Prompts.triageSystem user 200 with
    | .ok j =>
      match Core.Prompts.parseTriage j with
      | some v =>
        st.db.exec "INSERT OR REPLACE INTO ai_triage (message_id, label, priority, reason, model, at) VALUES (?, ?, ?, ?, ?, ?)"
          #[r.text "id", v.label, v.priority, v.reason, st.config.aiModel, ← Clock.nowMs]
        n := n + 1
      | none => Log.warn "ai.triage_unparsed" []
    | .error "ai_reply" => Log.warn "ai.triage_unparsed" []
    | .error e =>
      setCursor st cur
      throw (IO.userError e)
    cur := (r.int "rid").toInt
  -- fewer than a batch: everything up to `top` was looked at
  setCursor st (if rows.size < triageBatch then max cur top.toInt else cur)
  return n

/-! ## Threads -/

private def pad2 (n : Nat) : String := if n < 10 then s!"0{n}" else toString n

def dateStr (ms : Nat) : String :=
  let (y, m, d) := Core.Mime.Date.civilFromDays (ms / 86400000 : Nat)
  let r := ms % 86400000
  s!"{y}-{pad2 m}-{pad2 d} {pad2 (r / 3600000)}:{pad2 (r / 60000 % 60)} UTC"

def threadMsgOf (r : Row) : Core.Prompts.ThreadMsg :=
  { fromName := r.text "from_name", fromAddr := r.text "from_addr", date := dateStr (r.nat "received_at"),
    subject := r.text "subject", text := r.text "body" }

/-- A thread's live messages, oldest first, as prompt input. -/
def threadMsgs (st : State) (threadId : String) : IO (List Core.Prompts.ThreadMsg) := do
  let rows ← st.db.query "SELECT m.from_name, m.from_addr, m.received_at, m.subject, COALESCE(t.body, '') AS body
      FROM messages m LEFT JOIN message_text t ON t.message_id = m.id
    WHERE m.thread_id = ? AND m.deleted = 0 ORDER BY m.received_at ASC, m.id ASC" #[threadId]
  return rows.toList.map threadMsgOf

def summaryJson (r : Row) : Json :=
  Json.mkObj [("summary", Json.str (r.text "summary")), ("model", Json.str (r.text "model")), ("at", Json.num (r.nat "at"))]

/-- The cached summary while no newer live message is in the thread. -/
def cachedSummary? (st : State) (threadId : String) : IO (Option Row) := do
  let some r ← st.db.queryOne "SELECT summary, model, at FROM ai_summaries WHERE thread_id = ?" #[threadId] | return none
  let newer ← st.db.queryOne "SELECT 1 AS x FROM messages WHERE thread_id = ? AND deleted = 0 AND received_at > ?" #[threadId, r.nat "at"]
  return if newer.isSome then none else some r

/-- The thread's summary, `{summary, model, at}`: cached, or asked for now. Refused (403
`privileged`) for a withheld thread. -/
def summarize (st : State) (threadId : String) (refresh : Bool := false) : IO (Except (Nat × String) Json) := do
  let msgs ← threadMsgs st threadId
  if msgs.isEmpty then return .error (404, "not found")
  if ← threadWithheld st threadId then return .error (403, "privileged")
  if !refresh then
    if let some r ← cachedSummary? st threadId then return .ok (summaryJson r)
  let subject := (msgs.head?.map (·.subject)).getD ""
  let digest := Core.Prompts.digest msgs Core.Prompts.summaryCap
  match ← chat st Core.Prompts.summarySystem (Core.Prompts.summaryUser subject digest) 500 with
  | .error e => return .error (502, e)
  | .ok s =>
    let s := Core.Prompts.capBytes (Str.trim s) 4000
    if s.isEmpty then return .error (502, "ai_reply")
    let now ← Clock.nowMs
    st.db.exec "INSERT OR REPLACE INTO ai_summaries (thread_id, summary, model, at) VALUES (?, ?, ?, ?)"
      #[threadId, s, st.config.aiModel, now]
    return .ok (Json.mkObj [("summary", Json.str s), ("model", Json.str st.config.aiModel), ("at", Json.num now)])

/-! ## Reply suggestions -/

def draftContextCap : Nat := 12 * 1024

/-- A body for a reply (or a new message): `{text}`, never saved. The context is the thread of
`messageId` (or the message alone), or `threadId`; a withheld one is refused. -/
def suggest (st : State) (threadId messageId : Option String) (intent : String) : IO (Except (Nat × String) String) := do
  if intent.length > 2000 then return .error (400, "intent too long")
  let ctx ← match messageId, threadId with
    | some mid, _ => do
      let some r ← st.db.queryOne "SELECT m.*, COALESCE(t.body, '') AS body FROM messages m
          LEFT JOIN message_text t ON t.message_id = m.id WHERE m.id = ? AND m.deleted = 0" #[mid]
        | return .error (404, "message not found")
      if ← isWithheld st mid then return .error (403, "privileged")
      match r.text? "thread_id" with
      | some t => pure (some (← threadMsgs st t))
      | none => pure (some [threadMsgOf r])
    | none, some t => do
      let ms ← threadMsgs st t
      if ms.isEmpty then return .error (404, "thread not found")
      if ← threadWithheld st t then return .error (403, "privileged")
      pure (some ms)
    | none, none => pure none
  let owner := ((← st.db.queryOne "SELECT display_name FROM account WHERE id = 1").map (·.text "display_name")).getD ""
  let digest := ctx.map (Core.Prompts.digest · draftContextCap)
  match ← chat st (Core.Prompts.draftSystem owner) (Core.Prompts.draftUser intent digest) 700 with
  | .error e => return .error (502, e)
  | .ok s =>
    let s := Core.Prompts.capBytes (Str.trim s) 8192
    if s.isEmpty then return .error (502, "ai_reply")
    return .ok s

/-! ## Routes (404 while the AI is off) -/

def offOr (c : Ctx) (k : IO Resp) : IO Resp := do
  if !(← enabled c.st) then return Resp.notFound
  k

def errResp (e : Nat × String) : Resp := Resp.error e.1 e.2

/-- `GET /api/ai`: `{enabled, model}`, for the pages' script. -/
def statusGet (c : Ctx) (_ : Req) : IO Resp := offOr c do
  return Resp.ok (Json.mkObj [("enabled", Json.bool true), ("model", Json.str c.st.config.aiModel)])

/-- `POST /api/threads/:id/summary[?refresh=1]`. -/
def summaryPost (c : Ctx) (req : Req) : IO Resp := offOr c do
  let refresh := req.param? "refresh" == some "1" || jbool? req.json "refresh" == some true
  match ← summarize c.st (c.param "id") refresh with
  | .ok j => return Resp.ok j
  | .error e => return errResp e

/-- `POST /api/ai/draft {thread_id?, message_id?, intent}` → `{text}`. -/
def draftPost (c : Ctx) (req : Req) : IO Resp := offOr c do
  let j := req.json
  let nonEmpty (o : Option String) := o.bind fun s => let t := Str.trim s; if t.isEmpty then none else some t
  match ← suggest c.st (nonEmpty (jstr? j "thread_id")) (nonEmpty (jstr? j "message_id")) ((jstr? j "intent").getD "") with
  | .ok s => return Resp.ok (Json.mkObj [("text", Json.str s)])
  | .error e => return errResp e

def routes : Array Route := #[
  route "GET" "/api/ai" (.token "read") statusGet,
  route "POST" "/api/threads/:id/summary" (.token "read") summaryPost,
  route "POST" "/api/ai/draft" (.token "draft") draftPost
]

end Server.AI
