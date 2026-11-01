import Lean.Data.Json
import Core.Mime
import Core.Mime.Address
import Core.Mime.Date
import Core.Ulid
import Core.Text.Str
import Core.Text.Html
import Core.Search
import Server.State
import Server.Crypto
import Server.Clock
import Server.Log
import Server.Worker
import Server.Blocks
import Server.Calendar.Store
import Core.Junk
import Core.Mcp

/-! # Ingest: one raw message into the database

A port of MailVault's `ingestRaw` (`src/ingest.ts`) without the sealing: parse (≤ 8 MiB, else the
header block only), dedupe by `sha256("mid:" + Message-ID)` or the raw bytes, tell a repeat from a
different message under a reused Message-ID by a content hash, thread, and insert the message, its
text, its attachment metadata and its FTS row in one transaction. The raw object is the caller's:
the spool and `/api/import` store it first and pass its key. -/

namespace Server.Ingest

open Lean Server.Db Core.Text

/-- Bodies above this are not parsed; only their headers are read. -/
def parseMax : Nat := 8 * 1024 * 1024
/-- `message_text.body` is capped here (UTF-8 bytes, cut at a character boundary). -/
def textMax : Nat := 32 * 1024
def snippetLen : Nat := 200
def threadWindowMs : Nat := 7 * 86400000

structure Incoming where
  raw : ByteArray
  /-- The store key of the raw object (`raw_key`). -/
  key : Option String
  rcpt : Option String
  /-- `route`, `gmail`, `import` or `sent`; anything else is stored as `route`. -/
  source : String
  receivedAt : Nat
  folder : String := "inbox"
  forcedId : Option String := none
  archivedAt : Option Nat := none
  /-- Overrides the account's retention. -/
  expiresAt : Option Nat := none
  /-- Keep forever (`expires_at` NULL), as the import's `X-Expires-At: never`. -/
  keepForever : Bool := false
  snoozedUntil : Option Nat := none
  toReadAt : Option Nat := none
  junkKind : Option String := none
  /-- Further recipients beside `rcpt` (the import's `X-Rcpts`). -/
  moreRcpts : List String := []
  /-- Gmail's labels for the message, for the junk check (`SPAM`). -/
  labels : List String := []
  deriving Inhabited

inductive IngestResult where
  | stored (id : String)
  | duplicate (id : String)
  /-- Stored straight into Junk because the sender is blocked. -/
  | blocked (id : String)
  deriving Repr, BEq, Inhabited

def IngestResult.id : IngestResult → String
  | .stored i | .duplicate i | .blocked i => i

/-! ## Pure helpers -/

/-- A JSON array of recipients (the import's `X-Rcpts`, the spool's `rcpts` metadata): its
strings, trimmed, empties dropped; `[]` for anything else. -/
def rcptsOf (v : Option String) : List String :=
  match v.map Json.parse with
  | some (.ok (.arr xs)) => xs.toList.filterMap fun
    | .str s => let t := Str.trim s; if t.isEmpty then none else some t
    | _ => none
  | _ => []

/-- Whether a newly stored message goes to the phone: only new mail as it arrives. An import (the
old vault's mail, already read there), a Sent copy and anything filed as junk never push. -/
def shouldPush (inc : Incoming) : Bool :=
  inc.source != "import" && inc.source != "sent" && inc.folder != "sent" && inc.junkKind.isNone

/-- Trimmed, lower-cased, surrounding `<>` stripped; `none` when nothing is left. -/
def normalizeMessageId (s : String) : Option String :=
  let t := (Str.trim s).toLower
  let t := if t.startsWith "<" then String.ofList (t.toList.drop 1) else t
  let t := if t.endsWith ">" then String.ofList (t.toList.dropLast) else t
  let t := Str.trim t
  if t.isEmpty then none else some t

/-- Every message id in a `References` / `In-Reply-To` value: the `<…>` runs, or failing those
the whitespace-separated words; normalized. -/
def messageIds (s : String) : Array String := Id.run do
  let a := s.toList.toArray
  let mut out := #[]
  let mut i := 0
  while i < a.size do
    if a[i]! == '<' then
      match Str.indexOfChar a '>' (i + 1) with
      | some e =>
        if let some m := normalizeMessageId (Str.slice a (i + 1) e) then
          if !out.contains m then out := out.push m
        i := e + 1
      | none => i := a.size
    else i := i + 1
  if out.isEmpty then
    for w in (s.split Str.isSpaceLike).toList do
      if let some m := normalizeMessageId w.toString then
        if !out.contains m then out := out.push m
  return out

private def collapseWs (s : String) : String :=
  " ".intercalate ((s.split (fun c => Str.isSpaceLike c || c == '\x0B' || c == '\x0C')).toList.map (·.toString) |>.filter (· ≠ ""))

/-- Lower-cased, leading `re:` `fwd:` `fw:` `aw:` stripped repeatedly, whitespace collapsed. -/
partial def subjectNorm (s : String) : String :=
  let t := collapseWs s.toLower
  let pre := ["re:", "fwd:", "fw:", "aw:"].find? (t.startsWith ·)
  match pre with
  | some p => subjectNorm (String.ofList (t.toList.drop p.length))
  | none => t

/-- At most `n` UTF-8 bytes, cut at a character boundary. -/
def capUtf8 (s : String) (n : Nat) : String × Bool := Id.run do
  if s.utf8ByteSize ≤ n then return (s, false)
  let mut out := ""
  let mut used := 0
  for c in s.toList do
    let w := c.utf8Size
    if used + w > n then break
    out := out.push c
    used := used + w
  return (out, true)

def snippetOf (s : String) : String := String.ofList ((collapseWs s).toList.take snippetLen)

/-- The header block of a message too large to parse: through the first blank line (at most
1 MiB when there is none). -/
def headerBlock (raw : ByteArray) : ByteArray := Id.run do
  let n := raw.size
  let mut i := 0
  while i + 1 < n do
    if raw[i]! == 10 && raw[i + 1]! == 10 then return raw.extract 0 (i + 2)
    if i + 3 < n && raw[i]! == 13 && raw[i + 1]! == 10 && raw[i + 2]! == 13 && raw[i + 3]! == 10 then
      return raw.extract 0 (i + 4)
    if i > 1024 * 1024 then break
    i := i + 1
  return raw.extract 0 (min n (1024 * 1024)) ++ "\r\n\r\n".toUTF8

structure Mailbox where
  name : String
  address : String
  deriving Inhabited, Repr, BEq

def mailboxes (raw : Option String) : Array Mailbox :=
  match raw with
  | none => #[]
  | some v => (Core.Mime.Address.parseFlat v).toArray.filterMap fun
      | .mailbox n a => some { name := n, address := (Str.trim a).toLower }
      | .group _ _ => none

def Mailbox.toJson (m : Mailbox) : Json := Json.mkObj [("name", Json.str m.name), ("address", Json.str m.address)]

def mailboxesJson (ms : Array Mailbox) : String := (Json.arr (ms.map Mailbox.toJson)).compress

/-- An FTS address column: every display name and address, with the parts of each address
(`Core.Search.indexText`). -/
def ftsAddresses (ms : Array Mailbox) : String :=
  Core.Search.indexText (" ".intercalate (ms.toList.map fun m => m.name ++ " " ++ m.address))

/-- Everything the database row needs from the bytes, before any lookup. -/
structure Parsed where
  msg : Core.Mime.Message
  truncatedParse : Bool
  messageId : Option String
  from_ : Option Mailbox
  to_ : Array Mailbox
  cc : Array Mailbox
  replyTo : Option String
  subject : String
  listId : Option String
  inReplyTo : Option String
  references : Array String
  text : String
  textCapped : String
  textTruncated : Bool
  snippet : String
  hasAttachments : Bool
  hasInvite : Bool
  /-- Hash input: what the message says. -/
  contentInput : String

def analyse (raw : ByteArray) : Parsed :=
  let big := raw.size > parseMax
  let msg := Core.Mime.parse (if big then headerBlock raw else raw)
  let subject := msg.subject.getD ""
  let from_ := (mailboxes (msg.rawHeader? "from"))[0]?
  let text := match msg.text with
    | some t => t
    | none => match msg.html with
      | some h => Html.toText h
      | none => ""
  let (capped, cut) := capUtf8 text textMax
  let snippet := if big then snippetOf subject else snippetOf text
  let listId := (msg.rawHeader? "list-id").bind fun v =>
    let a := v.toList.toArray
    match Str.indexOfChar a '<', Str.lastIndexOfChar a '>' with
    | some s, some e => if s < e then some (Str.slice a (s + 1) e) else some (Str.trim v)
    | _, _ => let t := Str.trim v; if t.isEmpty then none else some t
  let names := msg.attachments.toList.map (·.filename.getD "")
  { msg, truncatedParse := big
    messageId := msg.messageId.bind normalizeMessageId
    from_
    to_ := mailboxes (msg.rawHeader? "to")
    cc := mailboxes (msg.rawHeader? "cc")
    replyTo := (mailboxes (msg.rawHeader? "reply-to"))[0]?.map (·.address)
    subject
    listId
    inReplyTo := msg.inReplyTo.bind fun v => (messageIds v)[0]?
    references := (msg.references.map messageIds).getD #[]
    text, textCapped := capped, textTruncated := big || cut
    snippet
    hasAttachments := msg.attachments.any (!·.related)
    hasInvite := msg.attachments.any (fun a => a.method.isSome || a.mimeType == "text/calendar") ||
      Calendar.hasCalendarPart msg.attachments
    contentInput := "\n".intercalate ([(from_.map (·.address)).getD "", subject, (msg.date.getD ""), text] ++ names) }

/-! ## The database side -/

/-- The block a sender is under (`Server.Blocks`), outside a transaction. -/
def isBlocked (st : State) (addr : String) : IO Bool := do
  return (← st.db.withLock (Blocks.blockForUnlocked st.db addr)).isSome

/-- Mail the junk check may file by itself: live mail. An import is the old vault's mail as it
was filed there, a Sent copy is ours, and an explicit junk kind (Gmail's Spam) already decided. -/
def autoFiles (source : String) : Bool := source == "route" || source == "gmail"

/-- The kind a verdict files a message under, if it does. -/
def junkKindOf (v : Core.Junk.Verdict) : Option String :=
  match v.verdict with
  | .spam => some "spam"
  | .phishing => some "phishing"
  | _ => none

def validSources : List String := ["route", "gmail", "import", "sent"]

/-! ## Privileged mail (`Core.Mcp.Privilege`): withheld from MCP and the AI -/

def prefPrivilegedWords : String := "privileged_words"
def prefPrivilegedSenders : String := "privileged_senders"

/-- The owner's rules from `prefs` (JSON arrays): the words replace the default phrases when
set; the senders are addresses or domains. Under the caller's lock. -/
def privilegeRulesUnlocked (db : Db) : IO Core.Mcp.Privilege.Rules := do
  let get (k : String) : IO (Option (List String)) := do
    let rows ← db.queryUnlocked "SELECT value FROM prefs WHERE key = ?" #[k]
    return rows[0]?.bind fun r => Core.Mcp.Privilege.listOf (r.text "value")
  return { words := (← get prefPrivilegedWords).getD Core.Mcp.Privilege.defaultWords
           senders := (← get prefPrivilegedSenders).getD [] }

def privilegeRules (st : State) : IO Core.Mcp.Privilege.Rules := st.db.withLock (privilegeRulesUnlocked st.db)

private def retentionDays (st : State) : IO Nat := do
  let rows ← st.db.queryUnlocked "SELECT retention_days FROM account WHERE id = 1"
  return (rows[0]?.bind (·.nat? "retention_days")).getD st.config.retentionDaysDefault

private def recordDuplicate (st : State) (key : ByteArray) (id : String) (inc : Incoming) (now : Nat) : IO Unit := do
  let rcpt := inc.rcpt.map (·.toLower)
  st.db.execUnlocked "INSERT INTO duplicates (dedupe_key, source, rcpt, received_at) VALUES (?, ?, ?, ?)"
    #[key, inc.source, (rcpt : Value), now]
  for r in rcpt.toList ++ inc.moreRcpts.map (·.toLower) do
    st.db.execUnlocked "UPDATE messages SET rcpts = json_insert(rcpts, '$[#]', ?1), updated_at = ?3
      WHERE id = ?2 AND NOT EXISTS (SELECT 1 FROM json_each(messages.rcpts) WHERE value = ?1)" #[r, id, now]

/-- The thread: that of the closest referenced message we hold, else a thread with the same
normalized subject active within 7 days, else a new one. Updates the thread's count and range. -/
private def assignThread (st : State) (p : Parsed) (receivedAt now : Nat) : IO String := do
  let refs := (p.inReplyTo.toList ++ p.references.reverse.toList).eraseDups
  let mut found : Option String := none
  for r in refs do
    if found.isNone then
      let rows ← st.db.queryUnlocked
        "SELECT thread_id FROM messages WHERE message_id = ? AND thread_id IS NOT NULL LIMIT 1" #[r]
      found := rows[0]?.bind (·.text? "thread_id")
  let norm := subjectNorm p.subject
  if found.isNone && !norm.isEmpty then
    let rows ← st.db.queryUnlocked
      "SELECT id FROM threads WHERE subject_norm = ? AND last_at >= ? AND first_at <= ? ORDER BY last_at DESC LIMIT 1"
      #[norm, (receivedAt - threadWindowMs : Nat), receivedAt + threadWindowMs]
    found := rows[0]?.bind (·.text? "id")
  match found with
  | some tid =>
    st.db.execUnlocked "UPDATE threads SET count = count + 1, last_at = MAX(last_at, ?2), first_at = MIN(first_at, ?2),
      updated_at = ?3 WHERE id = ?1" #[tid, receivedAt, now]
    -- a thread found by reference may be missing its row (made before threads existed)
    st.db.execUnlocked "INSERT OR IGNORE INTO threads (id, subject_norm, first_at, last_at, count, updated_at)
      VALUES (?, ?, ?, ?, 1, ?)" #[tid, norm, receivedAt, receivedAt, now]
    return tid
  | none =>
    let tid := Core.Ulid.encode receivedAt (← Crypto.randomBytes 10)
    st.db.execUnlocked "INSERT INTO threads (id, subject_norm, first_at, last_at, count, updated_at)
      VALUES (?, ?, ?, ?, 1, ?)" #[tid, norm, receivedAt, receivedAt, now]
    return tid

/-- The one store path for every source. -/
def ingest (st : State) (inc : Incoming) : IO IngestResult := do
  let now ← Clock.nowMs
  let p := analyse inc.raw
  let source := if validSources.contains inc.source then inc.source else "route"
  let inc := { inc with source }
  let content := Crypto.sha256 p.contentInput.toUTF8
  let key0 : ByteArray := match p.messageId with
    | some m => Crypto.sha256Text ("mid:" ++ m)
    | none => Crypto.sha256 inc.raw
  let fromAddr := (p.from_.map (·.address)).getD ""
  let newId : String ← match inc.forcedId with
    | some i => pure (Core.Ulid.normalize i)
    | none => do pure (Core.Ulid.encode inc.receivedAt (← Crypto.randomBytes 10))
  let result ← st.db.transaction do
    -- an id we already hold (a marker drained twice, an import retried) is that message
    if inc.forcedId.isSome then
      if (← st.db.queryUnlocked "SELECT id FROM messages WHERE id = ?" #[newId]).size > 0 then
        return (IngestResult.duplicate newId, (none : Option String))
    -- dedupe: same key and same content is a repeat; same key, other content is another message
    let mut key : ByteArray := key0
    match (← st.db.queryUnlocked "SELECT id, content_hash FROM messages WHERE dedupe_key = ?" #[key0])[0]? with
    | some row =>
      let twin := row.text "id"
      if row.blob "content_hash" == content || p.messageId.isNone then
        recordDuplicate st key0 twin inc now
        return (.duplicate twin, none)
      let key1 := Crypto.sha256Text ("mid:" ++ p.messageId.getD "" ++ "#" ++ Crypto.toHex content)
      match (← st.db.queryUnlocked "SELECT id FROM messages WHERE dedupe_key = ?" #[key1])[0]? with
      | some row2 =>
        recordDuplicate st key1 (row2.text "id") inc now
        return (.duplicate (row2.text "id"), none)
      | none => key := key1
    | none => pure ()
    let id : String := newId
    -- sender blocks, then the junk check (never on our own Sent copies)
    let blockId ← if source == "sent" || fromAddr.isEmpty then pure none
      else Blocks.blockForUnlocked st.db fromAddr
    let blocked := blockId.isSome
    if let some b := blockId then Blocks.countHitUnlocked st.db b now
    let verdict : Option Core.Junk.Verdict ← if source == "sent" then pure none else do
      let own ← Blocks.ownDomainsUnlocked st.db
      pure (some (Core.Junk.check (Core.Junk.inputOfMessage p.msg own inc.labels)))
    let filed : Option String :=
      if blocked || inc.junkKind.isSome || !autoFiles source then none else verdict.bind junkKindOf
    let threadId ← assignThread st p inc.receivedAt now
    let expiresAt : Option Nat ←
      if inc.keepForever then pure none
      else match inc.expiresAt with
        | some e => pure (some e)
        | none => do pure (some (inc.receivedAt + (← retentionDays st) * 86400000))
    let rcpts := (inc.rcpt.toList ++ inc.moreRcpts).map (·.toLower) |>.eraseDups
    let junkKind := if blocked then some "blocked" else inc.junkKind.orElse fun _ => filed
    let junkAt : Option Nat := junkKind.map fun _ => now
    let archivedAt := if blocked then some (inc.archivedAt.getD now) else inc.archivedAt
    let rules ← privilegeRulesUnlocked st.db
    let privileged := Core.Mcp.Privilege.isPrivileged rules fromAddr ((p.from_.map (·.name)).getD "") p.subject p.textCapped
    st.db.execUnlocked "INSERT INTO messages (id, dedupe_key, content_hash, message_id, thread_id, source, folder,
        received_at, expires_at, size, has_attachments, has_invite, raw_key, rcpts, from_addr, from_name, to_json,
        cc_json, reply_to, subject, list_id, in_reply_to, references_json, snippet, text_truncated, snoozed_until,
        archived_at, to_read_at, junk_at, junk_kind, junk_verdict, updated_at, privileged)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      ((#[id, key, content, (p.messageId : Value), threadId, source, (if inc.folder == "sent" then "sent" else "inbox"),
        inc.receivedAt, (expiresAt : Value), inc.raw.size, p.hasAttachments, p.hasInvite, (inc.key : Value),
        (Json.arr (rcpts.toArray.map Json.str)).compress, fromAddr, (p.from_.map (·.name)).getD "",
        mailboxesJson p.to_, mailboxesJson p.cc, (p.replyTo : Value), p.subject, (p.listId : Value),
        (p.inReplyTo : Value), (Json.arr (p.references.map Json.str)).compress, p.snippet, p.textTruncated,
        (inc.snoozedUntil : Value), (archivedAt : Value), (inc.toReadAt : Value), (junkAt : Value),
        (junkKind : Value), ((verdict.map fun v => (Blocks.verdictJson v).compress) : Value), now] : Array Value).push (privileged : Value))
    st.db.execUnlocked "INSERT INTO message_text (message_id, body) VALUES (?, ?)" #[id, p.textCapped]
    for h : i in [:p.msg.attachments.size] do
      let a := p.msg.attachments[i]
      st.db.execUnlocked "INSERT INTO attachments (message_id, idx, filename, mime, size, content_id, inline)
        VALUES (?, ?, ?, ?, ?, ?, ?)"
        #[id, i, a.filename.getD "", (if a.mimeType.isEmpty then "application/octet-stream" else a.mimeType),
          a.content.size, (a.contentId : Value), a.related]
    let rowid := ((← st.db.queryUnlocked "SELECT rowid FROM messages WHERE id = ?" #[id])[0]?.map (·.int "rowid")).getD 0
    let ftsArgs : Array Value := #[.int rowid, ftsAddresses p.from_.toArray, ftsAddresses p.to_, ftsAddresses p.cc,
      Core.Search.indexText p.subject, Core.Search.indexText p.textCapped, Core.Search.indexText (p.listId.getD ""),
      Core.Search.indexText (" ".intercalate (p.msg.attachments.toList.filterMap (·.filename)))]
    st.db.execUnlocked "INSERT INTO fts_src (rowid, from_, to_, cc, subject, body, list_id, attachments)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?)" ftsArgs
    st.db.execUnlocked "INSERT INTO fts (rowid, from_, to_, cc, subject, body, list_id, attachments)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?)" ftsArgs
    -- invites: one events row per VEVENT, in the same transaction
    if p.hasInvite && !p.truncatedParse then
      discard <| Calendar.storeFromMessageUnlocked st.db id p.msg.attachments inc.receivedAt now
    return (if blocked then .blocked id else .stored id, junkKind)
  let (result, junkKind) := result
  match result with
  | .stored id =>
    Log.info "ingest.stored" [("source", Json.str source), ("bytes", Json.num inc.raw.size),
      ("partial", Json.bool p.truncatedParse), ("attachments", Json.bool p.hasAttachments),
      ("junk", match junkKind with | some k => Json.str k | none => Json.null)]
    if shouldPush { inc with junkKind } then Worker.push st id
  | .blocked _ => Log.info "ingest.blocked" [("source", Json.str source)]
  | .duplicate _ => Log.info "ingest.duplicate" [("source", Json.str source)]
  return result

end Server.Ingest
