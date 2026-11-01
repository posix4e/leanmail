import Server.Db

namespace Server.Schema

/-! The per-account database. Migrations are numbered SQL strings applied in order and recorded in
`schema_migrations`; a container that boots on an older file applies the rest. -/

def migrations : Array (Nat × String) := #[
  (1, "
CREATE TABLE IF NOT EXISTS schema_migrations (version INTEGER PRIMARY KEY, applied_at INTEGER NOT NULL);
CREATE TABLE IF NOT EXISTS settings (key TEXT PRIMARY KEY, value TEXT NOT NULL, updated_at INTEGER NOT NULL);
CREATE TABLE IF NOT EXISTS prefs (key TEXT PRIMARY KEY, value TEXT NOT NULL, updated_at INTEGER NOT NULL);
CREATE TABLE IF NOT EXISTS account (
  id INTEGER PRIMARY KEY CHECK (id = 1),
  email TEXT NOT NULL,
  display_name TEXT NOT NULL DEFAULT '',
  default_from TEXT NOT NULL DEFAULT '',
  retention_days INTEGER,
  is_admin INTEGER NOT NULL DEFAULT 0,
  session_epoch INTEGER NOT NULL DEFAULT 1,
  created_at INTEGER NOT NULL,
  updated_at INTEGER NOT NULL
);
CREATE TABLE IF NOT EXISTS messages (
  id TEXT PRIMARY KEY,
  dedupe_key BLOB NOT NULL UNIQUE,
  content_hash BLOB,
  message_id TEXT,
  thread_id TEXT,
  source TEXT NOT NULL CHECK (source IN ('route','gmail','import','sent')),
  folder TEXT NOT NULL DEFAULT 'inbox' CHECK (folder IN ('inbox','sent')),
  received_at INTEGER NOT NULL,
  expires_at INTEGER,
  size INTEGER NOT NULL DEFAULT 0,
  has_attachments INTEGER NOT NULL DEFAULT 0,
  has_invite INTEGER NOT NULL DEFAULT 0,
  raw_key TEXT,
  rcpts TEXT NOT NULL DEFAULT '[]',
  from_addr TEXT NOT NULL DEFAULT '',
  from_name TEXT NOT NULL DEFAULT '',
  to_json TEXT NOT NULL DEFAULT '[]',
  cc_json TEXT NOT NULL DEFAULT '[]',
  reply_to TEXT,
  subject TEXT NOT NULL DEFAULT '',
  list_id TEXT,
  in_reply_to TEXT,
  references_json TEXT NOT NULL DEFAULT '[]',
  snippet TEXT NOT NULL DEFAULT '',
  text_truncated INTEGER NOT NULL DEFAULT 0,
  gm_msgid TEXT UNIQUE,
  gm_labels TEXT,
  send_status TEXT,
  send_error TEXT,
  cf_message_id TEXT,
  client_id TEXT UNIQUE,
  snoozed_until INTEGER,
  archived_at INTEGER,
  to_read_at INTEGER,
  junk_at INTEGER,
  junk_kind TEXT,
  junk_gmail INTEGER,
  privileged INTEGER NOT NULL DEFAULT 0,
  forwarded_at INTEGER,
  updated_at INTEGER NOT NULL,
  deleted INTEGER NOT NULL DEFAULT 0,
  deleted_at INTEGER,
  purge_after INTEGER,
  purged_at INTEGER
);
CREATE INDEX IF NOT EXISTS idx_messages_updated ON messages(updated_at, id);
CREATE INDEX IF NOT EXISTS idx_messages_received ON messages(received_at DESC, id DESC) WHERE deleted = 0;
CREATE INDEX IF NOT EXISTS idx_messages_expires ON messages(expires_at) WHERE expires_at IS NOT NULL AND deleted = 0;
CREATE INDEX IF NOT EXISTS idx_messages_thread ON messages(thread_id);
CREATE INDEX IF NOT EXISTS idx_messages_message_id ON messages(message_id);
CREATE INDEX IF NOT EXISTS idx_messages_purge ON messages(purge_after) WHERE deleted = 1 AND purged_at IS NULL;
CREATE TABLE IF NOT EXISTS message_text (message_id TEXT PRIMARY KEY REFERENCES messages(id) ON DELETE CASCADE, body TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS attachments (
  message_id TEXT NOT NULL REFERENCES messages(id) ON DELETE CASCADE,
  idx INTEGER NOT NULL,
  filename TEXT NOT NULL DEFAULT '',
  mime TEXT NOT NULL DEFAULT 'application/octet-stream',
  size INTEGER NOT NULL DEFAULT 0,
  content_id TEXT,
  PRIMARY KEY (message_id, idx)
);
CREATE TABLE IF NOT EXISTS threads (
  id TEXT PRIMARY KEY, subject_norm TEXT NOT NULL, first_at INTEGER NOT NULL, last_at INTEGER NOT NULL,
  count INTEGER NOT NULL DEFAULT 0, updated_at INTEGER NOT NULL
);
CREATE INDEX IF NOT EXISTS idx_threads_subject ON threads(subject_norm, last_at);
CREATE TABLE IF NOT EXISTS fts_src (
  rowid INTEGER PRIMARY KEY,
  from_ TEXT, to_ TEXT, cc TEXT, subject TEXT, body TEXT, list_id TEXT, attachments TEXT
);
CREATE VIRTUAL TABLE IF NOT EXISTS fts USING fts5(
  from_, to_, cc, subject, body, list_id, attachments,
  content='fts_src', content_rowid='rowid',
  tokenize=\"unicode61 remove_diacritics 2 tokenchars '@._-'\"
);
CREATE TABLE IF NOT EXISTS duplicates (dedupe_key BLOB NOT NULL, source TEXT NOT NULL, rcpt TEXT, received_at INTEGER NOT NULL);
CREATE TABLE IF NOT EXISTS sessions (
  id TEXT PRIMARY KEY, token_hash BLOB NOT NULL UNIQUE, created_at INTEGER NOT NULL, last_seen_at INTEGER NOT NULL,
  expires_at INTEGER NOT NULL, ua TEXT NOT NULL DEFAULT '', revoked_at INTEGER
);
CREATE TABLE IF NOT EXISTS magic_links (
  token_hash BLOB PRIMARY KEY, code6 TEXT NOT NULL, purpose TEXT NOT NULL, expires_at INTEGER NOT NULL,
  used_at INTEGER, created_at INTEGER NOT NULL
);
CREATE TABLE IF NOT EXISTS api_tokens (
  id TEXT PRIMARY KEY, token_hash BLOB NOT NULL UNIQUE, name TEXT NOT NULL DEFAULT '', scopes TEXT NOT NULL DEFAULT '[\"read\"]',
  created_at INTEGER NOT NULL, last_used_at INTEGER, revoked_at INTEGER
);
CREATE TABLE IF NOT EXISTS jobs (name TEXT PRIMARY KEY, lease_until INTEGER, last_run_at INTEGER, last_error TEXT, cursor TEXT);
"),
  -- attachments.inline: a part the HTML refers to (multipart/related with a Content-ID), listed
  -- but not counted by has_attachments. duplicates gets an index for the dedupe lookups.
  (2, "
ALTER TABLE attachments ADD COLUMN inline INTEGER NOT NULL DEFAULT 0;
CREATE INDEX IF NOT EXISTS idx_duplicates_key ON duplicates(dedupe_key);
"),
  -- M2: per-link code attempts; the operator's tables (empty in an account container).
  (3, "
ALTER TABLE magic_links ADD COLUMN attempts INTEGER NOT NULL DEFAULT 0;
CREATE TABLE IF NOT EXISTS accounts (
  id TEXT PRIMARY KEY,
  email TEXT NOT NULL UNIQUE,
  status TEXT NOT NULL CHECK (status IN ('pending','active','disabled')),
  is_admin INTEGER NOT NULL DEFAULT 0,
  approved_by TEXT,
  approved_at INTEGER,
  created_at INTEGER NOT NULL,
  updated_at INTEGER NOT NULL
);
CREATE TABLE IF NOT EXISTS signups (
  id TEXT PRIMARY KEY, email TEXT NOT NULL, note TEXT NOT NULL DEFAULT '', ip_hash BLOB,
  created_at INTEGER NOT NULL, decided_at INTEGER, decision TEXT
);
CREATE INDEX IF NOT EXISTS idx_signups_ip ON signups(ip_hash, created_at);
CREATE INDEX IF NOT EXISTS idx_signups_email ON signups(email);
CREATE TABLE IF NOT EXISTS domain_grants (domain TEXT PRIMARY KEY, account_id TEXT NOT NULL, created_at INTEGER NOT NULL);
CREATE INDEX IF NOT EXISTS idx_domain_grants_account ON domain_grants(account_id);
CREATE TABLE IF NOT EXISTS audit (at INTEGER NOT NULL, actor TEXT NOT NULL, action TEXT NOT NULL, target TEXT NOT NULL DEFAULT '');
"),
  -- M3: routing domains and addresses (what may be sent from), drafts, and the send lookups.
  -- `domains.sending` is a JSON array of the domain names mail may be sent from.
  (4, "
CREATE TABLE IF NOT EXISTS domains (
  name TEXT PRIMARY KEY,
  manage INTEGER NOT NULL DEFAULT 1,
  default_mail TEXT NOT NULL DEFAULT 'vault' CHECK (default_mail IN ('vault','vault+forward','forward','drop')),
  default_forward_to TEXT NOT NULL DEFAULT '[]' CHECK (json_valid(default_forward_to)),
  sending TEXT NOT NULL DEFAULT '[]' CHECK (json_valid(sending)),
  created_at INTEGER NOT NULL,
  updated_at INTEGER NOT NULL
);
CREATE TABLE IF NOT EXISTS addresses (
  address TEXT PRIMARY KEY,
  domain TEXT NOT NULL,
  mail TEXT NOT NULL DEFAULT 'vault' CHECK (mail IN ('vault','vault+forward','forward','drop')),
  forward_to TEXT NOT NULL DEFAULT '[]' CHECK (json_valid(forward_to)),
  can_send INTEGER NOT NULL DEFAULT 1,
  note TEXT NOT NULL DEFAULT '',
  created_at INTEGER NOT NULL,
  updated_at INTEGER NOT NULL
);
CREATE INDEX IF NOT EXISTS idx_addresses_domain ON addresses(domain);
CREATE TABLE IF NOT EXISTS drafts (
  id TEXT PRIMARY KEY,
  revision INTEGER NOT NULL DEFAULT 1,
  mutation_id TEXT NOT NULL,
  send_id TEXT NOT NULL UNIQUE,
  state TEXT NOT NULL DEFAULT 'draft' CHECK (state IN ('draft','sending','sent','deleted')),
  from_addr TEXT NOT NULL DEFAULT '',
  to_json TEXT NOT NULL DEFAULT '[]',
  cc_json TEXT NOT NULL DEFAULT '[]',
  bcc_json TEXT NOT NULL DEFAULT '[]',
  subject TEXT NOT NULL DEFAULT '',
  body_text TEXT NOT NULL DEFAULT '',
  body_html TEXT,
  in_reply_to TEXT,
  references_json TEXT NOT NULL DEFAULT '[]',
  reply_of TEXT,
  forward_of TEXT,
  attachments TEXT NOT NULL DEFAULT '[]',
  created_at INTEGER NOT NULL,
  updated_at INTEGER NOT NULL
);
CREATE INDEX IF NOT EXISTS idx_drafts_state ON drafts(state, updated_at);
CREATE INDEX IF NOT EXISTS idx_messages_sent ON messages(source, received_at) WHERE source = 'sent';
"),
  -- M4: the Gmail connection (`Server.Gmail`). One row. The OAuth client secret and the refresh
  -- token are stored as they are: the container's disk and its Litestream replica in R2 are the
  -- trust boundary (docs/ARCHITECTURE.md, "Trust"), as they are for the mail itself.
  -- `history_jobs`, `history_failures`, `gmail_mirror` and `gmail_spam` are MailVault's
  -- (migrations 0001, 0011, 0013) column for column, minus `account` and the sealed columns;
  -- `restoring` is new: an undo queued by `Gmail.Mirror.onAction` for the lane to send.
  (5, "
CREATE TABLE IF NOT EXISTS gmail_connection (
  id INTEGER PRIMARY KEY CHECK (id = 1),
  email TEXT,
  client_id TEXT NOT NULL,
  client_secret TEXT NOT NULL,
  refresh_token TEXT,
  history_id TEXT,
  watch_expiration INTEGER,
  last_push_at INTEGER,
  last_poll_at INTEGER,
  enabled INTEGER NOT NULL DEFAULT 1,
  last_error TEXT,
  created_at INTEGER NOT NULL,
  updated_at INTEGER NOT NULL
);
CREATE TABLE IF NOT EXISTS history_jobs (
  id TEXT PRIMARY KEY,
  state TEXT NOT NULL CHECK (state IN ('running','paused','done','failed','cancelled')),
  page_token TEXT,
  stall_count INTEGER NOT NULL DEFAULT 0,
  planned_total INTEGER,
  seen INTEGER NOT NULL DEFAULT 0,
  imported INTEGER NOT NULL DEFAULT 0,
  duplicates INTEGER NOT NULL DEFAULT 0,
  skipped INTEGER NOT NULL DEFAULT 0,
  oversize INTEGER NOT NULL DEFAULT 0,
  failed INTEGER NOT NULL DEFAULT 0,
  bytes INTEGER NOT NULL DEFAULT 0,
  expires_at INTEGER,
  keep_forever INTEGER NOT NULL DEFAULT 0,
  error_code TEXT,
  lease_until INTEGER,
  started_at INTEGER NOT NULL,
  updated_at INTEGER NOT NULL,
  finished_at INTEGER
);
CREATE INDEX IF NOT EXISTS idx_history_jobs_state ON history_jobs(state, updated_at);
CREATE TABLE IF NOT EXISTS history_failures (
  gm_msgid TEXT PRIMARY KEY,
  job_id TEXT NOT NULL,
  stage TEXT NOT NULL,
  code TEXT NOT NULL,
  attempts INTEGER NOT NULL DEFAULT 1,
  first_at INTEGER NOT NULL,
  last_at INTEGER NOT NULL
);
CREATE INDEX IF NOT EXISTS idx_history_failures_job ON history_failures(job_id);
CREATE TABLE IF NOT EXISTS gmail_mirror (
  message_id TEXT PRIMARY KEY,
  gm_msgid TEXT NOT NULL,
  archived_at INTEGER NOT NULL,
  state TEXT NOT NULL CHECK (state IN ('removing','archived','restoring','restored','skipped','failed','released')),
  reason TEXT,
  attempts INTEGER NOT NULL DEFAULT 0,
  updated_at INTEGER NOT NULL
) WITHOUT ROWID;
CREATE INDEX IF NOT EXISTS idx_gmail_mirror_held ON gmail_mirror(state) WHERE state IN ('removing','archived','restoring');
CREATE TABLE IF NOT EXISTS gmail_spam (
  message_id TEXT PRIMARY KEY,
  gm_msgid TEXT NOT NULL,
  junk_at INTEGER NOT NULL,
  state TEXT NOT NULL CHECK (state IN ('reporting','reported','restoring','restored','skipped','failed','released')),
  reason TEXT,
  had_inbox INTEGER NOT NULL DEFAULT 0,
  attempts INTEGER NOT NULL DEFAULT 0,
  updated_at INTEGER NOT NULL
) WITHOUT ROWID;
CREATE INDEX IF NOT EXISTS idx_gmail_spam_held ON gmail_spam(state) WHERE state IN ('reporting','reported','restoring');
"),
  -- M7: the calendar (`Server.Calendar`: one row per VEVENT, keyed by `Core.Ical.rowKey`, the
  -- record as `Core.Ical.Event.toJson` in `data`), the person's own answers beside each event,
  -- blocked senders (plaintext: no HMAC without end-to-end encryption), the phone's push tokens,
  -- and the junk check's verdict on each message (`Core.Junk.Verdict` as JSON).
  (6, "
CREATE TABLE IF NOT EXISTS events (
  id TEXT PRIMARY KEY,
  row_key TEXT NOT NULL UNIQUE,
  uid TEXT NOT NULL DEFAULT '',
  sequence INTEGER NOT NULL DEFAULT 0,
  stamp INTEGER NOT NULL DEFAULT 0,
  cancelled INTEGER NOT NULL DEFAULT 0,
  source TEXT NOT NULL CHECK (source IN ('mail','manual','import')),
  message_id TEXT,
  received_at INTEGER NOT NULL,
  expires_at INTEGER,
  kept INTEGER NOT NULL DEFAULT 0,
  data TEXT NOT NULL CHECK (json_valid(data)),
  updated_at INTEGER NOT NULL,
  deleted INTEGER NOT NULL DEFAULT 0,
  deleted_at INTEGER
);
CREATE INDEX IF NOT EXISTS idx_events_uid ON events(uid) WHERE deleted = 0;
CREATE INDEX IF NOT EXISTS idx_events_message ON events(message_id);
CREATE INDEX IF NOT EXISTS idx_events_expires ON events(expires_at) WHERE expires_at IS NOT NULL AND deleted = 0;
CREATE TABLE IF NOT EXISTS event_overrides (
  event_id TEXT PRIMARY KEY REFERENCES events(id) ON DELETE CASCADE,
  partstat TEXT,
  notes TEXT NOT NULL DEFAULT '',
  hidden INTEGER NOT NULL DEFAULT 0,
  updated_at INTEGER NOT NULL
);
CREATE TABLE IF NOT EXISTS sender_blocks (
  id TEXT PRIMARY KEY,
  scope TEXT NOT NULL CHECK (scope IN ('address','domain')),
  value TEXT NOT NULL UNIQUE,
  created_at INTEGER NOT NULL,
  hits INTEGER NOT NULL DEFAULT 0,
  last_hit_at INTEGER
);
CREATE TABLE IF NOT EXISTS push_tokens (
  token TEXT PRIMARY KEY,
  env TEXT NOT NULL DEFAULT 'prod' CHECK (env IN ('sandbox','prod')),
  created_at INTEGER NOT NULL,
  last_seen_at INTEGER NOT NULL
);
ALTER TABLE messages ADD COLUMN junk_verdict TEXT;
CREATE INDEX IF NOT EXISTS idx_messages_sending ON messages(send_status) WHERE send_status = 'sending';
"),
  -- M6b: the optional AI's triage verdict per message and summary per thread (`Server.AI`), and
  -- one content-free row per MCP `tools/call` (`Server.Mcp`; no arguments are kept).
  -- `messages.privileged` (migration 1) is set at ingest from `prefs.privileged_*`.
  (7, "
CREATE TABLE IF NOT EXISTS ai_triage (
  message_id TEXT PRIMARY KEY REFERENCES messages(id) ON DELETE CASCADE,
  label TEXT NOT NULL,
  priority INTEGER NOT NULL,
  reason TEXT NOT NULL DEFAULT '',
  model TEXT NOT NULL DEFAULT '',
  at INTEGER NOT NULL
);
CREATE TABLE IF NOT EXISTS ai_summaries (
  thread_id TEXT PRIMARY KEY,
  summary TEXT NOT NULL,
  model TEXT NOT NULL DEFAULT '',
  at INTEGER NOT NULL
);
CREATE TABLE IF NOT EXISTS mcp_log (
  at INTEGER NOT NULL,
  token_id TEXT,
  tool TEXT NOT NULL,
  ok INTEGER NOT NULL,
  ms INTEGER NOT NULL
);
CREATE INDEX IF NOT EXISTS idx_mcp_log_at ON mcp_log(at);
CREATE INDEX IF NOT EXISTS idx_messages_privileged ON messages(thread_id) WHERE privileged = 1;
"),
  -- M9b: WebAuthn credentials (`Server.Auth.Passkey`; ES256 only, the COSE key kept as sent) and
  -- one-use challenges: a registration's random challenge (kind `register`) until it is spent,
  -- and `sha256` of each spent login challenge (kind `login`) until its token expires.
  (8, "
CREATE TABLE IF NOT EXISTS passkeys (
  id TEXT PRIMARY KEY,
  credential_id BLOB NOT NULL UNIQUE,
  public_key_cose BLOB NOT NULL,
  alg INTEGER NOT NULL,
  sign_count INTEGER NOT NULL DEFAULT 0,
  transports TEXT NOT NULL DEFAULT '[]',
  name TEXT NOT NULL DEFAULT '',
  backup_eligible INTEGER NOT NULL DEFAULT 0,
  created_at INTEGER NOT NULL,
  last_used_at INTEGER
);
CREATE TABLE IF NOT EXISTS auth_challenges (
  challenge BLOB PRIMARY KEY,
  kind TEXT NOT NULL CHECK (kind IN ('register','login')),
  expires_at INTEGER NOT NULL
);
")
]

def currentVersion (db : Db.Db) : IO Nat := do
  try
    let rows ← db.query "SELECT COALESCE(MAX(version), 0) AS v FROM schema_migrations"
    return (rows[0]?.map (·.nat "v")).getD 0
  catch _ => return 0

/-- Apply every migration above the recorded version, each in its own transaction. -/
def migrate (db : Db.Db) : IO Nat := do
  let mut v ← currentVersion db
  for (n, sql) in migrations do
    if n > v then
      db.transaction do
        db.execUnlocked sql
        let now ← IO.monoMsNow
        db.execUnlocked "INSERT OR REPLACE INTO schema_migrations(version, applied_at) VALUES (?, ?)" #[n, now]
      v := n
  return v

/-- Proves FTS5 is compiled in: a MATCH against a temporary table. -/
def fts5Works (db : Db.Db) : IO Bool := do
  try
    db.exec "CREATE VIRTUAL TABLE IF NOT EXISTS temp.fts_probe USING fts5(t); INSERT INTO temp.fts_probe(t) VALUES ('lean mail works');"
    let rows ← db.query "SELECT count(*) AS n FROM temp.fts_probe WHERE fts_probe MATCH 'mail'"
    db.exec "DROP TABLE IF EXISTS temp.fts_probe"
    return (rows[0]?.map (·.nat "n")).getD 0 == 1
  catch _ => return false

end Server.Schema
