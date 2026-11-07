-- Sign-ups, which the Worker now answers itself. D1 is where accounts start: an `accounts` row is
-- `pending` until an admin approves its sign-up here.
CREATE TABLE signups (
  id         TEXT PRIMARY KEY,
  account_id TEXT NOT NULL,
  email      TEXT NOT NULL,
  note       TEXT NOT NULL DEFAULT '',
  created_at INTEGER NOT NULL,
  decided_at INTEGER,
  decision   TEXT CHECK (decision IN ('approved', 'rejected'))
);
CREATE INDEX signups_open ON signups (created_at) WHERE decision IS NULL;
