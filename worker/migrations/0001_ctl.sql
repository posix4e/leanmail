-- D1 `leanmail-ctl`: what the Worker reads without waking a container. Projections only; the
-- operator and account containers own the truth and push it here via /internal/{accounts,routes}.

CREATE TABLE accounts (
  id         TEXT PRIMARY KEY,
  email      TEXT UNIQUE NOT NULL,
  status     TEXT NOT NULL,              -- pending | active | disabled
  has_gmail  INTEGER NOT NULL DEFAULT 0,
  updated_at INTEGER NOT NULL
);

-- Which account a receiving domain belongs to.
CREATE TABLE domains (
  name       TEXT PRIMARY KEY,           -- lower-case
  account_id TEXT NOT NULL
);
CREATE INDEX domains_account ON domains (account_id);

-- Per-address policy. `address` is a lower-case address or `*@domain` for the domain default.
CREATE TABLE routes (
  address    TEXT PRIMARY KEY,
  account_id TEXT NOT NULL,
  mail       TEXT NOT NULL CHECK (mail IN ('vault', 'vault+forward', 'forward', 'drop')),
  forward_to TEXT NOT NULL DEFAULT '[]', -- JSON array of addresses
  can_send   INTEGER NOT NULL DEFAULT 0,
  updated_at INTEGER NOT NULL
);
CREATE INDEX routes_account ON routes (account_id);

-- The per-account forward breaker: forwards counted in a one-hour window.
CREATE TABLE forward_window (
  account_id   TEXT PRIMARY KEY,
  window_start INTEGER NOT NULL,
  count        INTEGER NOT NULL
);
