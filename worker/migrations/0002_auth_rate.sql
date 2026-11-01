-- Per-client rate limit on the public sign-in endpoints (/auth/login, /auth/code, /signin/code):
-- 10 an hour. `key` is `<bucket>:<sha256 hex of cf-connecting-ip>`, so no address is stored.
-- The daily cron deletes windows older than an hour.
CREATE TABLE auth_rate (
  key          TEXT PRIMARY KEY,
  window_start INTEGER NOT NULL,
  count        INTEGER NOT NULL
);
