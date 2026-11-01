#!/bin/sh
# Container entrypoint: restore the account's SQLite file from R2 if this is a fresh disk, then run
# the server under Litestream so every WAL frame streams back out. Without R2 credentials (local
# runs) the server starts bare.
set -eu

# `docker run IMAGE <command...>` runs that command instead (e.g. `/usr/local/bin/leanmail version`).
if [ "$#" -gt 0 ]; then exec "$@"; fi

DATA_DIR="${DATA_DIR:-/data}"
DB="$DATA_DIR/mail.db"
mkdir -p "$DATA_DIR"

if [ -n "${R2_BUCKET:-}" ] && [ -n "${R2_ACCESS_KEY_ID:-}" ]; then
  export LITESTREAM_ACCESS_KEY_ID="$R2_ACCESS_KEY_ID"
  export LITESTREAM_SECRET_ACCESS_KEY="$R2_SECRET_ACCESS_KEY"
  if [ ! -f "$DB" ]; then
    echo '{"ev":"litestream.restore","db":"'"$DB"'"}'
    litestream restore -if-replica-exists -config /etc/litestream.yml "$DB" || {
      echo '{"ev":"litestream.restore.failed"}'; exit 1; }
  fi
  echo '{"ev":"litestream.replicate"}'
  exec litestream replicate -config /etc/litestream.yml -exec "/usr/local/bin/leanmail-server"
else
  echo '{"ev":"litestream.skipped","reason":"no R2 credentials"}'
  exec /usr/local/bin/leanmail-server
fi
