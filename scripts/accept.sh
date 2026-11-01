#!/usr/bin/env bash
# Local end-to-end acceptance (`make accept`): the real container image behind `wrangler dev`.
#
#   1. `make docker` (the image wrangler dev also builds from server/Dockerfile; the build cache is shared)
#   2. local D1: migrations, plus one route `accept@accept.test` → account `dev` (vault, can_send)
#   3. `npx wrangler dev` in worker/ (reads worker/.dev.vars: DEV_ACCOUNT=dev and the local secrets)
#   4. GET /health (the Worker), then GET /internal/health (the `dev` container, through its Durable
#      Object) with X-Internal-Secret = base64url(HMAC-SHA256(INTERNAL_SECRET, "dev"))
#   5. a fixture message POSTed to wrangler's local Email Routing endpoint
#      /cdn-cgi/handler/email?from=…&to=… (miniflare's /cdn-cgi/local/email): the Worker's email()
#      looks up the route, writes raw/dev/<ulid> + in/dev/<ulid> to local R2 and wakes the container
#   6. `leanmail search` through the Worker origin with --account dev, for that message; when the
#      delivery is not searchable (see below), the same message through `leanmail import`
#      (POST /api/import via the Worker), then `leanmail search` again
#   7. stop wrangler
#
# Known limit of a local run: the container reads the spool over R2's S3 API, and miniflare's R2
# has none, so a delivery reaches the container's spool only when worker/.dev.vars sets R2_BUCKET,
# R2_ENDPOINT, R2_ACCESS_KEY_ID and R2_SECRET_ACCESS_KEY for a real bucket. Without them step 5
# proves the Worker side (route lookup, spool write, wake) and step 6 falls back to import.
#
# ACCEPT_SKIP_DOCKER=1 skips step 1; ACCEPT_PORT (default 8787); ACCEPT_WORKER_DIR runs a copy of
# worker/ instead (to try a Worker change without touching the tree); ACCEPT_WAIT seconds for the
# container to answer (default 1800: the first wrangler dev builds the image under amd64 emulation).
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
worker="${ACCEPT_WORKER_DIR:-$root/worker}"
cli="$root/lean/.lake/build/bin/leanmail"
port="${ACCEPT_PORT:-8787}"
origin="http://127.0.0.1:$port"
wait_s="${ACCEPT_WAIT:-1800}"
acct=dev
rcpt="accept@accept.test"
sender="pat@example.com"

tmp="$(mktemp -d "${TMPDIR:-/tmp}/leanmail-accept.XXXXXX")"
wpid=""
started=""
cleanup() {
  if [ -n "$wpid" ] && kill -0 "$wpid" 2>/dev/null; then
    kill -INT "$wpid" 2>/dev/null || true
    for _ in $(seq 1 50); do kill -0 "$wpid" 2>/dev/null || break; sleep 0.2; done
    kill -TERM "$wpid" 2>/dev/null || true
    wait "$wpid" 2>/dev/null || true
  fi
  # wrangler dev sometimes leaves the Durable Object's container (or its proxy sidecar) running.
  # This removes every workerd-leanmail-MailContainer* container, including those of another
  # `wrangler dev` of this project on a different port.
  if [ -n "$started" ]; then
    docker ps -aq --filter name=workerd-leanmail-MailContainer | xargs docker rm -f >/dev/null 2>&1 || true
  fi
  rm -rf "$tmp"
}
trap cleanup EXIT

step() { echo "accept: $*"; }
fail() {
  echo "accept: FAIL: $*" >&2
  if [ -f "$tmp/wrangler.log" ]; then echo "--- wrangler dev log (tail)" >&2; tail -30 "$tmp/wrangler.log" >&2; fi
  exit 1
}

command -v docker >/dev/null || fail "docker is not installed"
docker info >/dev/null 2>&1 || fail "docker is not running"
if curl -s -o /dev/null "$origin/health"; then fail "something already listens on $origin; stop it or set ACCEPT_PORT"; fi

if [ "${ACCEPT_SKIP_DOCKER:-}" != 1 ]; then
  step "make docker"
  make -C "$root" docker >"$tmp/docker.log" 2>&1 || { tail -30 "$tmp/docker.log" >&2; fail "make docker"; }
fi

step "build the leanmail CLI"
( cd "$root/lean" && lake build leanmail >"$tmp/lake.log" 2>&1 ) || { tail -30 "$tmp/lake.log" >&2; fail "lake build leanmail"; }

secret_master="$(sed -n 's/^INTERNAL_SECRET=//p' "$worker/.dev.vars" | head -1)"
[ -n "$secret_master" ] || fail "worker/.dev.vars has no INTERNAL_SECRET"
grep -q "^DEV_ACCOUNT=$acct\$" "$worker/.dev.vars" || fail "worker/.dev.vars does not set DEV_ACCOUNT=$acct"

step "local D1: migrations and the route $rcpt -> $acct"
( cd "$worker" && npx wrangler d1 migrations apply leanmail-ctl --local ) >"$tmp/d1.log" 2>&1 \
  || { tail -20 "$tmp/d1.log" >&2; fail "d1 migrations apply --local"; }
now_ms="$(python3 -c 'import time; print(int(time.time() * 1000))')"
( cd "$worker" && npx wrangler d1 execute leanmail-ctl --local --command \
  "INSERT OR REPLACE INTO routes (address, account_id, mail, forward_to, can_send, updated_at) VALUES ('$rcpt', '$acct', 'vault', '[]', 1, $now_ms)" ) \
  >>"$tmp/d1.log" 2>&1 || { tail -20 "$tmp/d1.log" >&2; fail "d1 execute (route)"; }

step "wrangler dev on $origin"
( cd "$worker" && exec npx wrangler dev --ip 127.0.0.1 --port "$port" ) >"$tmp/wrangler.log" 2>&1 &
wpid=$!
started=1

for _ in $(seq 1 600); do
  curl -sf "$origin/health" >"$tmp/health.json" 2>/dev/null && break
  kill -0 "$wpid" 2>/dev/null || fail "wrangler dev exited"
  sleep 1
done
curl -sf "$origin/health" >"$tmp/health.json" || fail "the Worker never answered /health"
step "GET /health -> $(cat "$tmp/health.json")"

derived="$(python3 -I -c 'import base64, hashlib, hmac, sys
m = hmac.new(sys.argv[1].encode(), sys.argv[2].encode(), hashlib.sha256).digest()
print(base64.urlsafe_b64encode(m).decode().rstrip("="))' "$secret_master" "$acct")"

step "GET /internal/health as $acct (waiting up to ${wait_s}s for the container)"
start=$(date +%s)
code=""
last_note=0
while :; do
  code="$(curl -s -o "$tmp/ihealth.json" -w '%{http_code}' -H "x-account-id: $acct" -H "x-internal-secret: $derived" "$origin/internal/health" || true)"
  [ "$code" = 200 ] && break
  [ "$code" = 401 ] && fail "/internal/health refused the derived secret (401): $(cat "$tmp/ihealth.json")"
  kill -0 "$wpid" 2>/dev/null || fail "wrangler dev exited"
  el=$(( $(date +%s) - start ))
  [ "$el" -lt "$wait_s" ] || fail "the container never answered /internal/health (last status $code)"
  if [ $(( el - last_note )) -ge 60 ]; then step "  still waiting (${el}s, status $code)"; last_note=$el; fi
  sleep 3
done
step "GET /internal/health -> $(head -c 400 "$tmp/ihealth.json")"
step "  (unsigned: $(curl -s -o /dev/null -w '%{http_code}' "$origin/internal/health"); wrong secret: $(curl -s -o /dev/null -w '%{http_code}' -H "x-account-id: $acct" -H 'x-internal-secret: nope' "$origin/internal/health"))"

token="acceptance$(python3 -c 'import secrets; print(secrets.token_hex(6))')"
mid="<$token@accept.test>"
cat >"$tmp/fixture.eml" <<EML
From: Pat Example <$sender>
To: Accept <$rcpt>
Subject: Acceptance $token
Date: $(LC_ALL=C date -u '+%a, %d %b %Y %H:%M:%S +0000')
Message-ID: $mid
MIME-Version: 1.0
Content-Type: text/plain; charset=utf-8

This message is the local acceptance fixture. Its marker word is $token.
EML
# the wire format is CRLF
python3 -I -c 'import sys; p = sys.argv[1]; d = open(p, "rb").read().replace(b"\r\n", b"\n").replace(b"\n", b"\r\n"); open(p, "wb").write(d)' "$tmp/fixture.eml"

step "POST /cdn-cgi/handler/email?from=$sender&to=$rcpt"
code="$(curl -s -o "$tmp/email.json" -w '%{http_code}' -X POST --data-binary @"$tmp/fixture.eml" \
  "$origin/cdn-cgi/handler/email?from=$sender&to=$rcpt")"
step "  -> $code $(head -c 300 "$tmp/email.json")"
[ "$code" = 200 ] || fail "the local email handler answered $code"
# miniflare answers plain text, or a JSON outcome when asked for one; either way 200 is success
grep -qE '"outcome":"ok"|successfully processed' "$tmp/email.json" || fail "email() did not finish ok"
for _ in $(seq 1 20); do grep -q '"email.decision"' "$tmp/wrangler.log" && break; sleep 0.5; done
decision="$(grep -o '{[^{}]*"email.decision"[^{}]*}' "$tmp/wrangler.log" | tail -1 || true)"
[ -n "$decision" ] || fail "no email.decision log line in the wrangler dev output"
step "  Worker: $decision"
echo "$decision" | grep -q '"mail":"vault"' || fail "the delivery was not vaulted"

export LEANMAIL_CONFIG="$tmp/config/config.json"
step "leanmail login $origin --account $acct"
"$cli" login "$origin" --account "$acct" || fail "leanmail login"

search() {
  set +e
  "$cli" search "$token" >"$tmp/search.out" 2>"$tmp/search.err"
  local c=$?
  set -e
  return $c
}

delivered=0
step "leanmail search $token (the delivery; up to 30 s)"
for _ in $(seq 1 15); do
  if search; then
    if grep -q "Acceptance $token" "$tmp/search.out"; then delivered=1; break; fi
  else
    break
  fi
  sleep 2
done
if [ "$delivered" = 1 ]; then
  step "  found: $(head -1 "$tmp/search.out")"
else
  if ! search; then
    err="$(cat "$tmp/search.err")"
    case "$err" in
      *401*|*unauthorized*) fail "leanmail search through the Worker was refused: $err
  DEV_ACCOUNT routes the request to the $acct container, but the container authorizes a token
  route only with a bearer token, a session cookie or DEV_AUTH_BYPASS=1, and MailContainer.varsFor
  (worker/src/container.ts) does not pass DEV_AUTH_BYPASS through" ;;
      *) fail "leanmail search: $err" ;;
    esac
  fi
  if grep -q '^R2_BUCKET=' "$worker/.dev.vars"; then
    fail "the delivery is not searchable although .dev.vars sets R2_BUCKET"
  fi
  step "  not there: expected without R2_* in worker/.dev.vars (the container cannot read miniflare's R2)"
  step "leanmail import (POST /api/import through the Worker)"
  out="$("$cli" import "$tmp/fixture.eml" 2>&1)" || fail "leanmail import: $out"
  step "  $out"
  search || fail "leanmail search: $(cat "$tmp/search.err")"
  grep -q "Acceptance $token" "$tmp/search.out" || fail "search did not find the imported message: $(cat "$tmp/search.out")"
  step "  found: $(head -1 "$tmp/search.out")"
fi

step "stop wrangler dev"
kill -INT "$wpid"
for _ in $(seq 1 50); do kill -0 "$wpid" 2>/dev/null || break; sleep 0.2; done
kill -0 "$wpid" 2>/dev/null && kill -TERM "$wpid" 2>/dev/null || true
wait "$wpid" 2>/dev/null || true
wpid=""
if [ "$delivered" = 1 ]; then echo "accept: ok (delivery -> search)"; else echo "accept: ok (delivery reached the Worker spool; search via import)"; fi
