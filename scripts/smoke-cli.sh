#!/usr/bin/env bash
# End-to-end check of the `leanmail` CLI against a bare local server.
# Starts leanmail-server on a free port with a temp DATA_DIR (DEV_AUTH_BYPASS, no Worker: sends
# fail with worker_failed), logs in, imports the fixture mail, reads it back, acts on it, drafts and
# sends, drives `leanmail mcp --local` and `mcp --proxy` (against a python3 stub), and stops the server. Prints `smoke-cli: ok`; exits non-zero on any failure.
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
bin="$root/lean/.lake/build/bin"
cli="$bin/leanmail"
server="$bin/leanmail-server"
fixtures="$root/lean/test/fixtures/eml"

for b in "$cli" "$server"; do
  [ -x "$b" ] || { echo "smoke-cli: $b is missing; run: cd lean && lake build leanmail leanmail-server" >&2; exit 1; }
done

tmp="$(mktemp -d "${TMPDIR:-/tmp}/leanmail-smoke.XXXXXX")"
pid=""
stub_pid=""
cleanup() {
  if [ -n "$stub_pid" ] && kill -0 "$stub_pid" 2>/dev/null; then kill "$stub_pid" 2>/dev/null || true; wait "$stub_pid" 2>/dev/null || true; fi
  if [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null; then kill -TERM "$pid" 2>/dev/null || true; wait "$pid" 2>/dev/null || true; fi
  rm -rf "$tmp"
}
trap cleanup EXIT

fail() { echo "smoke-cli: FAIL: $*" >&2; if [ -f "$tmp/server.log" ]; then echo "--- server log (tail)" >&2; tail -20 "$tmp/server.log" >&2; fi; exit 1; }
step() { echo "smoke-cli: $*"; }

port="$(python3 -c 'import socket; s = socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1]); s.close()')"
origin="http://127.0.0.1:$port"

step "server on $origin, DATA_DIR=$tmp/data"
mkdir -p "$tmp/data"
PORT="$port" DATA_DIR="$tmp/data" ACCOUNT_ID=dev LOG_LEVEL=warn DEV_AUTH_BYPASS=1 PUBLIC_ORIGIN="$origin" WORKER_ORIGIN=http://127.0.0.1:9 "$server" >"$tmp/server.log" 2>&1 &
pid=$!

for _ in $(seq 1 100); do
  if "$cli" health "$origin" >/dev/null 2>&1; then break; fi
  kill -0 "$pid" 2>/dev/null || fail "server exited during startup"
  sleep 0.1
done
"$cli" health "$origin" >/dev/null || fail "server never answered /health"

export LEANMAIL_CONFIG="$tmp/config/config.json"

step "login"
"$cli" login "$origin" --account dev || fail "login"
# GNU stat first: GNU `stat -f` means --file-system and prints a filesystem report instead of failing
mode="$(stat -c '%a' "$LEANMAIL_CONFIG" 2>/dev/null || stat -f '%Lp' "$LEANMAIL_CONFIG")"
[ "$mode" = "600" ] || fail "config mode is $mode, want 600"

step "import $fixtures"
out="$("$cli" import "$fixtures")" || fail "import exited $?: $out"
echo "  $out"
echo "$out" | grep -q "stored 15, duplicate 0, blocked 0, failed 0" || fail "first import: $out"

step "recent"
"$cli" recent >"$tmp/recent.txt" || fail "recent"
sed 's/^/  /' "$tmp/recent.txt"
n="$(wc -l <"$tmp/recent.txt" | tr -d ' ')"
[ "$n" = "15" ] || fail "recent listed $n lines, want 15"

step "search matte"
"$cli" search matte >"$tmp/search.txt" || fail "search"
sed 's/^/  /' "$tmp/search.txt"
n="$(wc -l <"$tmp/search.txt" | tr -d ' ')"
[ "$n" = "1" ] || fail "search matte listed $n lines, want 1"
id="$(awk '{print $1}' "$tmp/search.txt")"

step "show $id"
"$cli" show "$id" >"$tmp/show.txt" || fail "show"
sed -n '1,12p' "$tmp/show.txt" | sed 's/^/  /'
grep -q "^Subject: *Quote for 500 units, matte finish" "$tmp/show.txt" || fail "show did not print the subject"

step "attachment of the pdf fixture"
pdf_id="$("$cli" search 'subject:"Invoice 2026-0912"' | awk 'NR==1 {print $1}')"
[ -n "$pdf_id" ] || fail "could not find the invoice message"
idx="$("$cli" show "$pdf_id" | sed -n 's/^  \[\([0-9]*\)\] .*application\/pdf.*/\1/p' | head -1)"
[ -n "$idx" ] || fail "invoice message lists no pdf attachment"
mkdir -p "$tmp/dl"
( cd "$tmp/dl" && "$cli" attachment "$pdf_id" "$idx" ) || fail "attachment"
f="$(ls "$tmp/dl" | head -1)"
[ -n "$f" ] || fail "attachment wrote no file"
echo "  wrote $f"
[ "$(head -c 4 "$tmp/dl/$f")" = "%PDF" ] || fail "attachment $f does not start with %PDF"

step "import again"
out="$("$cli" import "$fixtures")" || fail "second import exited $?: $out"
echo "  $out"
echo "$out" | grep -q "stored 0, duplicate 15, blocked 0, failed 0" || fail "second import: $out"

step "action $id done"
out="$("$cli" action "$id" done)" || fail "action done exited $?: $out"
echo "  $out"
echo "$out" | grep -q "done: archived" || fail "action done: $out"
"$cli" recent --filter archived >"$tmp/archived.txt" || fail "recent --filter archived"
grep -q "^$id " "$tmp/archived.txt" || fail "recent --filter archived does not list $id"
n="$("$cli" recent | wc -l | tr -d ' ')"
[ "$n" = "14" ] || fail "inbox lists $n after done, want 14"

step "action $id snooze 1d, then inbox"
out="$("$cli" action "$id" snooze 1d)" || fail "action snooze exited $?: $out"
echo "  $out"
echo "$out" | grep -q "snoozed until" || fail "action snooze: $out"
"$cli" recent --filter snoozed | grep -q "^$id " || fail "recent --filter snoozed does not list $id"
"$cli" action "$id" inbox >/dev/null || fail "action inbox"
set +e; "$cli" action "$id" frobnicate >/dev/null 2>&1; code=$?; set -e
[ "$code" = "2" ] || fail "an unknown action exited $code, want 2"

step "draft new / save / show / send"
did="$("$cli" draft new)" || fail "draft new"
echo "  draft id $did"
cat >"$tmp/draft.json" <<JSON
{"from": "dev@localhost", "to": ["Pat <pat@example.com>"], "subject": "Smoke draft", "text": "Hello from the smoke test.\n"}
JSON
out="$("$cli" draft save "$did" --file "$tmp/draft.json")" || fail "draft save exited $?: $out"
echo "  $out"
echo "$out" | grep -q "revision 1  created" || fail "first draft save: $out"
out="$("$cli" draft save "$did" --file "$tmp/draft.json")" || fail "second draft save exited $?: $out"
echo "  $out"
echo "$out" | grep -q "revision 2  saved" || fail "second draft save: $out"
set +e; out="$("$cli" draft save "$did" --file "$tmp/draft.json" --revision 1 2>&1)"; code=$?; set -e
[ "$code" = "1" ] && echo "$out" | grep -q "holds revision 2" || fail "a stale draft save exited $code: $out"
"$cli" draft show "$did" >"$tmp/draft.txt" || fail "draft show"
sed 's/^/  /' "$tmp/draft.txt"
grep -q "^Subject: *Smoke draft" "$tmp/draft.txt" || fail "draft show did not print the subject"
"$cli" draft list | grep -q "^$did " || fail "draft list does not list $did"
set +e; "$cli" draft send "$did" >"$tmp/dsend.out" 2>"$tmp/dsend.err"; code=$?; set -e
sed 's/^/  /' "$tmp/dsend.out" "$tmp/dsend.err"
[ "$code" = "1" ] || fail "draft send on a bare server exited $code, want 1 (worker_failed)"
grep -q "send failed: worker_failed" "$tmp/dsend.err" || fail "draft send did not report worker_failed"
grep -q " failed$" "$tmp/dsend.out" || fail "draft send did not print <id> failed"
"$cli" draft show "$did" --json >"$tmp/draft.json.out" || fail "draft show --json"
grep -q '"send_status":"failed"' "$tmp/draft.json.out" || fail "the draft's send is not recorded as failed"

step "send --from dev@localhost"
printf 'Sent from the smoke test.\n' >"$tmp/body.txt"
set +e; "$cli" send --from dev@localhost --to pat@example.com --subject "Smoke send" --body-file "$tmp/body.txt" >"$tmp/send.out" 2>"$tmp/send.err"; code=$?; set -e
sed 's/^/  /' "$tmp/send.out" "$tmp/send.err"
[ "$code" = "1" ] || fail "send on a bare server exited $code, want 1 (worker_failed)"
grep -q "send failed: worker_failed; retry with --client-id" "$tmp/send.err" || fail "send did not report worker_failed"
"$cli" recent --filter sent >"$tmp/sent.txt" || fail "recent --filter sent"
sed 's/^/  /' "$tmp/sent.txt"
grep -q "Smoke send" "$tmp/sent.txt" || fail "recent --filter sent does not list the Sent copy"
grep -q "Smoke draft" "$tmp/sent.txt" || fail "recent --filter sent does not list the draft's Sent copy"

step "tokens create / list / revoke"
"$cli" tokens create --name smoke --scopes read,act --json >"$tmp/token.json" || fail "tokens create"
tid="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["id"])' "$tmp/token.json")"
"$cli" tokens list | grep -q "smoke" || fail "tokens list does not list the new token"
"$cli" tokens revoke "$tid" >/dev/null || fail "tokens revoke"
"$cli" tokens list | grep -q "revoked" || fail "tokens list does not show the revocation"

step "gmail status"
"$cli" gmail status >"$tmp/gmail.txt" || fail "gmail status"
sed 's/^/  /' "$tmp/gmail.txt"
grep -q "^connected false" "$tmp/gmail.txt" || fail "gmail status on a bare server"

step "mcp --local"
"$cli" mcp --local >"$tmp/mcp.out" 2>"$tmp/mcp.err" <<'RPC' || fail "mcp --local exited $?"
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"smoke","version":"0"}}}
{"jsonrpc":"2.0","method":"notifications/initialized"}
{"jsonrpc":"2.0","id":2,"method":"tools/list"}
{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"search_mail","arguments":{"query":"matte"}}}
RPC
cut -c1-160 "$tmp/mcp.out" | sed 's/^/  /'
n="$(wc -l <"$tmp/mcp.out" | tr -d ' ')"
[ "$n" = "3" ] || fail "mcp --local wrote $n lines for 3 requests and a notification"
sed -n 1p "$tmp/mcp.out" | grep -q '"name":"leanmail"' || fail "initialize does not name leanmail"
sed -n 1p "$tmp/mcp.out" | grep -q '"protocolVersion":"2025-06-18"' || fail "initialize protocolVersion"
sed -n 2p "$tmp/mcp.out" | grep -q '"inputSchema"' || fail "tools/list has no schemas"
sed -n 2p "$tmp/mcp.out" | grep -q '"send_draft"' || fail "tools/list lacks send_draft"
sed -n 3p "$tmp/mcp.out" | grep -q "$id" || fail "search_mail did not return $id"
sed -n 3p "$tmp/mcp.out" | grep -q '"isError":false' || fail "search_mail is an error"

step "mcp --proxy against a stub"
cat >"$tmp/stub.py" <<'PY'
import json, sys
from http.server import BaseHTTPRequestHandler, HTTPServer
class H(BaseHTTPRequestHandler):
    def log_message(self, *a): pass
    def do_POST(self):
        n = int(self.headers.get("Content-Length", "0"))
        req = json.loads(self.rfile.read(n))
        if self.path != "/mcp":
            self.send_response(404); self.end_headers(); return
        if "id" not in req:
            self.send_response(202); self.end_headers(); return
        body = json.dumps({"jsonrpc": "2.0", "id": req["id"], "result": {"stub": True, "method": req.get("method"),
            "auth": self.headers.get("Authorization"), "ctype": self.headers.get("Content-Type"),
            "session": self.headers.get("Mcp-Session-Id")}}, indent=2).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Mcp-Session-Id", "smoke-session")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)
HTTPServer(("127.0.0.1", int(sys.argv[1])), H).serve_forever()
PY
sport="$(python3 -c 'import socket; s = socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1]); s.close()')"
python3 -I "$tmp/stub.py" "$sport" 2>"$tmp/stub.log" &
stub_pid=$!
for _ in $(seq 1 50); do
  if python3 -c 'import socket,sys; socket.create_connection(("127.0.0.1", int(sys.argv[1])), 1)' "$sport" 2>/dev/null; then break; fi
  sleep 0.1
done
mkdir -p "$tmp/config2"
printf '{"server": "http://127.0.0.1:%s", "token": "lm1.smoke.token", "mcp": "proxy"}\n' "$sport" >"$tmp/config2/config.json"
# no flag: the config's "mcp": "proxy" chooses the mode
LEANMAIL_CONFIG="$tmp/config2/config.json" "$cli" mcp >"$tmp/proxy.out" 2>"$tmp/proxy.err" <<'RPC' || fail "mcp --proxy exited $?"
{"jsonrpc":"2.0","id":7,"method":"initialize","params":{"protocolVersion":"2025-06-18"}}
{"jsonrpc":"2.0","method":"notifications/initialized"}
{"jsonrpc":"2.0","id":"eight","method":"tools/list"}
RPC
sed 's/^/  /' "$tmp/proxy.out"
n="$(wc -l <"$tmp/proxy.out" | tr -d ' ')"
[ "$n" = "2" ] || fail "mcp --proxy wrote $n lines, want 2 (a 202 writes nothing)"
sed -n 1p "$tmp/proxy.out" | grep -q '"id":7' || fail "proxy lost the id"
sed -n 1p "$tmp/proxy.out" | grep -q '"auth":"Bearer lm1.smoke.token"' || fail "proxy did not pass the bearer token"
sed -n 1p "$tmp/proxy.out" | grep -q '"ctype":"application/json"' || fail "proxy content type"
sed -n 2p "$tmp/proxy.out" | grep -q '"session":"smoke-session"' || fail "proxy did not echo Mcp-Session-Id"
kill "$stub_pid"; wait "$stub_pid" 2>/dev/null || true; stub_pid=""
echo '{"jsonrpc":"2.0","id":9,"method":"ping"}' | LEANMAIL_CONFIG="$tmp/config2/config.json" "$cli" mcp --proxy >"$tmp/proxy2.out" 2>/dev/null || fail "mcp --proxy with the stub gone exited $?"
sed 's/^/  /' "$tmp/proxy2.out"
grep -q '"code":-32000' "$tmp/proxy2.out" && grep -q '"id":9' "$tmp/proxy2.out" || fail "an unreachable /mcp is not a -32000 error"

step "stop server"
kill -TERM "$pid"
wait "$pid" || fail "server exited non-zero on SIGTERM"
pid=""

echo "smoke-cli: ok"
