#!/usr/bin/env bash
# docs/SPEC.md from the tests: every suite's passing checks, one bullet per `ok` sentence, in the
# order the suite runs them.
#
#   scripts/spec.sh            build, run every suite, rewrite docs/SPEC.md
#   scripts/spec.sh --check    the same, but fail (and print the diff) if docs/SPEC.md differs
#   scripts/spec.sh --no-build skip `lake build` (CI builds in an earlier step)
#
# Suites: the Lean test executables (core-test, address-test, ical-test, junk-test, server-test),
# the Worker's vitest suite (test names from `vitest run --reporter=json`), and the step names of
# scripts/smoke-cli.sh. Any FAIL, or any suite exiting non-zero, stops the script and writes nothing.
#
# Deterministic: the repo path, ULIDs, loopback ports and temp paths are replaced by placeholders,
# no timings are read (vitest's durations are separate JSON fields), and the vitest files are
# listed by name. The one volatile line is the date in the header, and it changes only
# when the body does: an unchanged body keeps the committed file as it is, and --check compares
# bodies only.
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
spec="$root/docs/SPEC.md"
check=0
build=1
for a in "$@"; do
  case "$a" in
    --check) check=1 ;;
    --no-build) build=0 ;;
    *) echo "usage: scripts/spec.sh [--check] [--no-build]" >&2; exit 2 ;;
  esac
done

tmp="$(mktemp -d "${TMPDIR:-/tmp}/leanmail-spec.XXXXXX")"
trap 'rm -rf "$tmp"' EXIT

say() { echo "spec: $*" >&2; }
die() { echo "spec: FAIL: $*" >&2; exit 1; }

# The placeholders. ULIDs are 26 Crockford base32 characters; ports follow a loopback host.
normalize() {
  SPEC_ROOT="$root" perl -pe '
    s/\Q$ENV{SPEC_ROOT}\E/./g;
    s/\b[0-9A-HJKMNP-TV-Z]{26}\b/<ulid>/g;
    s#(127\.0\.0\.1|localhost|\[::1\]):\d+#$1:<port>#g;
    s#(?:/private)?/(?:tmp|var/folders)/[^ ,;)\n]*#<tmp>#g;
  '
}

lean_suites=(core-test address-test ical-test junk-test server-test)
bin="$root/lean/.lake/build/bin"

if [ "$build" = 1 ]; then
  say "lake build ${lean_suites[*]} leanmail leanmail-server"
  ( cd "$root/lean" && lake build "${lean_suites[@]}" leanmail leanmail-server >"$tmp/lake.log" 2>&1 ) \
    || { tail -40 "$tmp/lake.log" >&2; die "lake build"; }
fi

body="$tmp/body.md"
counts="$tmp/counts"
: >"$body"
: >"$counts"

# one Lean executable: its `ok   ` lines become bullets, any `FAIL` line stops everything
lean_suite() {
  local name="$1"
  say "$name"
  ( cd "$root/lean" && "$bin/$name" ) >"$tmp/$name.out" 2>"$tmp/$name.err" \
    || { grep '^FAIL' "$tmp/$name.out" >&2 || tail -20 "$tmp/$name.out" "$tmp/$name.err" >&2; die "$name exited non-zero"; }
  if grep -q '^FAIL' "$tmp/$name.out"; then grep '^FAIL' "$tmp/$name.out" >&2; die "$name has failures"; fi
  grep '^ok   ' "$tmp/$name.out" | sed 's/^ok   //' | normalize >"$tmp/$name.ok"
  local n
  n="$(wc -l <"$tmp/$name.ok" | tr -d ' ')"
  [ "$n" -gt 0 ] || die "$name printed no ok lines"
  echo "$name $n" >>"$counts"
  {
    echo "## $name"
    echo
    echo "\`lean/test\`, run as \`lake build $name && ./.lake/build/bin/$name\`; $n checks."
    echo
    sed 's/^/- /' "$tmp/$name.ok"
    echo
  } >>"$body"
}

for s in "${lean_suites[@]}"; do lean_suite "$s"; done

# the Worker: vitest's JSON report, test files by name, tests in declaration order
say "worker: vitest run"
( cd "$root/worker" && npx vitest run --reporter=json --outputFile="$tmp/vitest.json" ) >"$tmp/vitest.log" 2>&1 \
  || { tail -40 "$tmp/vitest.log" >&2; die "worker tests"; }
node - "$tmp/vitest.json" "$root/worker/" >"$tmp/worker.md" 2>"$tmp/worker.err" <<'JS' || { cat "$tmp/worker.err" >&2; die "reading the vitest report"; }
const fs = require("fs");
const [file, prefix] = process.argv.slice(2);
const r = JSON.parse(fs.readFileSync(file, "utf8"));
if (r.numFailedTests || r.numFailedTestSuites) { console.error(`${r.numFailedTests} failed tests`); process.exit(1); }
const files = r.testResults.map((f) => ({ ...f, rel: f.name.startsWith(prefix) ? f.name.slice(prefix.length) : f.name }))
  .sort((a, b) => (a.rel < b.rel ? -1 : a.rel > b.rel ? 1 : 0));
let n = 0;
const out = [];
for (const f of files) {
  const ok = f.assertionResults.filter((t) => t.status === "passed");
  if (!ok.length) continue;
  out.push(`### ${f.rel}`, "");
  for (const t of ok) {
    out.push(`- ${[...t.ancestorTitles, t.title].join(" › ")}`);
    n++;
  }
  out.push("");
}
process.stdout.write(`${n}\n${out.join("\n")}\n`);
JS
wn="$(head -1 "$tmp/worker.md")"
echo "worker $wn" >>"$counts"
{
  echo "## worker"
  echo
  echo "\`worker/test\`, run as \`npm test\` (vitest in the Workers pool); $wn tests."
  echo
  tail -n +2 "$tmp/worker.md" | normalize
} >>"$body"

# the CLI end to end: smoke-cli.sh's step names
say "scripts/smoke-cli.sh"
"$root/scripts/smoke-cli.sh" >"$tmp/smoke.out" 2>"$tmp/smoke.err" \
  || { tail -30 "$tmp/smoke.out" "$tmp/smoke.err" >&2; die "smoke-cli"; }
grep -q '^smoke-cli: ok$' "$tmp/smoke.out" || die "smoke-cli did not print smoke-cli: ok"
grep '^smoke-cli: ' "$tmp/smoke.out" | grep -v '^smoke-cli: ok$' | sed 's/^smoke-cli: //' \
  | normalize >"$tmp/smoke.ok"
sn="$(wc -l <"$tmp/smoke.ok" | tr -d ' ')"
echo "cli-smoke $sn" >>"$counts"
{
  echo "## cli-smoke"
  echo
  echo "\`scripts/smoke-cli.sh\` (\`make cli-smoke\`): the \`leanmail\` CLI against a bare local server; $sn steps, each of which asserts on the output."
  echo
  sed 's/^/- /' "$tmp/smoke.ok"
} >>"$body"

total="$(awk '{s += $2} END {print s}' "$counts")"
summary="$(awk '{printf "%s%s %s", (NR > 1 ? ", " : ""), $1, $2}' "$counts")"

# the part --check compares: everything but the date line
{
  echo "$total passing checks: $summary."
  echo
  echo "Each bullet is the name of a check that passed, in the order its suite runs it. To change"
  echo "this file, change the tests and run \`make spec\`; CI fails when it is out of date."
  echo
  cat "$body"
} >"$tmp/rest.md"

existing_rest() { [ -f "$spec" ] && sed '1,4d' "$spec"; }

if [ "$check" = 1 ]; then
  if ! diff -u <(existing_rest || true) "$tmp/rest.md" >"$tmp/diff"; then
    cat "$tmp/diff" >&2
    die "docs/SPEC.md is out of date; run make spec and commit it"
  fi
  say "docs/SPEC.md is current ($total checks)"
  exit 0
fi

if [ -f "$spec" ] && diff -q <(existing_rest) "$tmp/rest.md" >/dev/null; then
  say "docs/SPEC.md unchanged ($total checks)"
  exit 0
fi

{
  echo "# LeanMail specification"
  echo
  echo "Generated $(date -u +%Y-%m-%d) (UTC) by \`scripts/spec.sh\` from the test suites. Do not edit by hand."
  echo
  cat "$tmp/rest.md"
} >"$spec"
say "wrote docs/SPEC.md ($total checks: $summary)"
