# LeanMail

A mail client and archive you run on your own Cloudflare account, written in Lean 4. Mail arrives
through Cloudflare Email Routing or from Gmail, is parsed and indexed by a Lean server running in a
Cloudflare Container, and is read in a server-rendered web app, a command-line client that doubles
as an MCP server for AI tools, and native iPhone, iPad and Mac apps written in Lean with
[LeanNative](https://github.com/LeanNative/leannative).

A hosted instance runs at **leanmail.io**. It is free; every sign-up is approved by hand.

## Shape

```
mail ─► Email Routing ─► Worker email(): route lookup ─► R2 raw/<account>/<ulid> ─► wake the container
browser / CLI / app ─► Worker fetch(): verify cookie or token ─► the account's container ─► Lean server
Lean server ─► libcurl ─► R2 (S3 API), Gmail, the optional AI endpoint, the Worker's /internal/* routes
Litestream ─► R2 db/<account>/ (SQLite restored on boot, replicated while awake)
```

- **One container per account.** Each account is its own process with its own SQLite file (FTS5
  for search). A self-hoster runs the same thing with one account.
- **A thin, fixed Worker.** Only what a Worker must do: receive mail, terminate TLS, route to the
  right container, run the daily cron, send mail and push notifications.
- **No end-to-end encryption.** The server reads your mail so it can index it, render it and,
  if you turn it on, hand it to an AI model. Secrets at rest are Cloudflare's.
- **AI is optional.** One client speaking the OpenAI chat-completions format: set a base URL, a key
  and a model and it is on; leave any of them unset and no AI route exists. It triages new mail
  (a chip in the inbox), summarizes threads and suggests replies on demand, and never sees mail
  you mark privileged (Settings → AI).
- **Sign in by passkey, magic link or a six-digit code.** Passkeys are WebAuthn ES256 with no
  attestation, added in Settings. The CLI and the apps use API tokens minted in Settings.

## Docs

- [docs/SELF-HOSTING.md](docs/SELF-HOSTING.md): run your own install on Cloudflare, in about an hour.
- [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md): the pieces, the rules the design keeps, the container's life.
- [docs/SPEC.md](docs/SPEC.md): what the tests check, generated from their names (`make spec`).
- [SECURITY.md](SECURITY.md): what is protected and what is not, and how to report a problem.

## What works today

- **Receiving.** Email Routing → the Worker → the R2 spool → the account's container, which parses
  MIME (RFC 2047/2231, nested multipart, charsets, attachments, invites), dedupes, threads and indexes
  it in SQLite FTS5. Per-address routing (keep, keep and forward, forward, drop) with a loop guard,
  edited in Settings → Routing and applied to Cloudflare from there when given an API token.
- **Reading.** A server-rendered web app: inbox, threads, messages, search (MailVault's grammar:
  `from:`, `subject:`, `has:`, `before:`, quoted phrases, negation), done / snooze / to-read, expiry
  with a keep-forever switch, blocks, and junk and phishing rules.
- **Writing.** Compose, reply and forward with correct threading headers, drafts with revisions,
  a Sent folder; sent through Cloudflare Email Sending.
- **Gmail.** Your own OAuth client: backfill, polling and Pub/Sub push, done-in-LeanMail archives in
  Gmail, junk reports as spam, Gmail send-as addresses.
- **Calendar.** Events from invites (iCalendar and RRULE), calendar pages.
- **Accounts.** A sign-up page (`/signup`) feeding the operator's approval queue (`SIGNUP_MODE`
  `approve`, `open` or `closed`), passkey, magic link and code sign-in, sessions you can revoke,
  scoped API tokens; one container and one database per account.
- **AI.** Optional triage, thread summaries and reply suggestions over any OpenAI-compatible
  endpoint, with privileged mail (by sender or keyword) withheld from it and from MCP.
- **CLI.** `leanmail`: search, recent, show, thread, attachments, import (.eml and mbox), actions,
  drafts, send, tokens, Gmail status, and `leanmail mcp`, a stdio MCP server for AI tools.
- **Backups.** Litestream streams every account's database to R2; raw mail stays in R2.
- **Push.** APNs from the Worker, for the native app.

In progress: the LeanNative iPhone, iPad and Mac app (`app/`) and push registration in LeanNative.

Passkeys belong to the host of `PUBLIC_ORIGIN`, which cannot be an IP address: to try them locally
run with `PUBLIC_ORIGIN=http://localhost:8787` behind `make worker-dev` (passkey sign-in asks the
Worker for its challenge, so a bare `make server` cannot do it; adding one works either way).

## Layout

```
lean/        one Lake package: Core (pure), Server (the container), Cli (leanmail); ffi/c (SQLite, curl, crypto shims)
app/         the LeanNative iOS and Mac Catalyst app (imports Core only)
worker/      the TypeScript Worker, wrangler.jsonc and the D1 migrations
server/      Dockerfile, entrypoint.sh, litestream.yml
web/         the stylesheet and script the pages use (served by the container at /static/)
docs/        ARCHITECTURE.md, SELF-HOSTING.md, SPEC.md (generated from test names)
scripts/     smoke-cli.sh, spec.sh, accept.sh
```

## Develop

You need elan (the toolchain is pinned in `lean/lean-toolchain`), Node 22+, Docker for the image,
and on macOS Homebrew's `curl` and `openssl@3` (the shims link them by path).

```bash
make build          # every Lean target
make test           # core-test, address-test, ical-test, junk-test, server-test
make cli-smoke      # the leanmail CLI end to end against a bare local server
make spec           # all of the above plus the Worker's tests, then regenerate docs/SPEC.md
make server         # the Lean server bare on :8080, data in ./data (no Worker, no auth)
make cli            # build the leanmail CLI
make worker-install # npm install in worker/; then `cd worker && npm test && npm run typecheck`
make docker         # the container image (linux/amd64; fetches the toolchain once into server/.cache)
make worker-dev     # wrangler dev, with the container built from server/Dockerfile
                    # (first: cp worker/.dev.vars.example worker/.dev.vars and set its secrets)
make accept         # end to end: the image behind wrangler dev, /health, /internal/health,
                    # a delivery to the local Email Routing endpoint, leanmail search
make app            # the native app's Lean and selftest (macOS; make app-ios for the simulator)
```

CI (`.github/workflows/lean.yml`) runs every Lean suite, `make cli-smoke`, the Worker's typecheck
and tests, the image build, and fails if `docs/SPEC.md` is stale. `deploy.yml` deploys `main` to
Cloudflare when the repository has deploy credentials.

## Status

Pre-release. Everything under "What works today" is implemented and covered by the suites
[docs/SPEC.md](docs/SPEC.md) lists. The native app is under way.

## Licence

Apache-2.0. The bundled SQLite is public domain.
