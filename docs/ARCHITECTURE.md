# Architecture

LeanMail is three programs built from one Lean 4 package, plus a small TypeScript Worker.

```
mail ─► Email Routing ─► Worker email(): route lookup (D1) ─► R2 raw/<account>/<ulid> + in/ marker ─► wake hint
browser / CLI / app ─► Worker fetch(): verify MAC on cookie or token ─► getContainer(MAIL, account) ─► Lean server
Lean server ─► libcurl ─► Worker /internal/{email/send,email/signin,push,routes,accounts,grants,routing/*}  (Worker-only bindings)
            ─► libcurl ─► R2 S3 API (SigV4), Gmail API, the optional AI endpoint
Litestream  ─► R2 db/<account>/        (restore on boot, replicate while awake)
```

## The pieces

| Piece | Where | Language | Job |
|---|---|---|---|
| Core | `lean/Core` | Lean | Pure domain: MIME, RFC 2047, addresses, RFC 822 builder, ULID, the query grammar and its FTS5 translation, iCalendar and RRULE, junk rules, model types and JSON codecs, a small HTML builder, MCP tool definitions as data. No I/O, no `@[extern]`. |
| Server | `lean/Server` | Lean | The container process: `Std.Http` server, SQLite with FTS5, ingest from the R2 spool, Gmail lanes, send, drafts, calendar, jobs, the optional AI client, MCP over HTTP, the server-rendered pages, the operator's sign-up queue. |
| CLI | `lean/Cli` | Lean | `leanmail`: subcommands against the HTTP API, and `leanmail mcp`, a stdio MCP server that proxies to the container's `/mcp`. |
| App | `app/` | Lean (LeanNative) | iPhone, iPad and Mac Catalyst. Imports `Core` only; talks to the API over the shell's HTTPS capability. |
| Worker | `worker/` | TypeScript | Only what a Worker must do: `email()` delivery to the R2 spool, TLS and routing to the right container, the daily cron, APNs, Email Sending, and the D1 projection tables the Worker reads without waking anything. |
| Shims | `lean/ffi/c` | C | SQLite statements (vendored amalgamation, FTS5 on), HTTPS over libcurl, digests, randomness and P-256 ECDSA verification (WebAuthn) over libcrypto. |

## Rules the design keeps

1. **The Worker decides routing itself.** `message.forward()` only exists in `email()`, so the vault / forward / drop decision, the loop guard and the forward breaker read a D1 `routes` table that containers project into. A delivery never waits for a container.
2. **Every credential the Worker routes on names the account.** Session cookies, API tokens, magic links, Gmail push URLs and OAuth states carry the account id and an HMAC under `SESSION_SECRET`. The Worker verifies the MAC and picks the container; the container re-verifies and checks revocation in its own database. The one exception is the passkey login challenge, minted before anyone is known: it is stateless (`v1.<rand>.<exp>.<mac>`, under a key derived from the master that no container holds), the account comes from the assertion's `userHandle`, and the Worker vouches for the checked challenge to that container with `x-passkey-challenge-ok: base64url(HMAC(K_acct, "passkey:" + token))`, which the container checks with its own secret before it verifies the signature and spends the challenge once (`lean/Server/Auth/Passkey.lean`, `worker/src/auth.ts`). A registration challenge is a random nonce stored in the account's database.
3. **The spool is the contract.** The Worker writes `raw/<account>/<ulid>` plus a zero-byte `in/<account>/<ulid>` marker and sends a wake hint. The container drains `in/` on boot and on every wake and deletes the marker after commit.
4. **Raw mail is the source of truth.** SQLite holds headers, plain text capped at 32 KiB per message, attachment metadata and the FTS index. HTML and attachments are re-parsed from raw on demand. Losing the database loses nothing that cannot be rebuilt.
5. **One account, one process, one file.** There are no `account_id` columns. The `_operator` container holds sign-ups, approvals and domain grants and projects the flat tables into D1.
6. **Crypto is a shim, not a port.** SHA-256, HMAC and P-256 verification come from libcrypto in the container. Nothing cryptographic is written in Lean.
7. **AI is one HTTP client.** `POST {AI_BASE_URL}/chat/completions` in the OpenAI format. Off unless base URL, key and model are all set. Replies are parsed leniently as plain JSON.

## The container's life

```
entrypoint.sh
  litestream restore -if-replica-exists /data/mail.db        # fresh disk every start
  exec litestream replicate -exec /usr/local/bin/leanmail-server
leanmail-server
  migrate the schema, probe FTS5, drain the spool, listen on :8080
  every response may carry X-Next-Wake: <seconds>; the Durable Object turns it into a scheduled wake
  SIGTERM: stop accepting, finish in-flight jobs, close, exit 0; Litestream syncs and exits
```

## Search

The query grammar is MailVault's: bare terms (prefix match at three or more characters), `"phrases"`,
`-negation`, `from: to: subject: list:` scoped to a column, and `has: is: in: before: after: source:` as
SQL predicates. The FTS5 `MATCH` string is built only from validated, quoted tokens; a term with a dot
or an at sign must be quoted or FTS5 rejects it. Ranking is `bm25` with column weights plus a recency
term.

## Trust

The server reads your mail. There is no end-to-end encryption. What protects the mail is Cloudflare's
encryption at rest, the Worker's authentication, per-account process isolation, and the operator's
honesty. Self-host if that last one is a problem.

Between the Worker and the containers (`worker/src/container.ts`, `worker/src/internal.ts`): every
request a client made reaches a container marked `x-lm-public: 1` (a client's own value is dropped),
and a container refuses its `/internal/*` routes to such a request whatever secret it carries; the
Worker canonicalises paths first and 404s any client path that decodes to `/internal/*`. An account
container sends mail only as an address it owns and may send from, with a `From:` header that matches
the envelope; its sign-in and welcome mail are written by the Worker (`/internal/email/signin`, to the
account's own email only). The Cloudflare API token is in the `_operator` container alone; accounts
plan and apply Email Routing through the Worker's `/internal/routing/{plan,apply}`, which the operator
answers after checking the caller's domain grants.

Inside a container (`lean/Server`):

- **Paths.** The server routes on the raw path only when it is plain (`Router.rawPathOk`): no empty
  segment (`//`), no `.`/`..`, no percent-encoded `/`, `\`, `.` or NUL, and a first segment of
  `[A-Za-z0-9_.-]+`, so `/%69nternal/…` is a 404, never `/internal/…`.
- **Routing grants.** An account adds, plans and applies only the domains the operator granted it
  (`GET /internal/grants`, failing closed); the operator answers the forwarded plan and apply only
  for domains in its own `domain_grants` for `x-routing-account`. A plan never lists other zones,
  and the Cloudflare account's destination addresses are only used to check the account's own
  forward targets.
- **MAIL_FROM is nobody's From.** `Send.sendable` refuses it for everyone (the dev bypass too), and
  `default_from` must be sendable.
- **Sign-in codes.** A wrong code costs each live link one try of its code (five, then the code is
  dead) but never spends the link; at most three live links at once.
- **Privileged mail and tokens.** An API token (anything but a person's browser session) never
  sees privileged mail: lists, search and threads leave it out, `/api/msg/:id` is `{id, withheld}`,
  its raw, HTML and attachments are 403, MCP lists omit it, and `reply_of`/`forward_of` on it is
  403 `privileged` in sends and draft saves. A session keeps full access.
- **Hostile MIME.** The parser keeps at most 32 multipart levels and 1,000 parts; the rest is one
  opaque body that is neither text nor an attachment, so parsing stays linear everywhere it runs.
- **Message HTML** is served `sandbox; default-src 'none'; img-src data: cid:` (remote images
  only with `?images=1`), no forms, no `<base>`; `/raw` is a `.eml` download under `sandbox`.
- **Outbound to untrusted URLs** (one-click unsubscribe, a non-local AI endpoint) goes through
  libcurl's safe profile: https only, no redirects, no proxy, and a socket opener that refuses
  loopback, private, link-local, CGNAT, multicast and reserved addresses (IPv6 and mapped forms
  too). Every response is capped (16 MiB safe, 64 MiB otherwise; 200 header lines); CR, LF and NUL
  in a URL or header are refused before libcurl.
- **Browser CSRF.** Pages send `Referrer-Policy: same-origin`; a form post needs `Origin =
  PUBLIC_ORIGIN`, or `Origin: null` only with `Sec-Fetch-Site: same-origin`; `/admin/*` mutations
  need `Origin = PUBLIC_ORIGIN` and `X-Requested-With: LeanMail`.
- **Sign-ups** never change an existing account's status (a disabled one stays disabled); the
  operator records `projected_at`/`provisioned_at` and retries either on a repeat sign-up or the
  `pending_projection` lane.
- **Boot.** Outside a bare localhost run the server refuses to start with the default
  `SESSION_SECRET` or an empty `INTERNAL_SECRET`.
