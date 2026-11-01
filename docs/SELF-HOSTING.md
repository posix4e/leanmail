# Self-hosting LeanMail

This guide takes you from nothing to reading your own mail on your own Cloudflare account. Budget
about an hour; the longest single step is the first container image build (10 to 20 minutes on an
Apple-silicon Mac, where the amd64 image is built under emulation).

Everything below uses `example.com` for your domain and `mail.example.com` for the host LeanMail
answers on. Replace both.

## What you need

| | |
|---|---|
| A Cloudflare account on the **Workers Paid** plan | Containers are not on the free plan. D1, R2 and Email Routing usage for one person fits the included amounts. |
| A domain whose DNS is on Cloudflare | Mail for it is delivered through Email Routing, so its MX records must be Cloudflare's. |
| Docker, running | `wrangler deploy` and `wrangler dev` build `server/Dockerfile` locally (linux/amd64). |
| Node 22 or later | For `wrangler` and the Worker's tests. |
| `elan` (the Lean toolchain manager) | Only to build the `leanmail` CLI or run the Lean tests on your machine. The image brings its own toolchain. |
| `zstd` and `curl` | `make toolchain-cache` uses them. |
| Optional: an APNs key | Push notifications to the iOS app. |
| Optional: an OpenAI-compatible AI endpoint | Workers AI, OpenAI, Anthropic's compatible endpoint, Ollama. The server does not use it yet (see "What works today" in the README); the settings are listed so an install is ready for it. |

```bash
git clone https://github.com/posix4e/leanmail && cd leanmail
cd worker && npm ci && npx wrangler login && cd ..
```

Run every `npx wrangler` command below from `worker/`.

## 1. Storage: D1 and R2

```bash
cd worker
npx wrangler d1 create leanmail-ctl
```

Copy the `database_id` it prints into `worker/wrangler.jsonc` (the `d1_databases` entry still holds
`00000000-…`). The deploy workflow refuses to run while the placeholder is there.

```bash
npx wrangler r2 bucket create leanmail-mail
```

The Worker reaches this bucket through its `RAW` binding. The container reaches it over R2's S3 API
(for the mail spool and for Litestream's database replica), so it needs an S3 key:

1. Dashboard: **R2 Object Storage → Manage API tokens → Create API token**.
2. Permission **Object Read & Write**, limited to the bucket `leanmail-mail`.
3. Keep the **Access Key ID**, the **Secret Access Key** and the S3 endpoint,
   `https://<ACCOUNT_ID>.r2.cloudflarestorage.com`.

```bash
npx wrangler d1 migrations apply leanmail-ctl --remote
```

## 2. Settings

Edit `vars` in `worker/wrangler.jsonc`:

| var | value |
|---|---|
| `PUBLIC_ORIGIN` | `https://mail.example.com`, no trailing slash. Links in mail, cookies, the OAuth redirect and the containers' calls back to the Worker all use it. |
| `SIGNUP_MODE` | `approve` (every sign-up waits for an admin), `open` (approved at once) or `closed` (no sign-ups; `ADMIN_EMAILS` included). |
| `RETENTION_DAYS_DEFAULT` | Days until newly stored mail expires, for accounts that have not chosen their own in Settings. `90` by default. |
| `ADMIN_EMAILS` | Add this key: comma-separated addresses. A sign-up from one of them is approved at once, as an admin. |

To answer on your own host, add a custom domain to the same file (or attach it later in the
dashboard under **Workers & Pages → leanmail → Settings → Domains & Routes**):

```jsonc
  "routes": [{ "pattern": "mail.example.com", "custom_domain": true }],
```

Then the secrets. Each command prompts for the value; nothing is written to the repository:

```bash
openssl rand -base64 32   # run twice, one value for each of the next two
npx wrangler secret put SESSION_SECRET
npx wrangler secret put INTERNAL_SECRET
npx wrangler secret put MAIL_FROM              # e.g. no-reply@example.com (see step 4)
npx wrangler secret put R2_BUCKET              # leanmail-mail
npx wrangler secret put R2_ENDPOINT            # https://<ACCOUNT_ID>.r2.cloudflarestorage.com
npx wrangler secret put R2_ACCESS_KEY_ID
npx wrangler secret put R2_SECRET_ACCESS_KEY
```

Optional ones, each independent:

| secrets | for |
|---|---|
| `APNS_TEAM_ID`, `APNS_KEY_ID`, `APNS_KEY_P8`, `APNS_TOPIC` | Push to the iOS app. `APNS_KEY_P8` is the `.p8` file's contents; `APNS_TOPIC` the app's bundle id. Push is off unless all four are set. |
| `AI_BASE_URL`, `AI_API_KEY`, `AI_MODEL` | An OpenAI-compatible chat-completions endpoint. Off unless all three are set. |
| `CF_API_TOKEN`, `CF_ACCOUNT_ID` | Lets Settings → Routing check and apply your Email Routing changes itself. The token needs Zone Read, Email Routing Edit and Email Sending Edit on your zones. Without them the page shows the plan and you apply it with the commands in step 5. (As of this writing the container does not yet receive these two, so use step 5's commands.) |
| `GOOGLE_PUBSUB_TOPIC` | `projects/<project>/topics/<topic>`, the default Pub/Sub topic for Gmail push (each account can set its own). |

`SESSION_SECRET` and `INTERNAL_SECRET` never reach a container: each account's container gets
`base64url(HMAC-SHA256(secret, account id))`, so one container can only speak for itself.

## 3. Deploy

```bash
cd ..
make toolchain-cache     # once: the Linux Lean toolchain (~600 MB) and Litestream, into server/.cache/
cd worker
npx wrangler deploy
```

`make toolchain-cache` resumes interrupted downloads; the image build copies the tarballs instead
of downloading them inside the (emulated, flaky) build stage. `wrangler deploy` then builds
`server/Dockerfile` for linux/amd64, pushes the image to Cloudflare's registry and deploys the
Worker. On an Apple-silicon Mac the Lean build under emulation is the slow part (about 13 minutes
here); on an x86-64 Linux machine or in CI it is native and much faster. Later deploys reuse
Docker's layer cache unless `lean/` changed.

```bash
curl https://mail.example.com/health
# {"ok":true,"worker":"leanmail","version":"0.1.0"}
```

`/health` is the Worker alone. To ask a container, sign the request with that account's derived
internal secret (`_operator` is the operator's container, which holds sign-ups and approvals):

```bash
acct=_operator
sig=$(printf %s "$acct" | openssl dgst -sha256 -hmac "$INTERNAL_SECRET" -binary | openssl base64 -A | tr '+/' '-_' | tr -d '=')
curl -H "x-account-id: $acct" -H "x-internal-secret: $sig" https://mail.example.com/internal/health
```

The first call starts the container (see Troubleshooting for cold starts).

## 4. Sending: Email Sending for `MAIL_FROM`

Sign-in is by a link and a six-digit code sent from `MAIL_FROM`, so sending must work before
anyone, you included, can sign in. Onboard the domain of `MAIL_FROM` to Email Sending:

```bash
npx wrangler email sending enable example.com
npx wrangler email sending dns get example.com
```

The dashboard (**Email Service → Email Sending → Onboard Domain**) adds the SPF and DKIM records for
you; with the CLI, add what `dns get` lists. Add a DMARC record if the domain has none:

```
_dmarc.example.com  TXT  "v=DMARC1; p=quarantine; rua=mailto:dmarc@example.com"
```

Allow a few minutes for DNS. Replies you write from your own addresses go out the same way, so
onboard every domain you will send from.

## 5. Receiving: Email Routing to the `leanmail` Worker

```bash
npx wrangler email routing enable example.com     # adds Cloudflare's MX and SPF records
npx wrangler email routing rules update example.com catch-all \
  --action-type worker --action-value leanmail --enabled true
```

Or in the dashboard: **Email → Email Routing → Routing rules → Catch-all address → Send to a
Worker → leanmail**. The Worker's name is what the rule points at, which is why `wrangler.jsonc`
says not to rename it.

The Worker then decides per address, from the routes your account projects into D1 (exact
address, then `user+tag` without the tag, then the domain's default): keep, keep and forward,
forward only, or drop. A domain with no routes drops everything, so finish step 6 before relying
on it. Forward targets must be verified destination addresses in Email Routing.

With `CF_API_TOKEN` and `CF_ACCOUNT_ID` set, **Settings → Routing → Check Cloudflare / Apply** does
the catch-all and rule changes above for each domain it manages.

## 6. First run

1. **Sign up.** Open `https://mail.example.com/signup` and enter your address, or post it:

   ```bash
   curl -X POST https://mail.example.com/auth/signup \
     -H 'content-type: application/json' -d '{"email": "you@example.com"}'
   ```

   The API always answers `202` (the form shows a thanks page). With `SIGNUP_MODE=approve` (or `open`) and your address in
   `ADMIN_EMAILS`, the sign-up is approved at once and your account is an admin: the operator
   creates your account, starts your container, and mails a welcome link from `MAIL_FROM` (valid
   24 hours).
2. **Sign in.** Follow the link, or open `https://mail.example.com/signin`, enter your address, and
   use the link or the six-digit code it mails you (valid 15 minutes).
   Once in, **Settings → Passkeys → Add a passkey** lets you sign in with Touch ID, Face ID, Windows
   Hello or a security key next time. The passkey's relying party is the host of `PUBLIC_ORIGIN`,
   so a passkey made on one origin does not work on another, and an IP address (the local default
   `http://127.0.0.1:8787`) cannot hold one: use `http://localhost:8787` to try passkeys locally.
3. **Give yourself your domain.** **Admin → Accounts**: put `example.com` in your account's
   domains and save. The Worker accepts routes only for domains an admin granted to that account.
4. **Route it.** **Settings → Routing**: add `example.com`, choose what its addresses do by default
   (keep in LeanMail), add any per-address rules and the addresses you send from, and save. Each
   change is pushed to the Worker at once. Send yourself a message from another provider; it should
   be in the inbox within seconds of arriving.
5. **Others.** Each person signs up at `/signup` the same way; approve them at
   **Admin → Sign-ups**. An approved person gets their own container and database and a welcome
   link. Grant them domains in **Admin → Accounts**.

## 7. The CLI

```bash
make cli                                  # builds lean/.lake/build/bin/leanmail; put it on your PATH
```

In the web app, **Settings → API tokens**: create a token. Scopes: `read`, `act` (archive, snooze,
import), `draft`, `send`, `junk`; give the CLI what you will use. The token is shown once.

```bash
leanmail login https://mail.example.com --token lm1.…
leanmail recent
leanmail search 'from:alice invoice'
leanmail show <id>
leanmail import ~/Mail/archive.mbox       # .eml files, directories of them, or an mbox
leanmail mcp                              # a stdio MCP server for Claude Code and other AI tools
```

The config lives at `~/.config/leanmail/config.json`, mode 600. `leanmail help` lists every command.

## 8. Gmail (optional)

LeanMail reads, archives and sends through Gmail with your own Google OAuth client:

1. In Google Cloud, create a project and enable the **Gmail API**.
2. **APIs & Services → OAuth consent screen**: an External app. Add your Google address as a test
   user. Scopes used: `gmail.modify`, `gmail.settings.basic`, `gmail.settings.sharing`.
3. **Credentials → Create credentials → OAuth client ID**, type **Web application**, authorized
   redirect URI exactly `https://mail.example.com/oauth/google/callback`
   (Settings → Gmail shows the one your install uses).
4. In LeanMail, **Settings → Gmail**: paste the client id and secret, then **Connect Gmail**.

While the consent screen is in **Testing**, Google expires refresh tokens after 7 days: the page
then says the connection was refused, and you connect again. Publishing the app (**Publish app**;
for personal use you can skip verification and click through Google's unverified-app warning)
removes the weekly re-connect.

Push instead of polling (optional): create a Pub/Sub topic, grant
`gmail-api-push@system.gserviceaccount.com` the **Pub/Sub Publisher** role on it, and give it a
push subscription to the URL Settings → Gmail shows (`https://mail.example.com/hooks/gmail/<account>/<sig>`).
Enter the topic on that page (or set `GOOGLE_PUBSUB_TOPIC`) and click **Turn on push**. Without push,
LeanMail polls.

## 9. The iOS and Mac app (optional)

The app is in `app/`, written in Lean with [LeanNative](https://github.com/LeanNative/leannative).
On a Mac with Xcode:

```bash
make app            # the app's Lean, its proof gate and its selftest
make app-ios        # a Debug build for the iOS simulator
make app-catalyst   # the Mac Catalyst app (needs LEAN_X86_64_PREFIX, see the Makefile)
```

For a device, open `app/ios/LeanMailApp.xcodeproj`, set your team and bundle id, and run. Sign the
app in to `https://mail.example.com`. Push needs the `APNS_*` secrets with `APNS_TOPIC` set to your
bundle id.

## Updating

```bash
git pull
make toolchain-cache                       # a new lean-toolchain means a new tarball
cd worker && npm ci
npx wrangler d1 migrations apply leanmail-ctl --remote
npx wrangler deploy
```

Containers pick up the new image as they restart; each one migrates its own SQLite schema on boot.
`.github/workflows/deploy.yml` does the same on every push to `main` once the repository has the
`CLOUDFLARE_API_TOKEN` and `CLOUDFLARE_ACCOUNT_ID` secrets and a `PUBLIC_ORIGIN` variable (start the
token from Cloudflare's "Edit Cloudflare Workers" template and add what `wrangler deploy` reports
missing for D1, R2 and Containers). CI never sets the Worker's secrets; they stay as you put them.

## Backups

- **Raw mail** is the source of truth: `raw/<account>/<ulid>` in the `leanmail-mail` bucket, one
  object per message. Back the bucket up with any S3 tool using the R2 key from step 1.
- **Each account's database** is streamed by Litestream to `db/<account>/` in the same bucket, about
  once a second while the container is awake, with a daily snapshot kept for 7 days. A container
  starts on a fresh disk and restores from there, so nothing else is needed to survive a restart.

To restore a database by hand (to inspect it, or to keep a copy off Cloudflare), run Litestream from
the image with the same configuration the container uses:

```bash
docker run --rm -v "$PWD:/out" \
  -e ACCOUNT_ID=<account id> -e DATA_DIR=/data \
  -e R2_BUCKET=leanmail-mail -e R2_ENDPOINT=https://<ACCOUNT_ID>.r2.cloudflarestorage.com \
  -e LITESTREAM_ACCESS_KEY_ID=<key id> -e LITESTREAM_SECRET_ACCESS_KEY=<secret> \
  leanmail-server:dev litestream restore -config /etc/litestream.yml -o /out/mail.db /data/mail.db
sqlite3 mail.db 'select count(*) from messages'
```

(`make docker` builds `leanmail-server:dev`; the account id is in **Admin → Accounts**.) A database
lost entirely can be rebuilt from raw mail: copy `raw/<account>/` down, give the files a `.eml`
suffix, and `leanmail import` the directory into a fresh account.

## Troubleshooting

| Symptom | Cause and fix |
|---|---|
| `401` from `/internal/health` | The secret must be the account's derivation, `base64url(HMAC-SHA256(INTERNAL_SECRET, account))` with no padding, sent with `x-account-id: <account>`. The master secret itself is refused. |
| The first request after a while is slow, or times out once | A cold start: containers sleep after 5 minutes idle, and waking one restores its database from R2 before it answers. Mail is never lost to this; the Worker writes it to R2 first and the container drains the spool when it starts. A large archive restores more slowly; leanmail.io uses the `standard-1` instance type for that reason. |
| Sending fails with `worker_failed` (`leanmail send`, drafts, compose) | The Worker could not send it. Usually `MAIL_FROM`'s domain, or the From address's domain, is not onboarded to Email Sending (step 4), or the From address is not marked as one you send from in Settings → Routing (the Worker answers `from_not_allowed` when its route lacks `can_send`). `npx wrangler tail` shows the Worker's `internal.email_send_failed` line. |
| `429 too many attempts` on sign-in | 10 sign-in requests (and 10 code attempts) an hour per client address. Wait an hour, or clear it: `npx wrangler d1 execute leanmail-ctl --remote --command "DELETE FROM auth_rate"`. |
| No sign-in mail arrives | Step 4: check `wrangler email sending dns get`, and the recipient's spam folder. `npx wrangler tail` shows whether the Worker sent it. |
| Mail sent to your domain does not arrive | Check the catch-all points at `leanmail` (step 5), the domain is granted to your account and has routes (step 6), and `npx wrangler tail` for `email.decision` lines; `"mail":"drop"` with `"match":"none"` means no route matched. |
| `wrangler deploy` fails building the image | Docker must be running. Run `make toolchain-cache` first; a half-downloaded toolchain inside the build is the usual cause. |

## How leanmail.io differs

leanmail.io is this code with `SIGNUP_MODE=approve`: anyone may sign up, and the operator approves
each sign-up by hand at **Admin → Sign-ups** before an account and its container exist. It runs the
`standard-1` instance type instead of `basic`, and it is free. Otherwise it is the install this
guide describes, and the same caveats apply: the operator can read the mail on it (see
[SECURITY.md](../SECURITY.md)). Self-host if that matters to you.
