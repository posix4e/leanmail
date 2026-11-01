# Security

## What LeanMail protects, and what it does not

- Mail is stored in plaintext on Cloudflare (R2 for raw messages, SQLite replicated to R2 for the
  index). It is encrypted at rest by Cloudflare and in transit by TLS. The operator of an instance
  can read everything on it. There is no end-to-end encryption by design.
- Each account runs in its own container with its own database. A bug in one account's process does
  not expose another account's mail.
- Sign-in is by magic link to the account's email address, then passkeys. API tokens for the CLI and
  the apps are shown once and stored hashed. Sessions are revocable; a global epoch logs out every
  device at once.
- The Worker is the only public surface. Containers are reachable only through their Durable Object,
  which stamps the account id and a shared secret on every request.
- The optional AI feature sends message text to whatever endpoint the operator configured. It is off
  unless three settings are present, and messages matching the privileged-sender rules are never sent.

## Reporting

Email security@leanmail.io, or open a private security advisory on GitHub. Please include the
instance (hosted or self-hosted), the version from `/health`, and steps to reproduce. You will get
a reply within a week.

## Self-hosters

Rotate `SESSION_SECRET` and `INTERNAL_SECRET` if they leak; rotating `SESSION_SECRET` signs everyone
out. Keep the Worker's R2 credentials scoped to the one bucket. Do not expose port 8080 of the
container image anywhere but behind the Worker.
