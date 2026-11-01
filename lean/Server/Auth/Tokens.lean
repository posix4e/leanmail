import Server.Config
import Server.Crypto

/-! # Token formats

Every credential the Worker routes on is `<prefix>.<acct>.<payload…>.<mac>` with
`mac = base64url(HMAC-SHA256(SESSION_SECRET, everything before the last dot))`, unpadded; the
Worker checks the MAC to pick the container (`worker/src/tokens.ts`, `verifyMac`), the container
re-checks it here, then the account, the expiry, and revocation in its own database.

| credential          | form                                   |
|---------------------|----------------------------------------|
| session cookie      | `v1.<acct>.<sid>.<exp>.<epoch>.<mac>`  |
| magic link          | `v1.<acct>.<rand>.<exp>.<mac>`         |
| API token           | `lm1.<acct>.<rand>.<mac>`              |

`exp` is epoch milliseconds; `epoch` is `account.session_epoch` at minting. A session and a magic
link share the `v1` prefix and differ in payload length, so neither verifies as the other. The
database stores `sha256` of the whole token, never the token. -/

namespace Server.Auth.Tokens

def mac (secret signed : String) : String := Crypto.base64Url (Crypto.hmacText secret signed)

/-- `<pfx>.<acct>.<payload…>.<mac>`. -/
def mint (secret pfx acct : String) (payload : List String) : String :=
  let signed := ".".intercalate (pfx :: acct :: payload)
  s!"{signed}.{mac secret signed}"

structure Verified where
  pfx : String
  acct : String
  payload : List String
  deriving Repr, BEq

/-- The MAC, checked in constant time; nothing else. -/
def verifyMac (secret token : String) : Option Verified :=
  if secret.isEmpty || token.length > 4096 then none else
  let parts := token.splitOn "."
  if parts.length < 4 then none else
  let given := parts.getLastD ""
  let signed := ".".intercalate parts.dropLast
  if !Crypto.constantTimeEq given.toUTF8 (mac secret signed).toUTF8 then none else
  match parts with
  | p :: a :: rest => some { pfx := p, acct := a, payload := rest.dropLast }
  | _ => none

/-- MAC, prefix and account. -/
def verifyFor (c : Config) (pfx : String) (token : String) : Option (List String) := do
  let v ← verifyMac c.sessionSecret token
  if v.pfx == pfx && v.acct == c.accountId then some v.payload else none

/-- What the database keeps of a token. -/
def hash (token : String) : ByteArray := Crypto.sha256Text token

structure SessionClaims where
  sid : String
  exp : Nat
  epoch : Nat
  deriving Repr, BEq

def mintSession (c : Config) (sid : String) (exp epoch : Nat) : String :=
  mint c.sessionSecret "v1" c.accountId [sid, toString exp, toString epoch]

def verifySession (c : Config) (now : Nat) (token : String) : Option SessionClaims := do
  match ← verifyFor c "v1" token with
  | [sid, exp, epoch] =>
    let e ← exp.toNat?
    let ep ← epoch.toNat?
    if now < e && !sid.isEmpty then some { sid, exp := e, epoch := ep } else none
  | _ => none

def mintMagic (c : Config) (rand : String) (exp : Nat) : String :=
  mint c.sessionSecret "v1" c.accountId [rand, toString exp]

/-- The link's random part and expiry. -/
def verifyMagic (c : Config) (now : Nat) (token : String) : Option (String × Nat) := do
  match ← verifyFor c "v1" token with
  | [rand, exp] =>
    let e ← exp.toNat?
    if now < e && !rand.isEmpty then some (rand, e) else none
  | _ => none

def mintApi (c : Config) (rand : String) : String :=
  mint c.sessionSecret "lm1" c.accountId [rand]

/-- API tokens do not expire; they are revoked. -/
def verifyApi (c : Config) (token : String) : Option String := do
  match ← verifyFor c "lm1" token with
  | [rand] => if rand.isEmpty then none else some rand
  | _ => none

end Server.Auth.Tokens
