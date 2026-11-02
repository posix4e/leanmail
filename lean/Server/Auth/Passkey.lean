import Lean.Data.Json
import Core.Base64
import Core.Cbor
import Core.Ulid
import Server.Router.Types
import Server.Auth.Tokens
import Server.Auth.Session
import Server.Clock

/-! # Passkeys (WebAuthn, ES256 only)

**Registration** (a signed-in session): `POST /api/passkeys/options` stores 32 random bytes in
`auth_challenges` (kind `register`, five minutes) and answers the `PublicKeyCredentialCreationOptions`
with binary fields as base64url: `rp.id` is the host of `PUBLIC_ORIGIN`, `user.id` the account id's
UTF-8 bytes, ES256 (`alg -7`) only, `attestation: "none"`, a discoverable credential required.
`POST /api/passkeys {id, rawId, response:{clientDataJSON, attestationObject}, transports?, name?}`
checks `clientDataJSON` (`type` `webauthn.create`, `origin` = `PUBLIC_ORIGIN`, the challenge one of
ours, unexpired, then deleted), decodes the attestation object (`Core.Cbor`: `fmt` `none` with an
empty `attStmt`), and reads `authData`: the `rpIdHash` is `sha256(rp.id)`, the UP (bit 0) and AT
(bit 6) flags are set, the credential id equals `rawId` (at most 1023 bytes), and the COSE key is
EC2 (`kty 2`), ES256 (`alg -7`), P-256 (`crv 1`) with 32-byte `x` and `y`. The COSE bytes are kept
as sent and decoded again at each login.

**Login** (public, routed by the Worker). Before any account is known the Worker mints a stateless
challenge `v1.<rand>.<exp>.<mac>` (`worker/src/auth.ts`); the browser's challenge is that token's
UTF-8 bytes. On `POST /auth/passkey/login` the Worker reads the account from `userHandle`, checks
the token's MAC and expiry, and proxies to that account's container with
`x-passkey-challenge-ok: base64url(HMAC-SHA256(K_acct, "passkey:" + token))`, where `K_acct` is the
container's own `SESSION_SECRET` (the container never holds the master the token is MAC'd under, so
the Worker vouches for it under the key the container has; the `passkey:` prefix keeps the voucher
from ever equalling the MAC of a credential `<prefix>.<acct>.…`). Here: the header is checked against the token in
`clientDataJSON` (constant time), the token's `exp` is in the future, `type` is `webauthn.get`,
`origin` is `PUBLIC_ORIGIN`, `userHandle` names this account, the credential is ours, the
`rpIdHash` and UP flag hold, and the ECDSA P-256 signature over
`authenticatorData || sha256(clientDataJSON)` verifies (`Crypto.p256Verify`). The sign count must
grow unless the stored and the new count are both 0 (authenticators that do not count). A spent
login challenge's `sha256` is kept until its expiry, so an assertion cannot be replayed even when
the count stays 0. Then a session starts (`Auth.Session.create`) and the answer is `200 {ok:true}`
with the cookie. -/

namespace Server.Auth.Passkey

open Lean Server.Db Server.Router

def challengeTtlMs : Nat := 5 * 60 * 1000
def maxCredentialId : Nat := 1023
def headerName : String := "x-passkey-challenge-ok"

/-- The relying party id: `RP_ID` when it is the host of `PUBLIC_ORIGIN` or a parent domain of it
(what WebAuthn lets a page claim), else that host. -/
def rpId (c : Config) : String :=
  let host := Config.originHost c.publicOrigin
  let r := c.rpIdSetting
  if !r.isEmpty && r.contains '.' && (host == r || host.endsWith ("." ++ r)) then r else host

/-- Strict base64url: only the URL alphabet, no padding. -/
def b64u? (s : String) : Option ByteArray :=
  if s.length % 4 == 1 then none
  else if s.toList.all (fun ch => ch.isAlphanum || ch == '-' || ch == '_') then some (Core.Base64.decode s)
  else none

def b64u (b : ByteArray) : String := Core.Base64.encodeUrl b

/-! ## Authenticator data -/

structure Attested where
  aaguid : ByteArray
  credentialId : ByteArray
  /-- The COSE key exactly as it appears in the authenticator data. -/
  coseKey : ByteArray

structure AuthData where
  rpIdHash : ByteArray
  flags : UInt8
  signCount : Nat
  attested : Option Attested := none

def AuthData.up (a : AuthData) : Bool := a.flags &&& 0x01 != 0
def AuthData.uv (a : AuthData) : Bool := a.flags &&& 0x04 != 0
def AuthData.backupEligible (a : AuthData) : Bool := a.flags &&& 0x08 != 0
def AuthData.hasAttested (a : AuthData) : Bool := a.flags &&& 0x40 != 0
def AuthData.hasExtensions (a : AuthData) : Bool := a.flags &&& 0x80 != 0

private def be (b : ByteArray) (i n : Nat) : Nat :=
  (List.range n).foldl (fun acc k => acc * 256 + b[i + k]!.toNat) 0

/-- `rpIdHash(32) flags(1) signCount(4) [aaguid(16) credIdLen(2) credId COSEkey] [extensions]`. -/
def parseAuthData (b : ByteArray) : Except String AuthData := do
  if b.size < 37 then throw "authData too short"
  let flags := b[32]!
  let base : AuthData := { rpIdHash := b.extract 0 32, flags, signCount := be b 33 4 }
  let mut i := 37
  let mut out := base
  if base.hasAttested then
    if b.size < i + 18 then throw "attested data too short"
    let aaguid := b.extract i (i + 16)
    let len := be b (i + 16) 2
    i := i + 18
    if len == 0 || len > maxCredentialId || b.size < i + len then throw "bad credential id"
    let credentialId := b.extract i (i + len)
    i := i + len
    let (_, j) ← Core.Cbor.decodeAt b i
    out := { out with attested := some { aaguid, credentialId, coseKey := b.extract i j } }
    i := j
  if base.hasExtensions then
    let (ext, j) ← Core.Cbor.decodeAt b i
    if ext.map?.isNone then throw "extensions are not a map"
    i := j
  if i != b.size then throw "trailing bytes in authData"
  return out

/-! ## COSE keys -/

structure Es256Key where
  x : ByteArray
  y : ByteArray

/-- An EC2 P-256 ES256 key: `{1: 2, 3: -7, -1: 1, -2: x, -3: y}`. -/
def parseCose (b : ByteArray) : Except String Es256Key := do
  let k ← Core.Cbor.decode b
  if (k.atInt 1).bind (·.int?) != some 2 then throw "not an EC2 key"
  if (k.atInt 3).bind (·.int?) != some (-7) then throw "not ES256"
  if (k.atInt (-1)).bind (·.int?) != some 1 then throw "not P-256"
  let some x := (k.atInt (-2)).bind (·.bytes?) | throw "no x"
  let some y := (k.atInt (-3)).bind (·.bytes?) | throw "no y"
  if x.size != 32 || y.size != 32 then throw "bad point size"
  return { x, y }

/-! ## Client data -/

structure ClientData where
  type : String
  /-- The challenge's bytes (decoded from base64url). -/
  challenge : ByteArray
  origin : String

def parseClientData (raw : ByteArray) : Except String ClientData := do
  let some s := String.fromUTF8? raw | throw "clientDataJSON is not UTF-8"
  let j ← match Json.parse s with
    | .ok j@(.obj _) => pure j
    | _ => throw "clientDataJSON is not an object"
  let some type := jstr? j "type" | throw "no type"
  let some ch := (jstr? j "challenge").bind b64u? | throw "bad challenge"
  let some origin := jstr? j "origin" | throw "no origin"
  if jbool? j "crossOrigin" == some true then throw "cross-origin"
  return { type, challenge := ch, origin }

/-! ## Verification (pure) -/

structure Registration where
  client : ClientData
  auth : AuthData
  credentialId : ByteArray
  coseKey : ByteArray

/-- Everything about a registration but its challenge, which the caller spends. -/
def verifyRegistration (c : Config) (clientDataJSON attestationObject rawId : ByteArray) : Except String Registration := do
  let cd ← parseClientData clientDataJSON
  if cd.type != "webauthn.create" then throw "wrong type"
  if cd.origin != c.origin then throw "wrong origin"
  let att ← Core.Cbor.decode attestationObject
  if (att.atText "fmt").bind (·.text?) != some "none" then throw "attestation format is not none"
  match (att.atText "attStmt").bind (·.map?) with
  | some kvs => if !kvs.isEmpty then throw "attStmt is not empty"
  | none => throw "no attStmt"
  let some adRaw := (att.atText "authData").bind (·.bytes?) | throw "no authData"
  let ad ← parseAuthData adRaw
  if !Crypto.constantTimeEq ad.rpIdHash (Crypto.sha256Text (rpId c)) then throw "wrong rp id"
  if !ad.up then throw "user not present"
  let some a := ad.attested | throw "no attested credential"
  if a.credentialId != rawId then throw "credential id differs from rawId"
  discard <| parseCose a.coseKey
  return { client := cd, auth := ad, credentialId := a.credentialId, coseKey := a.coseKey }

/-- An assertion's client data, authenticator data and signature against a stored key. -/
def verifyAssertion (c : Config) (clientDataJSON authenticatorData signature : ByteArray) (key : Es256Key) :
    Except String (ClientData × AuthData) := do
  let cd ← parseClientData clientDataJSON
  if cd.type != "webauthn.get" then throw "wrong type"
  if cd.origin != c.origin then throw "wrong origin"
  let ad ← parseAuthData authenticatorData
  if !Crypto.constantTimeEq ad.rpIdHash (Crypto.sha256Text (rpId c)) then throw "wrong rp id"
  if !ad.up then throw "user not present"
  if !Crypto.p256Verify key.x key.y (authenticatorData ++ Crypto.sha256 clientDataJSON) signature then
    throw "bad signature"
  return (cd, ad)

/-- The sign-count rule: both 0 (an authenticator that does not count), or strictly growing. -/
def countOk (stored new : Nat) : Bool := (stored == 0 && new == 0) || new > stored

/-- The `exp` of a Worker login challenge `v1.<rand>.<exp>.<mac>`. -/
def loginTokenExp? (tok : String) : Option Nat :=
  match tok.splitOn "." with
  | ["v1", r, e, m] => if r.isEmpty || m.isEmpty then none else e.toNat?
  | _ => none

/-- What the Worker puts in `x-passkey-challenge-ok` for a token, under this container's secret. -/
def vouch (c : Config) (tok : String) : String := Tokens.mac c.sessionSecret ("passkey:" ++ tok)

/-! ## Routes -/

private def obj? (j : Json) (k : String) : Json := (j.getObjValAs? Json k).toOption.getD .null

private def prune (st : State) (now : Nat) : IO Unit :=
  st.db.exec "DELETE FROM auth_challenges WHERE expires_at <= ?" #[now]

def newId : IO String := do return Core.Ulid.encode (← Clock.nowMs) (← Crypto.randomBytes 10)

/-- `POST /api/passkeys/options`. -/
def options (c : Ctx) (_ : Req) : IO Resp := do
  let st := c.st
  let now ← Clock.nowMs
  prune st now
  let ch ← Crypto.randomBytes 32
  st.db.exec "INSERT INTO auth_challenges(challenge, kind, expires_at) VALUES (?, 'register', ?)" #[ch, now + challengeTtlMs]
  let acct ← st.db.queryOne "SELECT email, display_name FROM account WHERE id = 1"
  let email := (acct.map (·.text "email")).getD ""
  let shown := (acct.map (·.text "display_name")).getD ""
  let creds ← st.db.query "SELECT credential_id FROM passkeys ORDER BY created_at"
  return Resp.ok <| Json.mkObj [
    ("challenge", Json.str (b64u ch)),
    ("rp", Json.mkObj [("id", Json.str (rpId st.config)), ("name", Json.str "LeanMail")]),
    ("user", Json.mkObj [("id", Json.str (b64u st.config.accountId.toUTF8)), ("name", Json.str email),
                         ("displayName", Json.str (if shown.isEmpty then email else shown))]),
    ("pubKeyCredParams", Json.arr #[Json.mkObj [("type", Json.str "public-key"), ("alg", toJson (-7 : Int))]]),
    ("timeout", Json.num challengeTtlMs),
    ("attestation", Json.str "none"),
    ("authenticatorSelection", Json.mkObj [("residentKey", Json.str "required"), ("requireResidentKey", Json.bool true),
                                           ("userVerification", Json.str "preferred")]),
    ("excludeCredentials", Json.arr <| creds.map fun r =>
      Json.mkObj [("type", Json.str "public-key"), ("id", Json.str (b64u (r.blob "credential_id")))])]

def knownTransports : List String := ["usb", "nfc", "ble", "internal", "hybrid", "smart-card"]

private def bad (msg : String) : IO Resp := do
  Log.info "auth.passkey_register" [("ok", Json.bool false), ("reason", Json.str msg)]
  return Resp.error 400 msg

/-- `POST /api/passkeys`. -/
def register (c : Ctx) (req : Req) : IO Resp := do
  let st := c.st
  let j := req.json
  let r := obj? j "response"
  let some rawId := (jstr? j "rawId").bind b64u? | bad "bad rawId"
  let some cdj := (jstr? r "clientDataJSON").bind b64u? | bad "bad clientDataJSON"
  let some att := (jstr? r "attestationObject").bind b64u? | bad "bad attestationObject"
  let reg ← match verifyRegistration st.config cdj att rawId with
    | .ok v => pure v
    | .error e => return ← bad e
  let now ← Clock.nowMs
  let spent ← st.db.execCount "DELETE FROM auth_challenges WHERE challenge = ? AND kind = 'register' AND expires_at > ?"
    #[reg.client.challenge, now]
  if spent != 1 then return ← bad "unknown or expired challenge"
  let transports := ((j.getObjValAs? (Array String) "transports").toOption.getD #[]).filter knownTransports.contains
  let name := String.ofList ((Core.Text.Str.trim ((jstr? j "name").getD "")).toList.take 100)
  let name := if name.isEmpty then "Passkey" else name
  if (← st.db.queryOne "SELECT id FROM passkeys WHERE credential_id = ?" #[reg.credentialId]).isSome then
    return Resp.error 409 "passkey already registered"
  let id ← newId
  st.db.exec "INSERT INTO passkeys(id, credential_id, public_key_cose, alg, sign_count, transports, name, backup_eligible, created_at) VALUES (?, ?, ?, -7, ?, ?, ?, ?, ?)"
    #[id, reg.credentialId, reg.coseKey, reg.auth.signCount, (Json.arr (transports.toList.eraseDups.toArray.map Json.str)).compress,
      name, reg.auth.backupEligible, now]
  Log.info "auth.passkey_register" [("ok", Json.bool true)]
  return Resp.json 201 (Json.mkObj [("id", Json.str id), ("name", Json.str name), ("created_at", Json.num now)])

def rowJson (r : Row) : Json := Json.mkObj [
  ("id", Json.str (r.text "id")), ("name", Json.str (r.text "name")),
  ("created_at", Json.num (r.nat "created_at")),
  ("last_used_at", match r.nat? "last_used_at" with | some n => Json.num n | none => Json.null),
  ("transports", (Json.parse (r.text "transports")).toOption.getD (Json.arr #[])),
  ("backup_eligible", Json.bool (r.bool "backup_eligible"))]

def rows (st : State) : IO (Array Row) :=
  st.db.query "SELECT id, name, created_at, last_used_at, transports, backup_eligible FROM passkeys ORDER BY created_at DESC"

/-- `GET /api/passkeys`. -/
def list (c : Ctx) (_ : Req) : IO Resp := do
  return Resp.ok (Json.mkObj [("items", Json.arr ((← rows c.st).map rowJson))])

/-- Delete one passkey; false when there is none by that id. -/
def removeId (st : State) (id : String) : IO Bool := do
  return (← st.db.execCount "DELETE FROM passkeys WHERE id = ?" #[id]) == 1

/-- `DELETE /api/passkeys/:id`. -/
def remove (c : Ctx) (_ : Req) : IO Resp := do
  if ← removeId c.st (c.param "id") then return Resp.okStatus
  return Resp.notFound

private def refuse (reason : String) : IO Resp := do
  Log.info "auth.passkey_login" [("ok", Json.bool false), ("reason", Json.str reason)]
  return Resp.error 401 "passkey not accepted"

/-- `POST /auth/passkey/login` (public; the Worker vouches for the challenge, see the module doc). -/
def login (c : Ctx) (req : Req) : IO Resp := do
  let st := c.st
  let cfg := st.config
  let j := req.json
  let r := obj? j "response"
  let some rawId := (jstr? j "rawId").bind b64u? | refuse "bad rawId"
  let some cdj := (jstr? r "clientDataJSON").bind b64u? | refuse "bad clientDataJSON"
  let some ad := (jstr? r "authenticatorData").bind b64u? | refuse "bad authenticatorData"
  let some sig := (jstr? r "signature").bind b64u? | refuse "bad signature encoding"
  if let some uh := jstr? r "userHandle" then
    if !uh.isEmpty && b64u? uh != some cfg.accountId.toUTF8 then return ← refuse "another account's user handle"
  let some given := req.header? headerName | refuse "no challenge voucher"
  let cd ← match parseClientData cdj with
    | .ok v => pure v
    | .error e => return ← refuse e
  let some tok := String.fromUTF8? cd.challenge | refuse "challenge is not text"
  if !Crypto.constantTimeEq given.toUTF8 (vouch cfg tok).toUTF8 then return ← refuse "voucher does not match"
  let now ← Clock.nowMs
  let some exp := loginTokenExp? tok | refuse "bad challenge"
  if exp ≤ now then return ← refuse "challenge expired"
  let some row ← st.db.queryOne "SELECT id, public_key_cose, sign_count FROM passkeys WHERE credential_id = ?" #[rawId]
    | refuse "unknown credential"
  let key ← match parseCose (row.blob "public_key_cose") with
    | .ok k => pure k
    | .error e => return ← refuse e
  let (_, auth) ← match verifyAssertion cfg cdj ad sig key with
    | .ok v => pure v
    | .error e => return ← refuse e
  let stored := row.nat "sign_count"
  if !countOk stored auth.signCount then return ← refuse "sign count did not grow"
  let fresh ← st.db.transaction do
    st.db.execUnlocked "DELETE FROM auth_challenges WHERE expires_at <= ?" #[now]
    st.db.execUnlocked "INSERT OR IGNORE INTO auth_challenges(challenge, kind, expires_at) VALUES (?, 'login', ?)"
      #[Crypto.sha256Text tok, exp]
    if (← st.db.changes) != 1 then return false
    st.db.execUnlocked "UPDATE passkeys SET sign_count = ?, last_used_at = ? WHERE id = ? AND sign_count = ?"
      #[auth.signCount, now, row.text "id", stored]
    return (← st.db.changes) == 1
  if !fresh then return ← refuse "challenge already used"
  let some cookie ← Session.create st ((req.header? "user-agent").getD "") | refuse "no account"
  Log.info "auth.passkey_login" [("ok", Json.bool true)]
  return (Resp.okStatus).withHeader "Set-Cookie" (Session.cookieHeader cfg cookie)

def routes : Array Route := #[
  route "POST" "/api/passkeys/options" .session options,
  route "POST" "/api/passkeys" .session register,
  route "GET" "/api/passkeys" .session list,
  route "DELETE" "/api/passkeys/:id" .session remove,
  route "POST" "/auth/passkey/login" .«public» login
]

end Server.Auth.Passkey
