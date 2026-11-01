namespace Server.Crypto

/-! # Digests, randomness and P-256 verification over libcrypto (`ffi/c/lm_shim.c`). -/

@[extern "lm_sha256"] opaque sha256 : @& ByteArray → ByteArray
@[extern "lm_hmac_sha256"] opaque hmacSha256 : @& ByteArray → @& ByteArray → ByteArray
@[extern "lm_random_bytes"] opaque randomBytes : USize → IO ByteArray
/-- ECDSA P-256 / SHA-256: `p256Verify x y msg derSig` checks a DER signature over `msg` (hashed
here) under the public point `(x, y)`, 32 bytes each. False on anything malformed. WebAuthn's
ES256 (`Server.Auth.Passkey`). -/
@[extern "lm_p256_verify"] opaque p256Verify : @& ByteArray → @& ByteArray → @& ByteArray → @& ByteArray → Bool

private def hexDigit (n : UInt8) : Char :=
  if n < 10 then Char.ofNat (48 + n.toNat) else Char.ofNat (87 + n.toNat)

def toHex (b : ByteArray) : String :=
  b.foldl (init := "") fun acc x => acc.push (hexDigit (x >>> 4)) |>.push (hexDigit (x &&& 0x0f))

def sha256Hex (b : ByteArray) : String := toHex (sha256 b)
def sha256Text (s : String) : ByteArray := sha256 s.toUTF8
def hmacText (key : String) (msg : String) : ByteArray := hmacSha256 key.toUTF8 msg.toUTF8

/-- URL-safe base64 without padding, as tokens and WebAuthn use it. -/
def base64Url (b : ByteArray) : String := Id.run do
  let alphabet := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  let ch (i : Nat) : Char := alphabet.toList.getD i 'A'
  let mut out := ""
  let mut i := 0
  while i + 2 < b.size do
    let n := (b[i]!.toNat <<< 16) ||| (b[i+1]!.toNat <<< 8) ||| b[i+2]!.toNat
    out := out.push (ch (n >>> 18)) |>.push (ch ((n >>> 12) &&& 63)) |>.push (ch ((n >>> 6) &&& 63)) |>.push (ch (n &&& 63))
    i := i + 3
  if i + 1 == b.size then
    let n := b[i]!.toNat <<< 16
    out := out.push (ch (n >>> 18)) |>.push (ch ((n >>> 12) &&& 63))
  else if i + 2 == b.size then
    let n := (b[i]!.toNat <<< 16) ||| (b[i+1]!.toNat <<< 8)
    out := out.push (ch (n >>> 18)) |>.push (ch ((n >>> 12) &&& 63)) |>.push (ch ((n >>> 6) &&& 63))
  return out

/-- `n` random bytes as URL-safe base64: the shape of every secret we mint. -/
def randomToken (n : USize := 32) : IO String := do
  return base64Url (← randomBytes n)

/-- Constant-time equality for secrets. -/
def constantTimeEq (a b : ByteArray) : Bool := Id.run do
  if a.size != b.size then return false
  let mut acc : UInt8 := 0
  for i in [:a.size] do
    acc := acc ||| (a[i]! ^^^ b[i]!)
  return acc == 0

end Server.Crypto
