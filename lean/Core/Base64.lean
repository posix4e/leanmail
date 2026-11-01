namespace Core.Base64

/-! # Base64 (RFC 4648), standard and URL-safe, encode and decode

Decoding is lenient the way mail needs it: whitespace, line breaks and any other foreign
character are skipped, padding is optional, and `=` ends the data. -/

private def stdAlphabet : Array Char :=
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".toList.toArray
private def urlAlphabet : Array Char :=
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_".toList.toArray

private def encodeWith (alphabet : Array Char) (pad : Bool) (b : ByteArray) : String := Id.run do
  let ch (i : UInt32) : Char := alphabet.getD i.toNat 'A'
  let mut out := ""
  let mut i := 0
  while i + 2 < b.size do
    let n := (b[i]!.toUInt32 <<< 16) ||| (b[i+1]!.toUInt32 <<< 8) ||| b[i+2]!.toUInt32
    out := out.push (ch (n >>> 18)) |>.push (ch ((n >>> 12) &&& 63)) |>.push (ch ((n >>> 6) &&& 63)) |>.push (ch (n &&& 63))
    i := i + 3
  if i + 1 == b.size then
    let n := b[i]!.toUInt32 <<< 16
    out := out.push (ch (n >>> 18)) |>.push (ch ((n >>> 12) &&& 63))
    if pad then out := out ++ "=="
  else if i + 2 == b.size then
    let n := (b[i]!.toUInt32 <<< 16) ||| (b[i+1]!.toUInt32 <<< 8)
    out := out.push (ch (n >>> 18)) |>.push (ch ((n >>> 12) &&& 63)) |>.push (ch ((n >>> 6) &&& 63))
    if pad then out := out.push '='
  return out

/-- Standard alphabet with padding, as MIME uses it. -/
def encode (b : ByteArray) : String := encodeWith stdAlphabet true b
/-- URL-safe alphabet without padding, as tokens and WebAuthn use it. -/
def encodeUrl (b : ByteArray) : String := encodeWith urlAlphabet false b

/-- Both alphabets are accepted on decode; `-`/`_` and `+`/`/` mean the same thing. -/
private def value? (c : Char) : Option UInt32 :=
  if 'A' ≤ c && c ≤ 'Z' then some (c.toNat - 65).toUInt32
  else if 'a' ≤ c && c ≤ 'z' then some (c.toNat - 97 + 26).toUInt32
  else if '0' ≤ c && c ≤ '9' then some (c.toNat - 48 + 52).toUInt32
  else if c == '+' || c == '-' then some 62
  else if c == '/' || c == '_' then some 63
  else none

/-- Decode, skipping whitespace and foreign characters, stopping at `=`. -/
def decode (s : String) : ByteArray := Id.run do
  let mut out := ByteArray.empty
  let mut acc : UInt32 := 0
  let mut bits : Nat := 0
  for c in s.toList do
    if c == '=' then break
    if c.isWhitespace || c == '\r' || c == '\n' then continue
    match value? c with
    | none => continue
    | some v =>
      acc := (acc <<< 6) ||| v
      bits := bits + 6
      if bits ≥ 8 then
        bits := bits - 8
        out := out.push ((acc >>> bits.toUInt32).toUInt8)
        acc := acc &&& ((1 <<< bits.toUInt32) - 1)
  return out

/-- postal-mime's `decodeBase64`, over bytes already reduced to `[A-Za-z0-9+/=]`: trailing `=` are
dropped, a dangling single character is ignored, and any other `=` reads as the value 0. -/
def decodePostal (b : ByteArray) : ByteArray := Id.run do
  let mut len := b.size
  while len > 0 && b[len - 1]! == 61 do len := len - 1
  if len % 4 == 1 then len := len - 1
  let val (i : Nat) : UInt32 :=
    if i < len then (value? (Char.ofNat b[i]!.toNat)).getD 0 else 0
  let rem := len % 4
  let outLen := len / 4 * 3 + (if rem == 0 then 0 else rem - 1)
  let mut out := ByteArray.emptyWithCapacity outLen
  let mut i := 0
  while i < len do
    let e1 := val i
    let e2 := val (i + 1)
    let e3 := val (i + 2)
    let e4 := val (i + 3)
    out := out.push ((e1 <<< 2) ||| (e2 >>> 4)).toUInt8
    if out.size < outLen then out := out.push (((e2 &&& 15) <<< 4) ||| (e3 >>> 2)).toUInt8
    if out.size < outLen then out := out.push (((e3 &&& 3) <<< 6) ||| (e4 &&& 63)).toUInt8
    i := i + 4
  return out

end Core.Base64
