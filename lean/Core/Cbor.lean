/-! # CBOR (RFC 8949), the subset WebAuthn needs

Enough to read a WebAuthn attestation object and a COSE public key: unsigned and negative
integers, byte and text strings, arrays, maps, and the simple values `false`, `true` and `null`.
Definite lengths only; tags, floats and indefinite-length items are refused, as is nesting deeper
than 16 or a length past the end of the input. `decode` reads one item at an offset and returns
where it ended, because a COSE key sits inside `authenticatorData` with extensions after it.

`encode` writes the same subset with the shortest heads, for building test records. -/

namespace Core.Cbor

inductive Value where
  | uint (n : Nat)
  /-- The integer `-1 - n`. -/
  | nint (n : Nat)
  | bytes (b : ByteArray)
  | text (s : String)
  | array (xs : Array Value)
  | map (kvs : Array (Value × Value))
  | bool (b : Bool)
  | null
  deriving Inhabited

namespace Value

def int? : Value → Option Int
  | .uint n => some n
  | .nint n => some (-1 - (n : Int))
  | _ => none

def bytes? : Value → Option ByteArray
  | .bytes b => some b
  | _ => none

def text? : Value → Option String
  | .text s => some s
  | _ => none

def map? : Value → Option (Array (Value × Value))
  | .map kvs => some kvs
  | _ => none

def array? : Value → Option (Array Value)
  | .array xs => some xs
  | _ => none

/-- Integer and text keys compare by value; other keys never match. -/
def keyEq : Value → Value → Bool
  | .uint a, .uint b => a == b
  | .nint a, .nint b => a == b
  | .text a, .text b => a == b
  | _, _ => false

/-- A map's value at `k`, the first if a key repeats. -/
def lookup (v : Value) (k : Value) : Option Value := do
  let kvs ← v.map?
  (kvs.find? (keyEq ·.1 k)).map (·.2)

/-- A map's value at an integer key (COSE labels). -/
def atInt (v : Value) (k : Int) : Option Value :=
  v.lookup (if k ≥ 0 then .uint k.toNat else .nint (-1 - k).toNat)

/-- A map's value at a text key. -/
def atText (v : Value) (k : String) : Option Value := v.lookup (.text k)

end Value

def maxDepth : Nat := 16

/-- The argument of a head byte at `i` (`ai` its low five bits): the value and the offset after it. -/
private def arg (b : ByteArray) (i : Nat) (ai : Nat) : Except String (Nat × Nat) :=
  let be (n : Nat) : Except String (Nat × Nat) :=
    if i + 1 + n > b.size then .error "cbor: truncated head" else
    .ok ((List.range n).foldl (fun acc k => acc * 256 + b[i + 1 + k]!.toNat) 0, i + 1 + n)
  if ai < 24 then .ok (ai, i + 1)
  else if ai == 24 then be 1
  else if ai == 25 then be 2
  else if ai == 26 then be 4
  else if ai == 27 then be 8
  else .error "cbor: indefinite or reserved length"

/-- One item starting at `i`: the value and the offset just after it. -/
partial def decodeAt (b : ByteArray) (i : Nat) (depth : Nat := 0) : Except String (Value × Nat) := do
  if depth > maxDepth then throw "cbor: too deep"
  if i ≥ b.size then throw "cbor: truncated"
  let ib := b[i]!.toNat
  let major := ib / 32
  let ai := ib % 32
  if major == 7 then
    match ai with
    | 20 => return (.bool false, i + 1)
    | 21 => return (.bool true, i + 1)
    | 22 => return (.null, i + 1)
    | _ => throw "cbor: unsupported simple value or float"
  let (n, j) ← arg b i ai
  match major with
  | 0 => return (.uint n, j)
  | 1 => return (.nint n, j)
  | 2 =>
    if j + n > b.size then throw "cbor: byte string past the end"
    return (.bytes (b.extract j (j + n)), j + n)
  | 3 =>
    if j + n > b.size then throw "cbor: text past the end"
    match String.fromUTF8? (b.extract j (j + n)) with
    | some s => return (.text s, j + n)
    | none => throw "cbor: text is not UTF-8"
  | 4 =>
    -- every item takes at least one byte, so a count past the remaining bytes is a lie
    if n > b.size - j then throw "cbor: array past the end"
    let mut xs := #[]
    let mut k := j
    for _ in [:n] do
      let (v, k') ← decodeAt b k (depth + 1)
      xs := xs.push v
      k := k'
    return (.array xs, k)
  | 5 =>
    if 2 * n > b.size - j then throw "cbor: map past the end"
    let mut kvs := #[]
    let mut k := j
    for _ in [:n] do
      let (key, k1) ← decodeAt b k (depth + 1)
      let (v, k2) ← decodeAt b k1 (depth + 1)
      kvs := kvs.push (key, v)
      k := k2
    return (.map kvs, k)
  | _ => throw "cbor: tags are not supported"

/-- Exactly one item filling the whole input. -/
def decode (b : ByteArray) : Except String Value := do
  let (v, j) ← decodeAt b 0
  if j != b.size then throw "cbor: trailing bytes"
  return v

/-! ## Encoding -/

private def head (major : Nat) (n : Nat) : ByteArray :=
  let m := major * 32
  let bytesBE (k : Nat) : ByteArray :=
    ⟨((List.range k).reverse.map fun s => (n / 256 ^ s % 256).toUInt8).toArray⟩
  if n < 24 then ⟨#[(m + n).toUInt8]⟩
  else if n < 256 then ⟨#[(m + 24).toUInt8]⟩ ++ bytesBE 1
  else if n < 65536 then ⟨#[(m + 25).toUInt8]⟩ ++ bytesBE 2
  else if n < 4294967296 then ⟨#[(m + 26).toUInt8]⟩ ++ bytesBE 4
  else ⟨#[(m + 27).toUInt8]⟩ ++ bytesBE 8

partial def encode : Value → ByteArray
  | .uint n => head 0 n
  | .nint n => head 1 n
  | .bytes b => head 2 b.size ++ b
  | .text s => head 3 s.utf8ByteSize ++ s.toUTF8
  | .array xs => xs.foldl (fun acc x => acc ++ encode x) (head 4 xs.size)
  | .map kvs => kvs.foldl (fun acc (k, v) => acc ++ encode k ++ encode v) (head 5 kvs.size)
  | .bool false => ⟨#[0xf4]⟩
  | .bool true => ⟨#[0xf5]⟩
  | .null => ⟨#[0xf6]⟩

/-- An integer value, negative or not. -/
def int (i : Int) : Value := if i ≥ 0 then .uint i.toNat else .nint (-1 - i).toNat

end Core.Cbor
