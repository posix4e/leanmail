namespace Core.Ulid

/-! # ULID

26 characters of Crockford base32: 48 bits of milliseconds since the epoch, then 80 random bits.
Message ids, draft ids and spool keys are ULIDs, so they sort by time as strings. The random bytes
come from the caller so this module stays pure. -/

def alphabet : String := "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

private def digit (n : Nat) : Char := alphabet.toList.getD (n % 32) '0'

/-- Encode `ms` (48 bits used) and 10 random bytes. -/
def encode (ms : Nat) (random : ByteArray) : String := Id.run do
  let mut out := ""
  -- 10 time characters, most significant first
  for i in [0:10] do
    out := out.push (digit ((ms >>> (5 * (9 - i))) &&& 31))
  -- 80 random bits as 16 characters
  let mut acc : Nat := 0
  for i in [0:10] do
    acc := (acc <<< 8) ||| (random[i]?.getD 0).toNat
  for i in [0:16] do
    out := out.push (digit ((acc >>> (5 * (15 - i))) &&& 31))
  return out

private def value? (c : Char) : Option Nat :=
  match c.toUpper with
  | '0' | 'O' => some 0 | '1' | 'I' | 'L' => some 1 | '2' => some 2 | '3' => some 3 | '4' => some 4
  | '5' => some 5 | '6' => some 6 | '7' => some 7 | '8' => some 8 | '9' => some 9
  | 'A' => some 10 | 'B' => some 11 | 'C' => some 12 | 'D' => some 13 | 'E' => some 14 | 'F' => some 15
  | 'G' => some 16 | 'H' => some 17 | 'J' => some 18 | 'K' => some 19 | 'M' => some 20 | 'N' => some 21
  | 'P' => some 22 | 'Q' => some 23 | 'R' => some 24 | 'S' => some 25 | 'T' => some 26 | 'V' => some 27
  | 'W' => some 28 | 'X' => some 29 | 'Y' => some 30 | 'Z' => some 31
  | _ => none

/-- A well-formed ULID: 26 Crockford characters whose first is at most `7`. -/
def isValid (s : String) : Bool :=
  s.length == 26 && s.toList.all (value? · |>.isSome) && ((s.toList.head?.bind value?).getD 32) ≤ 7

/-- The timestamp a ULID carries, in milliseconds. -/
def timestamp? (s : String) : Option Nat := do
  guard (isValid s)
  let cs := s.toList.take 10
  cs.foldlM (init := 0) fun acc c => do return (acc <<< 5) ||| (← value? c)

/-- Canonical form: upper case, with the confusable letters mapped back to digits. -/
def normalize (s : String) : String :=
  String.ofList <| s.toList.map fun c => (value? c).map digit |>.getD c

end Core.Ulid
