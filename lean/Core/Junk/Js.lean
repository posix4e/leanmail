import Core.Text.Str

/-! # JavaScript string behaviour the junk check depends on

`shared/junkcheck.js` is written with ASCII regular expressions and plain loops, but it still leans
on a few JavaScript string semantics a port must reproduce: `toLowerCase` (Unicode, and able to
change a string's length: `İ` becomes `i` + U+0307), `trim` and `\s` (Unicode whitespace), UTF-16
`length` and `slice`, and `String.fromCharCode` (which wraps at 65536). This module gives those, over
`Array Char` where the rules scan. -/

namespace Core.Junk.Js

open Core.Text.Str (isJsWhitespace isDigit isAlpha)

/-- JavaScript's `toLowerCase` for one character. ASCII, Latin-1, Latin Extended-A, Greek, Cyrillic,
fullwidth Latin and the three letterlike symbols that lowercase into those blocks (`K` Kelvin, `Å`
Angstrom, `Ω` Ohm) are covered; anything else is kept. Quirk not reproduced: a final capital sigma
becomes `σ`, not the `ς` JavaScript's context rule gives. -/
def lowerChar (c : Char) : List Char :=
  let n := c.toNat
  let ch (m : Nat) : List Char := [Char.ofNat m]
  if 0x41 ≤ n && n ≤ 0x5A then ch (n + 32)
  else if n < 0xC0 then [c]
  else if n ≤ 0xDE then (if n == 0xD7 then [c] else ch (n + 32))
  else if n == 0x130 then ['i', Char.ofNat 0x307]
  else if 0x100 ≤ n && n ≤ 0x17F then
    if n == 0x178 then ch 0xFF
    else if (0x100 ≤ n && n ≤ 0x12F) || (0x132 ≤ n && n ≤ 0x137) || (0x14A ≤ n && n ≤ 0x177) then
      (if n % 2 == 0 then ch (n + 1) else [c])
    else if (0x139 ≤ n && n ≤ 0x148) || (0x179 ≤ n && n ≤ 0x17E) then
      (if n % 2 == 1 then ch (n + 1) else [c])
    else [c]
  else if n == 0x386 then ch 0x3AC
  else if 0x388 ≤ n && n ≤ 0x38A then ch (n + 37)
  else if n == 0x38C then ch 0x3CC
  else if n == 0x38E || n == 0x38F then ch (n + 63)
  else if (0x391 ≤ n && n ≤ 0x3A1) || (0x3A3 ≤ n && n ≤ 0x3AB) then ch (n + 32)
  else if 0x400 ≤ n && n ≤ 0x40F then ch (n + 80)
  else if 0x410 ≤ n && n ≤ 0x42F then ch (n + 32)
  else if n == 0x212A then ['k']
  else if n == 0x212B then ch 0xE5
  else if n == 0x2126 then ch 0x3C9
  else if 0xFF21 ≤ n && n ≤ 0xFF3A then ch (n + 32)
  else [c]

/-- JavaScript's `toLowerCase`. -/
def lower (s : String) : String := String.ofList (s.toList.flatMap lowerChar)

/-- JavaScript's `length`: UTF-16 code units. -/
def utf16Len (s : String) : Nat := s.toList.foldl (fun n c => n + (if c.toNat ≥ 0x10000 then 2 else 1)) 0

/-- JavaScript's `slice(0, n)`, by UTF-16 units; a character that would be cut in half is left out
(JavaScript would keep its lone high surrogate, which a Lean string cannot hold). -/
def takeUtf16 (s : String) (n : Nat) : String := Id.run do
  let mut out : Array Char := #[]
  let mut used := 0
  for c in s.toList do
    let w := if c.toNat ≥ 0x10000 then 2 else 1
    if used + w > n then break
    used := used + w
    out := out.push c
  return String.ofList out.toList

/-- JavaScript's `trim`. -/
def trim (s : String) : String := Core.Text.Str.trimJs s

/-- `[a-z0-9]`, no `i` flag. -/
def isLowerAlnum (c : Char) : Bool := ('a' ≤ c && c ≤ 'z') || isDigit c

/-- `\w`: what `\b` looks at. -/
def isWordC (c : Char) : Bool := isAlpha c || isDigit c || c == '_'

def asciiLower (c : Char) : Char := if 'A' ≤ c && c ≤ 'Z' then Char.ofNat (c.toNat + 32) else c

/-- Does `pat` stand at `i` in `a`, ASCII case folded when `ci`. -/
def at? (a : Array Char) (i : Nat) (pat : Array Char) (ci : Bool := false) : Bool := Id.run do
  if i + pat.size > a.size then return false
  for j in [0 : pat.size] do
    let x := a[i + j]!
    let y := pat[j]!
    if ci then
      if asciiLower x != asciiLower y then return false
    else if x != y then return false
  return true

/-- `hay.includes(needle)`. -/
def contains (hay needle : Array Char) : Bool :=
  (Core.Text.Str.indexOf hay needle).isSome

def includes (hay needle : String) : Bool := contains hay.toList.toArray needle.toList.toArray

/-- `s.replace(/pat/g, rep)` (with `gi` when `ci`): left to right, never overlapping. -/
def replaceAll (s pat rep : String) (ci : Bool := false) : String := Id.run do
  let a := s.toList.toArray
  let p := pat.toList.toArray
  if p.isEmpty then return s
  let r := rep.toList.toArray
  let mut out : Array Char := #[]
  let mut i := 0
  while i < a.size do
    if at? a i p ci then
      out := out ++ r
      i := i + p.size
    else
      out := out.push a[i]!
      i := i + 1
  return String.ofList out.toList

/-- `String.fromCharCode(n)`: `n` wraps at 65536. A lone surrogate becomes U+FFFD (JavaScript keeps
the code unit; a Lean string cannot). -/
def fromCharCode (n : Nat) : Char :=
  let u := n % 65536
  if 0xD800 ≤ u && u ≤ 0xDFFF then Char.ofNat 0xFFFD else Char.ofNat u

/-- `.replace(/&#(\d{1,5});/g, (_, n) => String.fromCharCode(Number(n)))`. -/
def decodeNumeric (s : String) : String := Id.run do
  let a := s.toList.toArray
  let mut out : Array Char := #[]
  let mut i := 0
  while i < a.size do
    if a[i]! == '&' && i + 1 < a.size && a[i + 1]! == '#' then
      let mut j := i + 2
      while j < a.size && isDigit a[j]! do j := j + 1
      let d := j - (i + 2)
      if 1 ≤ d && d ≤ 5 && j < a.size && a[j]! == ';' then
        let v := (a.extract (i + 2) j).foldl (fun n c => n * 10 + (c.toNat - 48)) 0
        out := out.push (fromCharCode v)
        i := j + 1
        continue
    out := out.push a[i]!
    i := i + 1
  return String.ofList out.toList

/-- `decodeEntities`, pass for pass in its order. -/
def decodeEntities (s : String) : String :=
  let s := replaceAll s "&nbsp;" " " true
  let s := replaceAll s "&lt;" "<" true
  let s := replaceAll s "&gt;" ">" true
  let s := replaceAll s "&quot;" "\"" true
  -- `/&#39;|&apos;/gi`: the replacement cannot make either, so two passes are one
  let s := replaceAll s "&#39;" "'" true
  let s := replaceAll s "&apos;" "'" true
  let s := decodeNumeric s
  replaceAll s "&amp;" "&" true

/-- `.replace(/<[^>]*>/g, " ")`. -/
def dropTags (s : String) : String := Id.run do
  let a := s.toList.toArray
  let mut out : Array Char := #[]
  let mut i := 0
  while i < a.size do
    if a[i]! == '<' then
      match Core.Text.Str.indexOfChar a '>' (i + 1) with
      | some j =>
        out := out.push ' '
        i := j + 1
        continue
      | none => pure ()
    out := out.push a[i]!
    i := i + 1
  return String.ofList out.toList

/-- `.replace(/\s+/g, " ")`. -/
def collapseWs (s : String) : String := Id.run do
  let mut out : Array Char := #[]
  let mut inWs := false
  for c in s.toList do
    if isJsWhitespace c then
      if !inWs then out := out.push ' '
      inWs := true
    else
      out := out.push c
      inWs := false
  return String.ofList out.toList

/-- `stripTags`: tags out, entities decoded, whitespace collapsed, trimmed. -/
def stripTags (html : String) : String := trim (collapseWs (decodeEntities (dropTags html)))

end Core.Junk.Js
