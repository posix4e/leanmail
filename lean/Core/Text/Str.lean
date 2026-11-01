namespace Core.Text.Str

/-! Small string helpers the parsers share. Mail is mostly ASCII punctuation around text, so
these work on `List Char` and stay obvious. -/

def isWsp (c : Char) : Bool := c == ' ' || c == '\t'
def isSpaceLike (c : Char) : Bool := c == ' ' || c == '\t' || c == '\r' || c == '\n'
def isDigit (c : Char) : Bool := '0' ≤ c && c ≤ '9'
def isHex (c : Char) : Bool := isDigit c || ('a' ≤ c && c ≤ 'f') || ('A' ≤ c && c ≤ 'F')
def isAlpha (c : Char) : Bool := ('a' ≤ c && c ≤ 'z') || ('A' ≤ c && c ≤ 'Z')
def isWordChar (c : Char) : Bool := isAlpha c || isDigit c || c == '_'

def hexVal (c : Char) : Nat :=
  if isDigit c then c.toNat - 48
  else if 'a' ≤ c && c ≤ 'f' then c.toNat - 97 + 10
  else if 'A' ≤ c && c ≤ 'F' then c.toNat - 65 + 10
  else 0

/-- JavaScript's `\s` (and what `String.prototype.trim` strips). -/
def isJsWhitespace (c : Char) : Bool :=
  let n := c.toNat
  n == 32 || (9 ≤ n && n ≤ 13) || n == 0xA0 || n == 0x1680 || (0x2000 ≤ n && n ≤ 0x200A) ||
    n == 0x2028 || n == 0x2029 || n == 0x202F || n == 0x205F || n == 0x3000 || n == 0xFEFF

def trimBy (p : Char → Bool) (s : String) : String :=
  String.ofList (s.toList.dropWhile p |>.reverse.dropWhile p |>.reverse)

/-- Only SP and HTAB, as RFC 5322 allows around a field name. -/
def trimWsp (s : String) : String := trimBy isWsp s
/-- SP, HTAB, CR and LF. -/
def trim (s : String) : String := trimBy isSpaceLike s
/-- JavaScript's `String.prototype.trim`. -/
def trimJs (s : String) : String := trimBy isJsWhitespace s
def trimLeft (s : String) : String := String.ofList (s.toList.dropWhile isSpaceLike)
def trimRight (s : String) : String := String.ofList (s.toList.reverse.dropWhile isSpaceLike |>.reverse)

def replaceChars (s : String) (f : Char → Char) : String := String.ofList (s.toList.map f)

/-- Collapse every run of characters satisfying `p` into `rep`. -/
def collapse (p : Char → Bool) (rep : String) (s : String) : String := Id.run do
  let mut out := ""
  let mut inRun := false
  for c in s.toList do
    if p c then
      if !inRun then out := out ++ rep
      inRun := true
    else
      out := out.push c
      inRun := false
  return out

/-- Lines split on LF, with a trailing CR removed from each. -/
def lines (s : String) : List String :=
  (s.splitOn "\n").map fun l => if l.endsWith "\r" then String.ofList (l.toList.dropLast) else l

def startsWithCI (s prefix_ : String) : Bool := s.toLower.startsWith prefix_.toLower

/-- The index of the first occurrence of `needle` at or after `from`, over a char array. -/
def indexOf (a : Array Char) (needle : Array Char) (from_ : Nat := 0) : Option Nat := Id.run do
  if needle.isEmpty then return some from_
  let n := a.size
  let m := needle.size
  if m > n then return none
  for i in [from_ : n - m + 1] do
    let mut ok := true
    for j in [0 : m] do
      if a[i + j]! != needle[j]! then
        ok := false
        break
    if ok then return some i
  return none

def indexOfChar (a : Array Char) (c : Char) (from_ : Nat := 0) : Option Nat := Id.run do
  for i in [from_ : a.size] do
    if a[i]! == c then return some i
  return none

def lastIndexOfChar (a : Array Char) (c : Char) : Option Nat := Id.run do
  let mut r := none
  for i in [0 : a.size] do
    if a[i]! == c then r := some i
  return r

def slice (a : Array Char) (from_ to_ : Nat) : String :=
  String.ofList (a.extract from_ to_).toList

def natOf (s : String) : Option Nat := s.toNat?

def repeatChar (c : Char) (n : Nat) : String := String.ofList (List.replicate n c)

end Core.Text.Str
