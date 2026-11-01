import Core.Text.Str
import Core.Text.HtmlEntities

namespace Core.Text.Html

/-! # HTML helpers: entities, escaping, and a readable text rendering

Ports of postal-mime 3.0's `text-format.js`: `decodeHTMLEntities`, `escapeHtml`, `textToHtml`, and
`htmlToText` in the linear-time form MailVault patches in, step for step (each step is the same
rewrite one of the original regular expressions made, odd corners included). Indices are over
characters where JavaScript counts UTF-16 code units; nothing here depends on the difference. -/

open Str

/-! ## Entities -/

private def isAsciiLetter (c : Char) : Bool := ('a' ≤ c && c ≤ 'z') || ('A' ≤ c && c ≤ 'Z')
private def isHexDigit (c : Char) : Bool := isDigit c || ('a' ≤ c && c ≤ 'f') || ('A' ≤ c && c ≤ 'F')

/-- `str.replace(/&(#\d+|#x[a-f0-9]+|[a-z]+\d*);?/gi, …)`: a known name (with or without `;`, as
the table lists it) becomes its text, `&#N;` and `&#xH;` their code point (U+FFFD when out of
range, and U+0000 for `&#X…;`, which JavaScript's `parseInt` reads as NaN); everything else stays. -/
def decodeEntities (s : String) : String := Id.run do
  let a := s.toList.toArray
  let n := a.size
  let mut out := ""
  let mut i := 0
  while i < n do
    let c := a[i]!
    if c != '&' then
      out := out.push c
      i := i + 1
      continue
    -- the entity body after `&`, by the regex's alternatives in order
    let mut j := i + 1
    let mut kind := 0 -- 0 none, 1 decimal, 2 hex, 3 name
    if j < n && a[j]! == '#' then
      let mut k := j + 1
      while k < n && isDigit a[k]! do k := k + 1
      if k > j + 1 then
        kind := 1
        j := k
      else if j + 1 < n && (a[j + 1]! == 'x' || a[j + 1]! == 'X') then
        let mut h := j + 2
        while h < n && isHexDigit a[h]! do h := h + 1
        if h > j + 2 then
          kind := 2
          j := h
    else
      let mut k := j
      while k < n && isAsciiLetter a[k]! do k := k + 1
      if k > j then
        while k < n && isDigit a[k]! do k := k + 1
        kind := 3
        j := k
    if kind == 0 then
      out := out.push c
      i := i + 1
      continue
    let entity := slice a (i + 1) j
    let terminated := j < n && a[j]! == ';'
    let endIdx := if terminated then j + 1 else j
    let whole := slice a (i + 1) endIdx
    match HtmlEntities.table.get? whole with
    | some v => out := out ++ v
    | none =>
      if kind == 3 || !terminated then
        out := out ++ slice a i endIdx
      else
        let cs := entity.toList.drop 1
        let cp : Option Nat :=
          if cs.head? == some 'x' then some ((cs.drop 1).foldl (fun acc c => acc * 16 + hexVal c) 0)
          else if cs.head? == some 'X' then none
          else some (cs.foldl (fun acc c => acc * 10 + (c.toNat - 48)) 0)
        match cp with
        | none => out := out.push (Char.ofNat 0)
        | some cp =>
          if (0xD800 ≤ cp && cp ≤ 0xDFFF) || cp > 0x10FFFF then out := out.push '�'
          else out := out.push (Char.ofNat cp)
    i := endIdx
  return out

/-- `escapeHtml`: trim, then `< > " ' ? &` as numeric entities. -/
def escape (s : String) : String := Id.run do
  let mut out := ""
  for c in (trimJs s).toList do
    if c == '<' || c == '>' || c == '"' || c == '\'' || c == '?' || c == '&' then
      let hex := String.ofList (Nat.toDigits 16 c.toNat) |>.toUpper
      out := out ++ "&#x" ++ (if hex.length < 2 then "0" ++ hex else hex) ++ ";"
    else out := out.push c
  return out

def textToHtml (s : String) : String :=
  "<div>" ++ ((escape s).replace "\n" "<br />") ++ "</div>"

/-! ## HTML to text

Each step below mirrors one function of the patched `text-format.js`. Positions are `Int` so that
JavaScript's `-1` (not found) and `-2` (not looked yet) carry over unchanged. -/

private abbrev Chars := Array Char

private def isWordC (c : Char) : Bool := isDigit c || isAsciiLetter c || c == '_'
private def isTerm (c : Char) : Bool := c == '\n' || c == '\r' || c == ' ' || c == ' '

/-- `charCodeAt`, with a character no test matches for an index out of range (JavaScript's NaN). -/
@[inline] private def at_ (s : Chars) (i : Int) : Char :=
  if i < 0 then '￿' else s.getD i.toNat '￿'

private def sl (s : Chars) (a b : Int) : Chars :=
  let a := (max a 0).toNat
  let b := (max b 0).toNat
  s.extract a b

/-- `indexOf(needle, from)`. -/
private def indexOf (s : Chars) (needle : Chars) (from_ : Int) : Int := Id.run do
  let f := (max from_ 0).toNat
  if needle.size > s.size then return -1
  for i in [f : s.size - needle.size + 1] do
    let mut ok := true
    for j in [0 : needle.size] do
      if s[i + j]! != needle[j]! then
        ok := false
        break
    if ok then return i
  return -1

private def indexOfC (s : Chars) (c : Char) (from_ : Int) : Int := Id.run do
  for i in [(max from_ 0).toNat : s.size] do
    if s[i]! == c then return i
  return -1

private def lastIndexOfC (s : Chars) (c : Char) : Int := Id.run do
  let mut i := s.size
  while i > 0 do
    i := i - 1
    if s[i]! == c then return i
  return -1

private def wordEnd (s : Chars) (p : Int) : Int := Id.run do
  let mut q := p
  while q < s.size && isWordC (at_ s q) do q := q + 1
  return q

private def nextTerminator (s : Chars) (from_ : Int) : Int := Id.run do
  for i in [(max from_ 0).toNat : s.size] do
    if isTerm s[i]! then return i
  return -1

private def lower (s : Chars) : String := (String.ofList s.toList).toLower

/-- Where a tag's name ends when the `<` at `i` opens one of `names` (`none`: any tag). -/
private def tagNameEnd (s : Chars) (i : Int) (names : Option (List String)) (slash exact : Bool) : Int :=
  match names with
  | none => i + 1
  | some ns =>
    let p := if slash && at_ s (i + 1) == '/' then i + 2 else i + 1
    let q := wordEnd s p
    if q == p then -1 else
    let name := String.ofList (sl s p q).toList
    if ns.contains (if exact then name else name.toLower) then q else -1

private def finish (s out : Chars) (at0 : Int) : Chars :=
  if at0 == 0 && out.isEmpty then s else out ++ sl s at0 s.size

/-- `/<NAME\b[^>]*>/` → `to`, for every tag of `names`; `skip` moves past what follows. -/
private def replaceTags (s : Chars) (names : Option (List String)) (slash exact : Bool) (to_ : String)
    (skip : Option (Chars → Int → Int) := none) : Chars := Id.run do
  let toA := to_.toList.toArray
  let mut out : Chars := #[]
  let mut at0 : Int := 0
  let mut gt : Int := -2
  let mut i := indexOfC s '<' 0
  while i != -1 do
    let e := tagNameEnd s i names slash exact
    if e != -1 then
      if gt != -1 && gt < e then gt := indexOfC s '>' e
      if gt == -1 then break
      out := out ++ sl s at0 i ++ toA
      at0 := match skip with | some f => f s (gt + 1) | none => gt + 1
      i := at0 - 1
    i := indexOfC s '<' (i + 1)
  return finish s out at0

/-- `/<!--.*?-->/g` → a space (a comment on one line). -/
private def stripComments (s : Chars) : Chars := Id.run do
  let opener := "<!--".toList.toArray
  let closer := "-->".toList.toArray
  let mut out : Chars := #[]
  let mut at0 : Int := 0
  let mut close : Int := -2
  let mut term : Int := -2
  let mut i := indexOf s opener 0
  while i != -1 do
    let from_ := i + 4
    if close != -1 && close < from_ then close := indexOf s closer from_
    if close == -1 then break
    if term != -1 && term < from_ then term := nextTerminator s from_
    if !(term != -1 && term < close) then
      out := out ++ sl s at0 i |>.push ' '
      at0 := close + 3
      i := at0 - 1
    i := indexOf s opener (i + 1)
  return finish s out at0

/-- `/<script\b[^>]*>.*?<\/script\b[^>]*>/gi` → a space. -/
private def stripScripts (s : Chars) : Chars := Id.run do
  let scripts := some ["script"]
  let mut out : Chars := #[]
  let mut at0 : Int := 0
  let mut gt : Int := -2
  let mut closer : Int := -2
  let mut closerEnd : Int := -1
  let mut term : Int := -2
  let lt2 := "</".toList.toArray
  let mut i := indexOfC s '<' 0
  while i != -1 do
    let e := tagNameEnd s i scripts false false
    if e != -1 then
      if gt != -1 && gt < e then gt := indexOfC s '>' e
      if gt == -1 then break
      let from_ := gt + 1
      if closer != -1 && closer < from_ then
        closer := -1
        let mut j := indexOf s lt2 from_
        while j != -1 do
          let e2 := tagNameEnd s j scripts true false
          if e2 != -1 then
            let g := indexOfC s '>' e2
            if g == -1 then break
            closer := j
            closerEnd := g
            break
          j := indexOf s lt2 (j + 1)
      if closer == -1 then break
      if term != -1 && term < from_ then term := nextTerminator s from_
      if !(term != -1 && term < closer) then
        out := out ++ sl s at0 i |>.push ' '
        at0 := closerEnd + 1
        i := at0 - 1
    i := indexOfC s '<' (i + 1)
  return finish s out at0

/-- `/^.*<NAME\b[^>]*>/i` → nothing. -/
private def stripThrough (s : Chars) (name : String) (slash bang : Bool) : Chars := Id.run do
  let limit := nextTerminator s 0
  let stop : Int := if limit == -1 then s.size else limit
  let lastGt := lastIndexOfC s '>'
  let mut found : Int := -1
  let mut foundEnd : Int := -1
  let mut i := indexOfC s '<' 0
  while i != -1 && i < stop do
    let mut p := i + 1
    let mut ok := true
    if slash then
      if at_ s p != '/' then ok := false else p := p + 1
    if ok && bang then
      if at_ s p != '!' then ok := false else p := p + 1
    if ok then
      let q := wordEnd s p
      if !(q == p || lower (sl s p q) != name) && !(lastGt < q) then
        found := i
        foundEnd := q
    i := indexOfC s '<' (i + 1)
  if found == -1 then return s
  return sl s (indexOfC s '>' foundEnd + 1) s.size

/-- `/<\/NAME\b[^>]*>.*$/i` → nothing. -/
private def stripFrom (s : Chars) (name : String) : Chars := Id.run do
  let mut lastTerminator : Int := -1
  let mut k := s.size
  while k > 0 do
    k := k - 1
    if isTerm s[k]! then
      lastTerminator := k
      break
  let mut gt : Int := -2
  let lt2 := "</".toList.toArray
  let mut i := indexOf s lt2 0
  while i != -1 do
    let p := i + 2
    let q := wordEnd s p
    if !(q == p || lower (sl s p q) != name) then
      if gt != -1 && gt < q then gt := indexOfC s '>' q
      if gt == -1 then return s
      if gt ≥ lastTerminator then return sl s 0 i
    i := indexOf s lt2 (i + 1)
  return s

/-- `/<a\b[^>]*href\s*=\s*["']?([^\s"']+)[^>]*>/gi` → ` ($1) `. -/
private def replaceAnchors (s : Chars) : Chars := Id.run do
  let lastGt := lastIndexOfC s '>'
  if lastGt == -1 then return s
  let lc (i : Int) : Char := (at_ s i).toLower
  let mut hrefAt : Array Int := #[]
  let mut valueAt : Array Int := #[]
  let len : Int := s.size
  let mut i : Int := 0
  while i + 4 ≤ len do
    if lc i == 'h' && lc (i + 1) == 'r' && lc (i + 2) == 'e' && lc (i + 3) == 'f' then
      let mut j := i + 4
      while j < len && isJsWhitespace (at_ s j) do j := j + 1
      if at_ s j == '=' then
        j := j + 1
        while j < len && isJsWhitespace (at_ s j) do j := j + 1
        let mut v := j
        if at_ s v == '"' || at_ s v == '\'' then v := v + 1
        let first := at_ s v
        if !(v ≥ len || isJsWhitespace first || first == '"' || first == '\'') && !(lastGt < v + 1) then
          hrefAt := hrefAt.push i
          valueAt := valueAt.push v
    i := i + 1
  if hrefAt.isEmpty then return s
  let mut out : Chars := #[]
  let mut at0 : Int := 0
  let mut gt : Int := -2
  let mut t := indexOfC s '<' 0
  while t != -1 do
    let e := tagNameEnd s t (some ["a"]) false false
    if e != -1 then
      if gt != -1 && gt < e then gt := indexOfC s '>' e
      if gt == -1 then break
      -- the last href (with a value) whose name lies before the tag's first `>`
      let mut lo := 0
      let mut hi := hrefAt.size
      while lo < hi do
        let mid := (lo + hi) / 2
        if hrefAt[mid]! + 4 ≤ gt then lo := mid + 1 else hi := mid
      if lo > 0 && hrefAt[lo - 1]! ≥ e then
        let v := valueAt[lo - 1]!
        let mut r := v
        while r < len && !(isJsWhitespace (at_ s r) || at_ s r == '"' || at_ s r == '\'') do r := r + 1
        let c := if r ≤ lastGt then r else lastGt
        let e2 := if r ≤ lastGt then indexOfC s '>' r else lastGt
        out := out ++ sl s at0 t ++ " (".toList.toArray ++ sl s v c ++ ") ".toList.toArray
        at0 := e2 + 1
        t := at0 - 1
    t := indexOfC s '<' (t + 1)
  return finish s out at0

/-- What `<li>` takes with it: `[\n\u0001\s]*`. -/
private def skipItemSpace (s : Chars) (from_ : Int) : Int := Id.run do
  let mut e := from_
  while e < s.size do
    let c := at_ s e
    if c == '\n' || c == '\x01' || isJsWhitespace c then e := e + 1 else break
  return e

/-- `/^\s+$/gm` → nothing. -/
private def stripBlankLines (s : Chars) : Chars := Id.run do
  let mut out : Chars := #[]
  let mut at0 : Int := 0
  let n : Int := s.size
  let mut i : Int := 0
  while i < n do
    if (i == 0 || isTerm (at_ s (i - 1))) && isJsWhitespace (at_ s i) then
      let mut r := i
      while r < n && isJsWhitespace (at_ s r) do r := r + 1
      let mut e : Int := -1
      if r == n then e := n
      else
        let mut j := r - 1
        while j > i do
          if isTerm (at_ s j) then
            e := j
            break
          j := j - 1
      if e == -1 then
        i := r
        continue
      out := out ++ sl s at0 i
      at0 := e
      i := e
      continue
    i := i + 1
  return finish s out at0

/-- Runs of `p` → `rep`. -/
private def collapseRuns (s : Chars) (p : Char → Bool) (rep : Chars) : Chars := Id.run do
  let mut out : Chars := #[]
  let mut i := 0
  while i < s.size do
    if p s[i]! then
      while i < s.size && p s[i]! do i := i + 1
      out := out ++ rep
    else
      out := out.push s[i]!
      i := i + 1
  return out

/-- postal-mime's `htmlToText`. -/
def toText (html : String) : String := Id.run do
  -- tags are matched on one line, so line breaks become U+0001 first
  let mut s : Chars := #[]
  let a := html.toList.toArray
  let mut i := 0
  while i < a.size do
    if a[i]! == '\r' && i + 1 < a.size && a[i + 1]! == '\n' then
      s := s.push '\x01'
      i := i + 2
    else
      s := s.push (if a[i]! == '\n' then '\x01' else a[i]!)
      i := i + 1
  s := stripComments s
  s := replaceTags s (some ["br"]) false false "\n"
  s := replaceTags s (some ["p", "div", "table", "tr", "td", "th"]) true false "\n\n"
  s := stripScripts s
  s := stripThrough s "body" false false
  s := stripThrough s "head" true false
  s := stripThrough s "doctype" false true
  s := stripFrom s "body"
  s := stripFrom s "html"
  s := replaceAnchors s
  s := replaceTags s (some ["span", "em", "i", "strong", "b", "u", "a"]) true false ""
  s := replaceTags s (some ["li"]) false false "* " (some skipItemSpace)
  s := replaceTags s (some ["hr"]) false true "\n-------------\n"
  s := replaceTags s none false false " "
  s := s.map fun c => if c == '\x01' then '\n' else c
  s := collapseRuns s (fun c => c == ' ' || c == '\t') #[' ']
  s := stripBlankLines s
  -- `/\n\n+/g` → `\n\n`
  let mut t : Chars := #[]
  let mut k := 0
  while k < s.size do
    if s[k]! == '\n' && k + 1 < s.size && s[k + 1]! == '\n' then
      while k < s.size && s[k]! == '\n' do k := k + 1
      t := t.push '\n' |>.push '\n'
    else
      t := t.push s[k]!
      k := k + 1
  -- `/^\n+/` → `\n`, `/\n+$/` → `\n`
  let lead := (t.toList.takeWhile (· == '\n')).length
  if lead > 0 then t := #['\n'] ++ t.extract lead t.size
  let trail := (t.toList.reverse.takeWhile (· == '\n')).length
  if trail > 0 then t := t.extract 0 (t.size - trail) |>.push '\n'
  return decodeEntities (String.ofList t.toList)

end Core.Text.Html
