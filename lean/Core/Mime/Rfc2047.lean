import Core.Base64
import Core.Text.Str
import Core.Text.Charset

namespace Core.Mime.Rfc2047

/-! # Encoded words in headers: `=?charset?Q?text?=` and `=?charset?B?text?=`

Port of postal-mime's `decodeWords`: adjacent encoded words are joined before decoding when they
share charset and encoding (a multi-byte character is often split across two), the whitespace
between two encoded words is dropped, and if joining produced a replacement character the words
are decoded separately instead. -/

open Core.Text Str

inductive Token where
  | text (s : String)
  | word (charset : String) (encoding : Char) (payload : String)

/-- Decode one word's payload under its charset. The RFC 2231 language tag after `*` is ignored. -/
def decodeWord (charset : String) (encoding : Char) (payload : String) : String :=
  let charset := (charset.splitOn "*").headD charset
  let bytes : ByteArray :=
    if encoding == 'Q' || encoding == 'q' then Id.run do
      -- `= <ws> hex` is a broken fold: drop the whitespace; `_` and whitespace are spaces
      let cs := payload.toList.toArray
      let mut fixed := ""
      let mut i := 0
      while i < cs.size do
        let c := cs[i]!
        if c == '=' then
          let mut j := i + 1
          while j < cs.size && isJsWhitespace cs[j]! do j := j + 1
          if j > i + 1 && j < cs.size && isHex cs[j]! then
            fixed := fixed.push '='
            i := j
          else
            fixed := fixed.push '='
            i := i + 1
        else if c == '_' || isJsWhitespace c then
          fixed := fixed.push ' '
          i := i + 1
        else
          fixed := fixed.push c
          i := i + 1
      let b := fixed.toUTF8
      let mut out := ByteArray.empty
      let mut k := 0
      while k < b.size do
        let x := b[k]!
        if x == 61 && k + 2 < b.size && isHex (Char.ofNat b[k+1]!.toNat) && isHex (Char.ofNat b[k+2]!.toNat) then
          out := out.push ((hexVal (Char.ofNat b[k+1]!.toNat) * 16 + hexVal (Char.ofNat b[k+2]!.toNat)).toUInt8)
          k := k + 3
        else
          out := out.push x
          k := k + 1
      return out
    else if encoding == 'B' || encoding == 'b' then
      Base64.decodePostal (String.ofList (payload.toList.filter fun c => isAlpha c || isDigit c || c == '+' || c == '/' || c == '=')).toUTF8
    else payload.toUTF8
  Charset.decode charset bytes

/-- Split a header value into literal text and encoded words. -/
def tokenize (s : String) : List Token := Id.run do
  let a := s.toList.toArray
  let n := a.size
  let mut tokens : Array Token := #[]
  let mut lit := ""
  let mut i := 0
  while i < n do
    -- try to match =?charset?E?payload?= at i
    let mut matched := false
    if a[i]! == '=' && i + 1 < n && a[i+1]! == '?' then
      let mut j := i + 2
      let cs0 := j
      while j < n && a[j]! != '?' && !isJsWhitespace a[j]! do j := j + 1
      if j > cs0 && j < n && a[j]! == '?' then
        let charset := slice a cs0 j
        j := j + 1
        if j < n && (a[j]! == 'Q' || a[j]! == 'q' || a[j]! == 'B' || a[j]! == 'b') && j + 1 < n && a[j+1]! == '?' then
          let enc := a[j]!
          j := j + 2
          let p0 := j
          while j < n && a[j]! != '?' do j := j + 1
          if j + 1 < n && a[j]! == '?' && a[j+1]! == '=' then
            let payload := slice a p0 j
            if !lit.isEmpty then
              tokens := tokens.push (.text lit)
              lit := ""
            tokens := tokens.push (.word charset enc payload)
            i := j + 2
            matched := true
    if !matched then
      lit := lit.push a[i]!
      i := i + 1
  if !lit.isEmpty then tokens := tokens.push (.text lit)
  return tokens.toList

private def isSeparator (s : String) : Bool :=
  !s.isEmpty && s.toList.all isSpaceLike

private def canJoin (c1 : String) (e1 : Char) (p1 : String) (c2 : String) (e2 : Char) : Bool :=
  c1 == c2 && e1.toUpper == e2.toUpper &&
    (if e1.toUpper == 'B' then p1.length % 4 == 0 && !p1.endsWith "=" else true)

private def render (tokens : List Token) (join : Bool) : String := Id.run do
  let arr := tokens.toArray
  let mut result := ""
  let mut pending : Option (String × Char × String) := none
  for h : i in [0 : arr.size] do
    match arr[i] with
    | .text t =>
      let nextIsWord := match arr[i+1]? with | some (.word ..) => true | _ => false
      if pending.isSome && nextIsWord && isSeparator t then
        continue
      if let some (c, e, p) := pending then
        result := result ++ decodeWord c e p
        pending := none
      result := result ++ t
    | .word c e p =>
      match pending with
      | some (c1, e1, p1) =>
        if join && canJoin c1 e1 p1 c e then
          pending := some (c1, e1, p1 ++ p)
        else
          result := result ++ decodeWord c1 e1 p1
          pending := some (c, e, p)
      | none => pending := some (c, e, p)
  if let some (c, e, p) := pending then
    result := result ++ decodeWord c e p
  return result

/-- Decode every encoded word in a header value. -/
def decodeWords (s : String) : String :=
  let tokens := tokenize s
  let joined := render tokens true
  if joined.toList.contains '�' then render tokens false else joined

/-- True when the string is nothing but encoded words, each optionally followed by whitespace
(`^(?:word\s*)+$`). -/
def isEncodedWordsOnly (s : String) : Bool :=
  match tokenize s with
  | .word .. :: rest => rest.all fun | .word .. => true | .text t => t.toList.all isJsWhitespace
  | _ => false

end Core.Mime.Rfc2047
