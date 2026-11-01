import Core.Text.Str
import Core.Mime.Date

/-! # The search grammar

A port of MailVault's `shared/query.js`, kept pure:

* free terms, AND-ed; a term matches a token exactly, or as a prefix when 3+ characters
* `"quoted phrase"`; `-term` and `-"phrase"` exclude
* `from: to: subject: list: source:` string filters (value may be quoted)
* `has:attachment has:invite`, `is:expiring is:forever is:event is:snoozed is:archived is:toread
  is:junk`, `in:sent in:inbox` boolean filters; a leading `-` flips them
* `before:YYYY-MM-DD` (that day included), `after:YYYY-MM-DD`

Differences from the JavaScript: dates are UTC days (the server has no local time zone), and
`normalizeText` lower-cases without NFKC (Lean has no Unicode normalisation tables). Anything that
does not parse stays as free text, so the user sees "no results" rather than an ignored token. -/

namespace Core.Query

open Core.Text Str

def maxTokens : Nat := 2000
def minTokenLen : Nat := 2

/-- Lower-case, whitespace collapsed to one space, trimmed. -/
def normalizeText (s : String) : String :=
  let lowered := String.ofList (s.toList.map Char.toLower)
  " ".intercalate ((lowered.split isSpaceLike).toList.map (·.toString) |>.filter (· ≠ ""))

/-- A letter, digit or mark: ASCII alphanumerics, and every non-ASCII character that is not a
space or common punctuation (Lean has no Unicode category tables; this keeps accented and
non-Latin words whole). -/
def isWord (c : Char) : Bool :=
  c.isAlphanum ||
    (c.toNat ≥ 0x80 && !(c.toNat ≥ 0x2000 && c.toNat ≤ 0x206F) && c.toNat != 0xA0 &&
      c.toNat != 0x3000 && c.toNat != 0xFEFF && !(c.toNat ≥ 0x3001 && c.toNat ≤ 0x3003) &&
      c.toNat != 0xFF0C && c.toNat != 0xFF0E)

private def isLetterish (c : Char) : Bool := c.isAlpha || (c.toNat ≥ 0x80 && isWord c)

private def isLocalChar (c : Char) : Bool :=
  isWord c || c == '.' || c == '_' || c == '%' || c == '+' || c == '\'' || c == '-'

private def isDomainChar (c : Char) : Bool := isWord c || c == '.' || c == '-'

/-- The email addresses in normalized text, as the regex
`[w][w._%+'-]*@[w][w.-]*\.[letters]{2,}` finds them, left to right. -/
def addresses (s : String) : Array String := Id.run do
  let a := s.toList.toArray
  let mut out := #[]
  let mut i := 0
  let mut minStart := 0
  while i < a.size do
    if a[i]! == '@' then
      -- left: the run of local characters, starting at a word character
      let mut l := i
      while l > minStart && isLocalChar a[l - 1]! do l := l - 1
      while l < i && !isWord a[l]! do l := l + 1
      -- right: the run of domain characters, cut back to end in `.` + 2 letters
      let mut r := i + 1
      while r < a.size && isDomainChar a[r]! do r := r + 1
      let mut found : Option Nat := none
      if l < i && i + 1 < a.size && isWord a[i + 1]! then
        let mut e := r
        while e > i + 1 && found.isNone do
          -- a[k .. e) letters (≥2), a[k-1] = '.'
          let mut k := e
          while k > i + 1 && isLetterish a[k - 1]! do k := k - 1
          if e - k ≥ 2 && k > i + 2 && a[k - 1]! == '.' then found := some e
          else e := e - 1
      match found with
      | some e =>
        out := out.push (String.ofList (a.extract l e).toList)
        minStart := e
        i := e
      | none => i := i + 1
    else i := i + 1
  return out

private def pushTok (out : Array String) (t : String) (max : Nat) : Array String :=
  if t.length ≥ minTokenLen && out.size < max && !out.contains t then out.push t else out

/-- Tokens of normalized text: each address whole plus its local part and domain, then the words;
unique, at least two characters, at most `max`. -/
def tokenizeNormalized (s : String) (max : Nat := maxTokens) : Array String := Id.run do
  let mut out : Array String := #[]
  if s.contains '@' then
    for addr in addresses s do
      match addr.splitOn "@" with
      | loc :: rest =>
        out := pushTok out addr max
        out := pushTok out loc max
        out := pushTok out ("@".intercalate rest) max
      | [] => pure ()
  for w in (s.split (!isWord ·)).toList do
    out := pushTok out w.toString max
  return out

def tokenize (s : String) (max : Nat := maxTokens) : Array String :=
  tokenizeNormalized (normalizeText s) max

structure Filters where
  from_ : Array String := #[]
  to_ : Array String := #[]
  subject : Array String := #[]
  list : Array String := #[]
  source : Array String := #[]
  hasAttachment : Option Bool := none
  hasInvite : Option Bool := none
  isExpiring : Option Bool := none
  isForever : Option Bool := none
  isEvent : Option Bool := none
  isSnoozed : Option Bool := none
  isArchived : Option Bool := none
  isToRead : Option Bool := none
  isJunk : Option Bool := none
  inSent : Option Bool := none
  /-- Epoch ms, exclusive: the start of the day after the named one. -/
  before : Option Nat := none
  /-- Epoch ms, inclusive: the start of the named day. -/
  after : Option Nat := none
  deriving Inhabited, Repr, BEq

structure Query where
  terms : Array String := #[]
  phrases : Array String := #[]
  negations : Array String := #[]
  filters : Filters := {}
  deriving Inhabited, Repr, BEq

structure RawToken where
  neg : Bool
  key : Option String
  value : String
  quoted : Bool
  deriving Inhabited, Repr

def stringKeys : List String := ["from", "to", "subject", "list", "source"]
def keys : List String := stringKeys ++ ["has", "is", "in", "before", "after"]

/-- Split into raw tokens, honouring quotes standalone and as filter values. -/
def scan (text : String) : Array RawToken := Id.run do
  let a := text.toList.toArray
  let n := a.size
  let mut out := #[]
  let mut i := 0
  while i < n do
    let c := a[i]!
    if isSpaceLike c || c == '\x0B' || c == '\x0C' then
      i := i + 1
      continue
    let mut neg := false
    if c == '-' then
      neg := true
      i := i + 1
    if i < n && a[i]! == '"' then
      let start := i + 1
      let e := (indexOfChar a '"' start).getD n
      out := out.push { neg, key := none, value := slice a start e, quoted := true }
      i := if e < n then e + 1 else n
      continue
    let start := i
    while i < n && !(isSpaceLike a[i]!) && a[i]! != '"' do i := i + 1
    let word := slice a start i
    if word.isEmpty then continue
    let wa := word.toList.toArray
    let keyLen := (wa.toList.takeWhile Char.isAlpha).length
    let key := (String.ofList (wa.toList.take keyLen)).toLower
    if keyLen > 0 && keyLen < wa.size && wa[keyLen]! == ':' && keys.contains key then
      let mut value := String.ofList (wa.toList.drop (keyLen + 1))
      let mut quoted := false
      if value.isEmpty && i < n && a[i]! == '"' then
        let s := i + 1
        let e := (indexOfChar a '"' s).getD n
        value := slice a s e
        i := if e < n then e + 1 else n
        quoted := true
      out := out.push { neg, key := some key, value, quoted }
    else
      out := out.push { neg, key := none, value := word, quoted := false }
  return out

/-- `YYYY-MM-DD` → UTC midnight in epoch ms plus `dayOffset` days; `none` when invalid. -/
def parseDay (s : String) (dayOffset : Nat) : Option Nat := do
  match (trim s).splitOn "-" with
  | [ys, ms, ds] =>
    guard (ys.length == 4 && ms.length ≥ 1 && ms.length ≤ 2 && ds.length ≥ 1 && ds.length ≤ 2)
    let y ← ys.toNat?
    let m ← ms.toNat?
    let d ← ds.toNat?
    guard (1 ≤ m && m ≤ 12 && 1 ≤ d && d ≤ Core.Mime.Date.daysInMonth y m)
    let days := Core.Mime.Date.daysFromCivil y m d + dayOffset
    guard (days ≥ 0)
    return days.toNat * 86400000
  | _ => none

def parse (text : String) : Query := Id.run do
  let mut q : Query := {}
  let addFree (q : Query) (raw : String) (neg : Bool) : Query :=
    (tokenize raw).foldl (init := q) fun q t =>
      if neg then (if q.negations.contains t then q else { q with negations := q.negations.push t })
      else (if q.terms.contains t then q else { q with terms := q.terms.push t })
  let addTo (xs : Array String) (v : String) := if xs.contains v then xs else xs.push v
  for tok in scan text do
    match tok.key with
    | none =>
      if tok.quoted then
        let p := normalizeText tok.value
        if p.isEmpty then continue
        if tok.neg then q := addFree q p true
        else if !q.phrases.contains p then q := { q with phrases := q.phrases.push p }
      else q := addFree q tok.value tok.neg
    | some key =>
      let v := normalizeText tok.value
      if stringKeys.contains key then
        if v.isEmpty then continue
        if tok.neg then q := addFree q tok.value true
        else
          let f := q.filters
          q := { q with filters := match key with
            | "from" => { f with from_ := addTo f.from_ v }
            | "to" => { f with to_ := addTo f.to_ v }
            | "subject" => { f with subject := addTo f.subject v }
            | "list" => { f with list := addTo f.list v }
            | _ => { f with source := addTo f.source v } }
        continue
      let flag := some !tok.neg
      let f := q.filters
      if key == "has" && (v == "attachment" || v == "attachments") then q := { q with filters := { f with hasAttachment := flag } }
      else if key == "has" && v == "invite" then q := { q with filters := { f with hasInvite := flag } }
      else if key == "is" && v == "expiring" then q := { q with filters := { f with isExpiring := flag } }
      else if key == "is" && v == "forever" then q := { q with filters := { f with isForever := flag } }
      else if key == "is" && v == "event" then q := { q with filters := { f with isEvent := flag } }
      else if key == "is" && v == "snoozed" then q := { q with filters := { f with isSnoozed := flag } }
      else if key == "is" && v == "archived" then q := { q with filters := { f with isArchived := flag } }
      else if key == "is" && (v == "toread" || v == "to-read") then q := { q with filters := { f with isToRead := flag } }
      else if key == "is" && v == "junk" then q := { q with filters := { f with isJunk := flag } }
      else if key == "in" && v == "sent" then q := { q with filters := { f with inSent := flag } }
      else if key == "in" && v == "inbox" then q := { q with filters := { f with inSent := some tok.neg } }
      else if (key == "before" || key == "after") && !tok.neg then
        match parseDay v (if key == "before" then 1 else 0) with
        | none => q := addFree q s!"{key}:{tok.value}" false
        | some t =>
          if key == "before" then
            q := { q with filters := { f with before := some (match f.before with | some b => min b t | none => t) } }
          else
            q := { q with filters := { f with after := some (match f.after with | some b => max b t | none => t) } }
      else q := addFree q s!"{key}:{tok.value}" tok.neg
  return q

/-- Constrains nothing (the UI shows the recent list instead). -/
def Query.isEmpty (q : Query) : Bool :=
  q.terms.isEmpty && q.phrases.isEmpty && q.negations.isEmpty && q.filters == {}

end Core.Query
