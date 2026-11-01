import Core.Text.Str
import Core.Text.Charset

namespace Core.Mime.Header

/-! # Header fields and structured values

A field is `name: value`, unfolded. A structured value (`Content-Type`, `Content-Disposition`) is
a token followed by `; name=value` parameters, with RFC 822 comments stripped, RFC 2231
continuations (`name*0=`, `name*1=`) joined and `name*=charset'lang'%XX` decoded. The rules are
postal-mime's, including the ones that exist because senders are sloppy. -/

open Core.Text Str

structure Field where
  /-- Lower-cased. -/
  key : String
  originalKey : String
  value : String
  deriving Inhabited, Repr

structure Structured where
  /-- Lower-cased main value, `""` when absent. -/
  value : String := ""
  /-- Parameters, lower-cased names, first occurrence wins. -/
  params : List (String × String) := []
  deriving Inhabited, Repr

def Structured.param? (s : Structured) (name : String) : Option String :=
  s.params.lookup name

def Structured.param (s : Structured) (name : String) : String :=
  (s.params.lookup name).getD ""

/-- Group folded continuation lines (SP or HTAB first) with their header, unfold with the
whitespace kept, and split at the first colon. -/
def parseLines (lines : Array String) : Array Field := Id.run do
  let mut groups : Array (Array String) := #[]
  for line in lines do
    if !groups.isEmpty && (line.startsWith " " || line.startsWith "\t") then
      groups := groups.modify (groups.size - 1) (·.push line)
    else
      groups := groups.push #[line]
  let mut fields : Array Field := #[]
  for g in groups do
    let unfolded := String.join g.toList
    let cs := unfolded.toList
    let (k, v) := match cs.span (· != ':') with
      | (k, []) => (String.ofList k, "")
      | (k, _ :: rest) => (String.ofList k, String.ofList rest)
    let key := trimWsp k
    let value := trimWsp (collapse (fun c => c == '\r' || c == '\n') " " v)
    fields := fields.push { key := key.toLower, originalKey := key, value }
  return fields

/-- Strip `(comments)`; inside a parameter value only a parenthesis after whitespace opens one. -/
def stripComments (s : String) : String := Id.run do
  let a := s.toList.toArray
  let mut result := ""
  let mut depth := 0
  let mut escaped := false
  let mut inQuote := false
  let mut inParam := false
  let mut commentStart := 0
  let mut i := 0
  while i < a.size do
    let c := a[i]!
    if escaped then
      if depth == 0 then result := result.push c
      escaped := false
      i := i + 1
      continue
    if c == '\\' then
      escaped := true
      if depth == 0 then result := result.push c
      i := i + 1
      continue
    if c == '"' && depth == 0 then
      inQuote := !inQuote
      result := result.push c
      i := i + 1
      continue
    if !inQuote then
      let opens := !inParam || result.isEmpty || result.endsWith " " || result.endsWith "\t"
      if c == '(' && opens then
        if depth == 0 then commentStart := i
        depth := depth + 1
        i := i + 1
        continue
      if c == ')' && depth > 0 then
        depth := depth - 1
        i := i + 1
        continue
      if depth == 0 then
        if c == '=' then inParam := true
        else if c == ';' then inParam := false
    if depth == 0 then result := result.push c
    i := i + 1
  if depth == 0 then return result
  -- unbalanced `(`: keep everything when a parameter still follows, else drop the dangling text
  return if (a.extract commentStart a.size).contains ';' then s else result

private def percentDecode (s : String) (charset : String) : String := Id.run do
  let a := s.toList.toArray
  let mut bytes := ByteArray.empty
  let mut i := 0
  while i < a.size do
    let c := a[i]!
    if c == '%' && i + 2 < a.size && isHex a[i+1]! && isHex a[i+2]! then
      bytes := bytes.push ((hexVal a[i+1]! * 16 + hexVal a[i+2]!).toUInt8)
      i := i + 3
    else
      bytes := bytes ++ (String.singleton c).toUTF8
      i := i + 1
  return Charset.decode charset bytes

/-- Where `/\*((\d+)\*?)?$/` matches a parameter name: the base name, the section number and
whether the section is percent-encoded (the name ends in `*`). -/
private def continuationKey? (k : String) : Option (String × Nat × Bool) := Id.run do
  let cs := k.toList.toArray
  let n := cs.size
  for i in [0 : n] do
    if cs[i]! != '*' then continue
    -- the rest must be empty, digits, or digits then `*`
    let mut j := i + 1
    while j < n && isDigit cs[j]! do j := j + 1
    let digits := slice cs (i + 1) j
    let ok := j == n || (j + 1 == n && cs[j]! == '*' && !digits.isEmpty)
    if ok then
      return some ((slice cs 0 i).toLower, digits.toNat?.getD 0, cs[n - 1]! == '*')
  return none

/-- `^([^']*)'[^']*'(.*)$`: the charset and the rest of an RFC 2231 initial section. -/
private def splitCharset? (v : String) : Option (String × String) :=
  match v.toList.span (· != '\'') with
  | (cs1, '\'' :: rest) =>
    match rest.span (· != '\'') with
    | (_, '\'' :: rest2) =>
      if rest2.any (fun c => c == '\n' || c == '\r' || c == '\u2028' || c == '\u2029') then none
      else some (String.ofList cs1, String.ofList rest2)
    | _ => none
  | _ => none

/-- Join `name*0`, `name*1*`, `name*` sections; decode the `*`-marked ones. The joined value
replaces a plain parameter of the same name, as postal-mime's assignment does. -/
private def continuations (params : List (String × String)) : List (String × String) := Id.run do
  let mut plain : Array (String × String) := #[]
  -- base name, charset, sections (nr, value, encoded), in first-seen order
  let mut groups : Array (String × Option String × Array (Nat × String × Bool)) := #[]
  for (k, v) in params do
    match continuationKey? k with
    | none => plain := plain.push (k, v)
    | some (actual, nr, encoded) =>
      let mut value := v
      let mut charset : Option String := none
      if nr == 0 && encoded then
        if let some (c, rest) := splitCharset? v then
          charset := some (if c.isEmpty then "utf-8" else c)
          value := rest
      match groups.findIdx? (·.1 == actual) with
      | some gi =>
        groups := groups.modify gi fun (key, cs0, parts) =>
          (key, (if charset.isSome then charset else cs0), parts.push (nr, value, encoded))
      | none => groups := groups.push (actual, charset, #[(nr, value, encoded)])
  let mut out := plain
  for (key, charset, parts) in groups do
    -- a stable sort by section number
    let sorted := (parts.zipIdx.qsort (fun a b => a.1.1 < b.1.1 || (a.1.1 == b.1.1 && a.2 < b.2))).map (·.1)
    let mut result := ""
    let mut pending := ""
    for (_, v, enc) in sorted do
      if enc then
        pending := pending ++ v
      else
        if !pending.isEmpty then
          result := result ++ percentDecode pending (charset.getD "utf-8")
          pending := ""
        result := result ++ v
    if !pending.isEmpty then result := result ++ percentDecode pending (charset.getD "utf-8")
    match out.findIdx? (·.1 == key) with
    | some i => out := out.set! i (key, result)
    | none => out := out.push (key, result)
  return out.toList

/-- `value; name=value; name="quoted value"`. -/
def parseStructured (raw : String) : Structured := Id.run do
  let s := stripComments raw
  let a := s.toList.toArray
  let mut value := ""
  let mut params : List (String × String) := []
  let mut key : Option String := none
  let mut cur := ""
  let mut pendingSpace := ""
  let mut quoteClosed := false
  let mut quote := false
  let mut escaped := false
  let mut stageKey := false
  let store (key : Option String) (cur : String) (value : String) (params : List (String × String)) : String × List (String × String) :=
    match key with
    | none => (cur, params)
    | some k => (value, if params.lookup k |>.isSome then params else params ++ [(k, cur)])
  for c in a.toList do
    if stageKey then
      if c == '=' then
        key := some (trimJs cur).toLower
        cur := ""
        pendingSpace := ""
        quoteClosed := false
        stageKey := false
      else if c == ';' then
        let name := (trimJs cur).toLower
        if !name.isEmpty && (params.lookup name).isNone then params := params ++ [(name, "")]
        cur := ""
      else
        cur := cur.push c
    else
      if escaped then
        if !cur.isEmpty then cur := cur ++ pendingSpace
        pendingSpace := ""
        cur := cur.push c
      else if quote && c == '\\' then
        escaped := true
        continue
      else if quote && c == '"' then
        quote := false
        quoteClosed := true
      else if !quote && c == '"' then
        quote := true
        if !cur.isEmpty then cur := cur ++ pendingSpace
        pendingSpace := ""
      else if !quote && c == ';' then
        let (v, p) := store key cur value params
        value := v
        params := p
        cur := ""
        pendingSpace := ""
        quoteClosed := false
        stageKey := true
      else if !quote && (c == ' ' || c == '\t') then
        pendingSpace := pendingSpace.push c
      else if !quoteClosed then
        if !cur.isEmpty then cur := cur ++ pendingSpace
        pendingSpace := ""
        cur := cur.push c
      escaped := false
  if stageKey then
    let name := (trimJs cur).toLower
    if !name.isEmpty && (params.lookup name).isNone then params := params ++ [(name, "")]
  else
    let (v, p) := store key cur value params
    value := v
    params := p
  return { value := value.toLower, params := continuations params }

/-- The first word of a transfer-encoding value, comments stripped, lower-cased. -/
def encodingToken (raw : String) : String :=
  let s := (stripComments raw).toLower
  String.ofList (s.toList.dropWhile (fun c => !(isWordChar c || c == '-')) |>.takeWhile (fun c => isWordChar c || c == '-'))

end Core.Mime.Header
