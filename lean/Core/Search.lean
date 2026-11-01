import Core.Query

/-! # Query → FTS5 MATCH

The MATCH string is built only from validated words, each wrapped in double quotes: FTS5 rejects a
bare term with `.` or `@` (the index tokenizer keeps `@._-` inside tokens), and quoting also makes
every FTS5 operator character inert. A word is a run of `[a-z0-9@._-]` or non-ASCII letters;
everything else, the double quote included, separates words. Bare terms of 3+ characters are
prefix matches (`"term"*`); `from: to: subject: list:` scope to their columns; negations become
`NOT`. A query with negations and nothing positive has no MATCH (FTS5's `NOT` is binary), so
`negMatch` gives the string for a `rowid NOT IN (…)` subquery instead. -/

namespace Core.Search

open Core.Query

def isFtsChar (c : Char) : Bool :=
  ('a' ≤ c && c ≤ 'z') || ('0' ≤ c && c ≤ '9') || c == '@' || c == '.' || c == '_' || c == '-' ||
    (c.toNat ≥ 0x80 && Core.Query.isWord c)

/-- Lower-cased words made of FTS characters, leading and trailing `.`/`-`/`_` trimmed. -/
def words (s : String) : Array String :=
  let trimPunct (w : String) : String :=
    let p (c : Char) := c == '.' || c == '-' || c == '_'
    String.ofList ((w.toList.dropWhile p).reverse.dropWhile p).reverse
  ((normalizeText s).split (!isFtsChar ·)).toArray.map (fun w => trimPunct w.toString) |>.filter (· ≠ "")

/-- The text the index stores for a column. The index tokenizer keeps `@._-` inside tokens, so
`maya@nordic-press.example` is one token; the query grammar (like MailVault's tokenizer) also
looks for its parts. So each word containing one of those characters is followed by its parts:
the halves around `@`, and the runs of letters and digits. A bare `nordic`, `press`, `maya`,
`nordic-press.example` or the whole address then all match. -/
def indexText (s : String) : String := Id.run do
  let mut out : Array String := #[]
  for w in words s do
    out := out.push w
    if w.any (fun c => c == '@' || c == '.' || c == '-' || c == '_') then
      if w.contains '@' then
        for part in w.splitOn "@" do
          if part.any (fun c => c == '.' || c == '-' || c == '_') then out := out.push part
      for part in (w.split (!Core.Query.isWord ·)).toList do
        let part := part.toString
        if !part.isEmpty then out := out.push part
  return " ".intercalate out.toList

/-- One word as an FTS5 string, a prefix match when long enough. -/
def term (w : String) (prefix_ : Bool := true) : String :=
  "\"" ++ w ++ "\"" ++ (if prefix_ && w.length ≥ 3 then "*" else "")

/-- Several words as one quoted phrase (prefix on the last word when asked). -/
def phrase (ws : Array String) (prefix_ : Bool := false) : Option String :=
  if ws.isEmpty then none
  else if ws.size == 1 then some (term ws[0]! prefix_)
  else some ("\"" ++ " ".intercalate ws.toList ++ "\"" ++ (if prefix_ then "*" else ""))

/-- A free term: every word of it, AND-ed (a token like `o'brien` gives two words). -/
private def freeTerm (t : String) : Option String :=
  let ws := words t
  if ws.isEmpty then none
  else if ws.size == 1 then some (term ws[0]!)
  else some ("(" ++ " AND ".intercalate (ws.toList.map (term ·)) ++ ")")

private def columnFilter (cols : String) (v : String) : Option String :=
  (phrase (words v) (prefix_ := true)).map (cols ++ ":" ++ ·)

/-- The positive clauses of the query, AND-ed in order. -/
def positives (q : Query) : Array String :=
  let f := q.filters
  q.terms.filterMap freeTerm ++
  q.phrases.filterMap (fun p => phrase (words p)) ++
  f.from_.filterMap (columnFilter "from_") ++
  f.to_.filterMap (columnFilter "{to_ cc}") ++
  f.subject.filterMap (columnFilter "subject") ++
  f.list.filterMap (columnFilter "list_id")

def negatives (q : Query) : Array String := q.negations.filterMap freeTerm

/-- The MATCH string, or `none` when nothing positive constrains the text. -/
def toFts (q : Query) : Option String :=
  let pos := positives q
  if pos.isEmpty then none
  else some (" AND ".intercalate pos.toList ++ String.join ((negatives q).toList.map (" NOT " ++ ·)))

/-- For a query with negations but no positive text: the MATCH of what to exclude. -/
def negMatch (q : Query) : Option String :=
  if !(positives q).isEmpty then none
  else
    let ns := negatives q
    if ns.isEmpty then none else some (" OR ".intercalate ns.toList)

end Core.Search
