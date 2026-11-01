import Server.Pages.Common
import Server.Api
import Server.Search

/-! # Pages: the inbox, its filters, and search

`/inbox[?filter=…&before=…]` uses the same predicates as `GET /api/msgs` (`Api.filterClauses`),
grouped by day, newest first, paged by the keyset cursor. `/search?q=…` runs the query grammar;
FTS results page by offset, filter-only queries by cursor. -/

namespace Server.Pages

open Lean Core.Html Server.Db Server.Router

def filterTabs : List (String × String) := [
  ("inbox", "Inbox"), ("toread", "To read"), ("snoozed", "Snoozed"), ("archived", "Done"),
  ("sent", "Sent"), ("junk", "Junk"), ("all", "All")]

def tabsNav (current : String) : Node :=
  el "nav" [("class", "tabs"), ("aria-label", "Views")] <| (filterTabs.map fun (k, label) =>
    let attrs := [("href", if k == "inbox" then "/inbox" else href "/inbox" [("filter", k)])]
    elt "a" (if k == current then attrs ++ [("class", "on"), ("aria-current", "page")] else attrs) label) ++
    [elt "a" [("href", "/drafts"), ("class", "tab-aux")] "Drafts",
     elt "a" [("href", "/compose"), ("class", "tab-aux compose-link")] "Compose"]

def fromLabel (r : Row) : String :=
  let n := r.text "from_name"
  if n.isEmpty then (let a := r.text "from_addr"; if a.isEmpty then "(unknown sender)" else a) else n

def badge (cls label : String) : Node := elt "span" [("class", s!"badge {cls}")] label

/-- One list row: a link to the message, with markers for attachments, invites and state. -/
def listRow (now : Nat) (r : Row) : Node :=
  let id := r.text "id"
  let subj := r.text "subject"
  let ts := r.nat "received_at"
  let marks : List Node :=
    (if r.bool "has_attachments" then [badge "att" "Attachment"] else []) ++
    (if r.bool "has_invite" then [badge "inv" "Invite"] else []) ++
    (if (r.nat? "to_read_at").isSome then [badge "toread" "To read"] else []) ++
    (if (r.nat? "snoozed_until").any (· > now) then [badge "snz" "Snoozed"] else []) ++
    (if (r.nat? "junk_at").isSome then [badge "junk" "Junk"] else []) ++
    -- the optional AI's triage (`Api.withTriage`), a small chip
    ((r.text? "triage_label").map fun l =>
      [elt "span" [("class", s!"badge ai ai-{l}"), ("title", s!"AI triage: {l.replace "_" " "}, priority {r.nat "triage_priority"}")]
        (l.replace "_" " ")]).getD []
  el "li" [("class", "row"), ("data-id", id)] [
    el "a" [("href", s!"/m/{id}"), ("class", "row-link")] [
      el "span" [("class", "row-top")] [
        elt "span" [("class", "from")] (fromLabel r),
        timeEl ts (shortTime ts now)],
      elt "span" [("class", "subj")] (if subj.isEmpty then "(no subject)" else subj),
      el "span" [("class", "row-bottom")] ([elt "span" [("class", "snip")] (r.text "snippet")] ++
        (if marks.isEmpty then [] else [el "span" [("class", "marks")] marks]))]]

/-- Rows grouped under day headings, in their given order. -/
def groupedList (now : Nat) (rows : Array Row) : Node := Id.run do
  let mut groups : Array (String × Array Row) := #[]
  for r in rows do
    let label := dayLabel (r.nat "received_at") now
    match groups.back? with
    | some (l, rs) => if l == label then groups := groups.pop.push (l, rs.push r) else groups := groups.push (label, #[r])
    | none => groups := groups.push (label, #[r])
  return el "div" [("class", "list"), ("id", "list")] <| groups.toList.map fun (label, rs) =>
    el "section" [("class", "day")] [
      elt "h2" [("class", "day-h")] label,
      el "ul" [("class", "rows")] (rs.toList.map (listRow now))]

def emptyNote (msg : String) : Node := elt "p" [("class", "empty")] msg

def inbox (p : Page) : IO Resp := do
  let now ← Clock.nowMs
  let req := p.req
  let filter := (req.param? "filter").getD "inbox"
  let some (cs, args) := Api.filterClauses filter now | return errorPage 400 (some p.me)
  let (cs, args) := if filter == "inbox" then (cs.push "m.folder = ?", args.push (.text "inbox")) else (cs, args)
  let pg ← Search.runQuery p.c.st {} 50 0 (req.param? "before") (cs, args)
  let pg := { pg with rows := ← Api.withTriage p.c.st pg.rows }
  let title := ((filterTabs.lookup filter).getD "Inbox")
  let older : Node := match pg.next with
    | some n => el "nav" [("class", "pager")] [elt "a" [("href", href "/inbox" [("filter", if filter == "inbox" then "" else filter), ("before", n)]), ("rel", "next")] "Older"]
    | none => empty
  return page p title (pageKind := "list") [
    tabsNav filter,
    if pg.rows.isEmpty then emptyNote (if (req.param? "before").isSome then "No older mail." else "Nothing here.")
    else groupedList now pg.rows,
    older]

def search (p : Page) : IO Resp := do
  let now ← Clock.nowMs
  let req := p.req
  let q := Core.Text.Str.trim ((req.param? "q").getD "")
  if q.isEmpty then
    return page p "Search" (pageKind := "list") [
      elt "h1" [] "Search",
      el "p" [("class", "hint")] [txt "Try ", elt "code" [] "from:alex", txt ", ", elt "code" [] "subject:\"logo\"",
        txt ", ", elt "code" [] "has:attachment", txt ", ", elt "code" [] "before:2026-09-01", txt " or ",
        elt "code" [] "-word", txt "."]]
  let offset := req.natParam "offset" 0
  let pg ← Search.runQuery p.c.st (Core.Query.parse q) 50 offset (req.param? "before")
  let pg := { pg with rows := ← Api.withTriage p.c.st pg.rows }
  let older : Node := match pg.next with
    | some n =>
      let link := if (Search.parseCursor n).isSome then href "/search" [("q", q), ("before", n)]
        else href "/search" [("q", q), ("offset", n)]
      el "nav" [("class", "pager")] [elt "a" [("href", link), ("rel", "next")] "More results"]
    | none => empty
  return page p s!"Search: {q}" (pageKind := "list") [
    el "h1" [("class", "search-h")] [txt "Results for ", elt "q" [] q],
    if pg.rows.isEmpty then emptyNote "No mail matches." else groupedList now pg.rows,
    older]

end Server.Pages
