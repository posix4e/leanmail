import LeanMailApp.View.Common

namespace LeanMailApp.View
open ElmLean

def inbox (m : Model) : Html Msg :=
  let title := if m.query.isEmpty then filterLabel m.filter else "Search"
  let controls := .section "" ""
    ([ .identified "filter" (.picker "Show" filterLabels m.filter Msg.filter)
     , .identified "search" (.input .plain "Search" "Search mail" m.search Msg.search (some .runSearch)) ] ++
     (if m.query.isEmpty then [] else [.identified "clear-search" (.button s!"Clear search “{m.query}”" .plain (some .clearSearch))]))
  let status := spinner m "Loading…" ++ errorText m "inbox-error" ++ noticeText m "inbox-notice"
  let rows :=
    if m.items.isEmpty && !m.busy && m.error.isEmpty then
      [.section "" "" [.identified "empty" (.text (if m.query.isEmpty then "Nothing here." else "No messages match.") .footnote)]]
    else if m.items.isEmpty then []
    else [.section (if m.query.isEmpty then "Messages" else s!"Results for “{m.query}”") "" (m.items.map row)]
  let more := match m.next with
    | some _ => if m.items.isEmpty then [] else [.section "" "" [act m "load-more" "Load more" .plain .loadMore]]
    | none => []
  .screen title none
    [ .toolbar [ .identified "settings" (.button "Settings" .plain (some .settings))
               , .identified "compose" (.button "Compose" .plain (some .compose)) ]
    , .list ([controls] ++ (if status.isEmpty then [] else [.section "" "" status]) ++ rows ++ more) ]
    (onRefresh := some .refresh)

end LeanMailApp.View
