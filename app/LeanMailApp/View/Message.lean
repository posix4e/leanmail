import LeanMailApp.View.Common

namespace LeanMailApp.View
open ElmLean

def fileName (path : String) : String := ((path.splitOn "/").getLast?).getD path

def message (m : Model) : Html Msg :=
  let base := (m.items.find? (·.id == m.opened)).getD { id := m.opened }
  let item := (m.detail.map (·.item)).getD base
  let headers := .section "" ""
    ([ .identified "message-from" (.labeled "From" (if item.fromName.isEmpty then item.fromAddr else s!"{item.fromName} <{item.fromAddr}>")) ] ++
     (match m.detail with
      | some d => (if d.to.isEmpty then [] else [.labeled "To" (", ".intercalate d.to)]) ++
                  (if d.cc.isEmpty then [] else [.labeled "Cc" (", ".intercalate d.cc)])
      | none => []) ++
     [ .labeled "Date" (formatDate item.receivedAt) ] ++
     (if item.markers.isEmpty then [] else [.labeled "Marked" (", ".intercalate item.markers)]))
  let body := match m.detail with
    | some d => [.section "Message" (if d.truncated then "The text is cut at 32 KiB; the formatted version has it all." else "")
                  [.identified "message-body" (.text (if d.text.isEmpty then "(no text part)" else d.text) .body)]]
    | none => []
  let formatted := match m.detail with
    | some _ => [.section "Formatted" "The HTML part, as a file: Quick Look or another app opens it."
        [ if m.htmlPath.isEmpty then act m "open-html" "Fetch the HTML version" .plain .openHtml
          else .identified "share-html" (.share "Open the HTML version" m.htmlPath (file := true)) ]]
    | none => []
  let attachments := match m.detail with
    | some d => if d.attachments.isEmpty then [] else
      [.section "Attachments" "" (d.attachments.map fun a =>
        match m.files.find? (·.1 == a.idx) with
        | some (_, path) => .identified s!"share-attachment-{a.idx}" (.share s!"Open {a.filename}" path (file := true))
        | none => act m s!"attachment-{a.idx}" s!"{if a.filename.isEmpty then "attachment" else a.filename} · {formatSize a.size}" .plain (.download a.idx))]
    | none => []
  let thread := if m.thread.length ≤ 1 then [] else [.section "Thread" "" (m.thread.map row)]
  let actions := match m.detail with
    | some _ => [.section "Actions" ""
        [ act m "action-snooze" "Snooze…" .plain (.snoozeMenu true)
        , act m "action-toread" (if item.toRead then "Not to read" else "To read") .plain (.act (if item.toRead then "untoread" else "toread"))
        , act m "action-junk" (if item.junk then "Not junk" else "Junk") .destructive (.act (if item.junk then "notjunk" else "junk"))
        , act m "action-forever" "Keep forever" .plain (.act "forever") ]]
    | none => []
  let status := spinner m "Opening…" ++ errorText m "message-error" ++ noticeText m "message-notice"
  .screen item.title (some .back)
    ([ .toolbar [ .identified "reply" (.button "Reply" .plain (if m.detail.isSome then some .reply else none))
                , act m "action-done" (if item.archived then "Inbox" else "Done") .primary (.act (if item.archived then "inbox" else "done")) ]
     , .list ([headers] ++ (if status.isEmpty then [] else [.section "" "" status]) ++ body ++ attachments ++ formatted ++ thread ++ actions) ] ++
     (if m.snoozing then
        [.alert "Snooze" "Until when?"
          [ .identified "snooze-1d" (.button "Tomorrow" .plain (some (.act "snooze_1d")))
          , .identified "snooze-3d" (.button "In 3 days" .plain (some (.act "snooze_3d")))
          , .identified "snooze-week" (.button "Next week" .plain (some (.act "snooze_week")))
          , .button "Cancel" .cancel (some (.snoozeMenu false)) ] (style := .actionSheet)]
      else []))
    (largeTitle := false) (depth := 1)

end LeanMailApp.View
