import LeanNative.Html
import LeanMailApp.Msg

namespace LeanMailApp.View
open LeanNative

/-- The last error, in red, or nothing. -/
def errorText (m : Model) (id : String) : List (Html Msg) :=
  if m.error.isEmpty then [] else [.identified id ((Html.text m.error .footnote).tone .negative)]

def noticeText (m : Model) (id : String) : List (Html Msg) :=
  if m.notice.isEmpty then [] else [.identified id ((Html.text m.notice .footnote).tone .positive)]

def spinner (m : Model) (label : String) : List (Html Msg) := if m.busy then [.progress label] else []

/-- A button that is disabled while `busy`. -/
def act (m : Model) (id label : String) (role : ButtonRole) (msg : Msg) : Html Msg :=
  .identified id (.button label role (if m.busy then none else some msg))

/-- One mail row: the subject as the button (its whole row), then sender, date, markers and snippet. -/
def row (i : Item) : Html Msg :=
  let info := " · ".intercalate ([i.sender, formatDate i.receivedAt] ++ i.markers)
  .col [ .identified s!"msg-{i.id}" (Html.navigates (.button i.title .plain (some (.select i.id))))
       , .text info .caption
       , .text (i.snippet.take 160).toString .caption ]

end LeanMailApp.View
