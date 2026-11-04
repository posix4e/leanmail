import LeanMailApp.View.Common

namespace LeanMailApp.View
open LeanNative

def compose (m : Model) (d : Draft) : Html Msg :=
  .sheet (if d.replyOf.isSome then "Reply" else "New message") (some .cancelCompose)
    [ .list
      [ .section "" ""
          [ .identified "compose-from" (.input .plain "From" "your default address" d.from_ Msg.composeFrom none)
          , .identified "compose-to" (.input .plain "To" "name@example.com, …" d.to Msg.composeTo none)
          , .identified "compose-cc" (.input .plain "Cc" "" d.cc Msg.composeCc none)
          , .identified "compose-subject" (.input .plain "Subject" "" d.subject Msg.composeSubject none) ]
      , .section "" "" [ .identified "compose-body" (.input .multiline "Message" "" d.body Msg.composeBody none) ]
      , .section "" ""
          ([ .identified "compose-send" (.button "Send" .primary
               (if m.busy || d.clientId.isEmpty || (trim d.to).isEmpty then none else some .send))
           , .identified "compose-cancel" (.button "Cancel" .cancel (some .cancelCompose)) ] ++
           spinner m "Sending…" ++ errorText m "compose-error") ] ]
    (size := .large)

end LeanMailApp.View
