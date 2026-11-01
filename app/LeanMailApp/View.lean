import LeanMailApp.View.Signin
import LeanMailApp.View.Inbox
import LeanMailApp.View.Message
import LeanMailApp.View.Compose
import LeanMailApp.View.Settings

namespace LeanMailApp
open ElmLean

def view (m : Model) : Html Msg :=
  if !m.booted then .screen "LeanMail" none [.progress "Opening…"]
  else if !m.signedIn then View.signin m
  else
    let main := match m.screen with
      | .inbox => View.inbox m
      | .message => View.message m
      | .settings => View.settings m
      | .admin => View.admin m
    match m.compose, main with
    | some d, .screen t b cs r lt dp => .screen t b (cs ++ [View.compose m d]) r lt dp
    | _, h => h

end LeanMailApp
