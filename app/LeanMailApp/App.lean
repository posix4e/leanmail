import LeanNative.Cmd
import LeanMailApp.Update
import LeanMailApp.View

/-! # The app's pure half: `init`, `update`, `view`, `cmds`, `host`

This is the `CmdApp` the native runtime steps (`LeanNative.Session.step`) and the proofs (`LeanMailApp.Proofs`,
`Gate.lean`) are about. `LeanMailApp.Perform` adds the effects. -/

namespace LeanMailApp
open LeanNative

def pureApp : CmdApp Model Msg Cmd Host :=
  { init := {}, update, view, cmds, host := Msg.host }

end LeanMailApp
