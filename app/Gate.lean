import ElmLean.Gate
import LeanMailApp.Proofs

/-! LeanMail's proof gate (`lake build` builds it; `framework/scripts/check-axioms.sh` counts its lines). -/

#assert_axioms LeanMailApp.cmds_signedOut
#assert_axioms LeanMailApp.cmds_busy
#assert_axioms LeanMailApp.run_signedOut
#assert_axioms LeanMailApp.run_busy
#assert_all_axioms LeanMailApp
