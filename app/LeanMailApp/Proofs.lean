import ElmLean.Proofs
import LeanMailApp.App

/-! # What LeanMail proves about its commands

1. **Signed out, no API command** (`cmds_signedOut`): a model without a session asks for no command
   that carries a credential (`Cmd.isApi`), whatever the message.
2. **Busy, no action** (`cmds_busy`): while `busy`, no command changes anything on the server
   (`Cmd.isAction`: message actions, sending, approving or rejecting a sign-up).

Both are lifted to every step of every run, any interleaving of user events and host answers, through
the framework's `CmdSpec.trace_origin` (`run_signedOut`, `run_busy`). -/

namespace LeanMailApp
open ElmLean

theorem guard_signedOut (m : Model) (cs : List Cmd) (h : m.session = none) :
    ∀ c ∈ guard m cs, c.isApi = false := by
  intro c hc
  unfold guard at hc
  simp only [h, Option.isNone_none, ite_true] at hc
  split at hc
  · simp only [List.mem_filter] at hc; simpa using hc.1.2
  · simp only [List.mem_filter] at hc; simpa using hc.2

theorem guard_busy (m : Model) (cs : List Cmd) (h : m.busy = true) :
    ∀ c ∈ guard m cs, c.isAction = false := by
  intro c hc
  unfold guard at hc
  simp only [h, ite_true, List.mem_filter] at hc
  simpa using hc.2

theorem cmds_signedOut (msg : Msg) (m : Model) (h : m.session = none) : ∀ c ∈ cmds msg m, c.isApi = false :=
  guard_signedOut m _ h

theorem cmds_busy (msg : Msg) (m : Model) (h : m.busy = true) : ∀ c ∈ cmds msg m, c.isAction = false :=
  guard_busy m _ h

/-- The app's `CmdSpec`: no further invariant is needed for the two rules. -/
def spec : CmdSpec pureApp where
  Inv := fun _ => True
  Target := fun _ _ => True
  init_inv := trivial
  update_inv := fun _ _ _ => trivial
  view_sat := fun _ _ => Html.Sat.of_forall (fun _ => trivial) _

/-- Over any run: a step taken from a signed-out session asks for no API command. -/
theorem run_signedOut (is : List (Input Host)) (k : Nat) (st : Step Model Msg Cmd Host)
    (h : (pureApp.traceFrom is)[k]? = some st) (hs : st.before.model.session = none) :
    ∀ c ∈ st.cmds, c.isApi = false := by
  obtain ⟨_, _, _, o⟩ := spec.trace_origin is k st h
  rcases o with e | ⟨msg, e, _⟩
  · simp [e]
  · rw [e]; exact cmds_signedOut msg _ hs

/-- Over any run: a step taken from a busy session asks for no action. -/
theorem run_busy (is : List (Input Host)) (k : Nat) (st : Step Model Msg Cmd Host)
    (h : (pureApp.traceFrom is)[k]? = some st) (hb : st.before.model.busy = true) :
    ∀ c ∈ st.cmds, c.isAction = false := by
  obtain ⟨_, _, _, o⟩ := spec.trace_origin is k st h
  rcases o with e | ⟨msg, e, _⟩
  · simp [e]
  · rw [e]; exact cmds_busy msg _ hb

end LeanMailApp
