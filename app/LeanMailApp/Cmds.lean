import LeanMailApp.Msg

/-! # Commands: what a message asks the host to do

`cmds msg m` is computed from the same pre-state `update msg m` sees (`LeanNative.Session.step`). Two rules
hold of every command list the app ever issues, and `Gate.lean` checks their proofs:

* **signed out, no API command**: a model without a session asks for no `Cmd.api` (the only commands
  that carry a credential); sign-in uses the public `/auth/*` endpoints and the Keychain only;
* **busy, no action**: while `busy`, no command changes anything on the server (`Call.isAction`:
  message actions, sending, approving and rejecting sign-ups).

`plan` is written to respect both; `guard` enforces them over whatever `plan` returns, so the proofs are
about the function that runs, not about `plan`'s many cases. Because a host answer is seen in its
pre-state, the answer that brings a credential (the boot's Keychain read, `/auth/code`'s token) cannot
load the inbox itself: the boot posts `ready`, and a new sign-in loads on the Keychain write's answer. -/

namespace LeanMailApp
open Lean

inductive Cmd where
  /-- the debug flag, the test environment and the Keychain's saved session → `Host.booted` -/
  | boot
  /-- posts `Host.ready` (the first load after a session appears) -/
  | ready
  /-- `POST <origin>/auth/login {email}` (public) -/
  | requestCode (origin email : String)
  /-- `POST <origin>/auth/code {email, code, name}` (public) → a token -/
  | redeemCode (origin email code : String)
  /-- writes the session to the Keychain -/
  | store (s : Session)
  /-- deletes it -/
  | forget
  /-- a new ULID for `/api/send`'s `client_id` (the time and random bytes from the system) -/
  | newId
  /-- one authenticated request -/
  | api (s : Session) (c : Call)
  deriving ToJson, Inhabited

/-- A command that carries a credential. -/
def Cmd.isApi : Cmd → Bool
  | .api .. => true
  | _ => false

/-- A command that changes something on the server. -/
def Cmd.isAction : Cmd → Bool
  | .api _ c => c.isAction
  | _ => false

/-- The list the model shows: its search, else its filter. -/
def listCall (m : Model) (before : Option String) : Call :=
  if m.query.isEmpty then .list (filterName m.filter) before else .search m.query before

/-- What the first screen of a session needs. -/
def loads (s : Session) (m : Model) : List Cmd :=
  [.api s (listCall m none), .api s .account, .api s .signups]

/-- The session a sign-in would start from what the person entered, if any. -/
def enteredSession (m : Model) : Option Session :=
  let origin := normalizeOrigin m.originInput
  if m.debug && m.devAccount then some { origin, cred := .dev }
  else if m.tokenMode then
    let t := trim m.tokenInput
    if t.startsWith "lm1" then some { origin, cred := .token t } else none
  else none

/-- The saved (or test) session the boot answer brings. -/
def bootSession (j : Json) : Option Session :=
  match (j.getObjValAs? Session "saved").toOption with
  | some s => some s
  | none => none

/-- Every command, before `guard`. -/
def plan : Msg → Model → List Cmd
  | .sendCode, m =>
    if m.busy || m.signedIn || (trim m.email).isEmpty then [] else [.requestCode (normalizeOrigin m.originInput) (trim m.email)]
  | .redeem, m =>
    if m.busy || m.signedIn || (trim m.code).isEmpty then []
    else [.redeemCode (normalizeOrigin m.originInput) (trim m.email) (trim m.code)]
  | .signIn, m => if m.busy || m.signedIn then [] else match enteredSession m with
    | some s => [.store s]
    | none => []
  | .host (.booted j), _ => if (bootSession j).isSome then [.ready] else []
  | .host (.redeemed t e), m =>
    if e.isEmpty && !t.isEmpty then [.store { origin := normalizeOrigin m.originInput, cred := .token t }] else []
  | .host (.stored _ _), m => match m.session with
    | some s => loads s m
    | none => []
  | .host .ready, m => match m.session with
    | some s => loads s m
    | none => []
  | .filter i, m => match m.session with
    | some s => if m.busy then [] else [.api s (.list (filterName i) none)]
    | none => []
  | .runSearch, m => match m.session with
    | some s =>
      if m.busy then [] else
      let q := trim m.search
      [.api s (if q.isEmpty then .list (filterName m.filter) none else .search q none)]
    | none => []
  | .clearSearch, m => match m.session with
    | some s => if m.busy then [] else [.api s (.list (filterName m.filter) none)]
    | none => []
  | .refresh, m => match m.session with
    | some s => if m.busy then [] else [.api s (listCall m none)]
    | none => []
  | .loadMore, m => match m.session, m.next with
    | some s, some n => if m.busy then [] else [.api s (listCall m (some n))]
    | _, _ => []
  | .select id, m => match m.session with
    | some s => if m.busy then [] else [.api s (.message id)]
    | none => []
  | .host (.answer (.message _) 200 j _ _), m => match m.session with
    | some s => let t := str j "thread_id"; if t.isEmpty then [] else [.api s (.thread t)]
    | none => []
  | .act a, m => match m.session, m.detail with
    | some s, some d => if m.busy then [] else [.api s (.act d.item.id a)]
    | _, _ => []
  | .openHtml, m => match m.session, m.detail with
    | some s, some d => if m.busy then [] else [.api s (.html d.item.id)]
    | _, _ => []
  | .download idx, m => match m.session, m.detail with
    | some s, some d =>
      match d.attachments.find? (·.idx == idx) with
      | some a => if m.busy then [] else [.api s (.attachment d.item.id idx a.filename)]
      | none => []
    | _, _ => []
  | .compose, m => if m.signedIn then [.newId] else []
  | .reply, m => if m.signedIn && m.detail.isSome then [.newId] else []
  | .send, m => match m.session, m.compose with
    | some s, some d =>
      if m.busy || d.clientId.isEmpty || (trim d.to).isEmpty then [] else [.api s (.send d.clientId d)]
    | _, _ => []
  | .settings, m => match m.session with
    | some s => [.api s .account]
    | none => []
  | .admin, m => match m.session with
    | some s => [.api s .signups]
    | none => []
  | .approve id, m => match m.session with
    | some s => if m.busy then [] else [.api s (.approve id)]
    | none => []
  | .reject id, m => match m.session with
    | some s => if m.busy then [] else [.api s (.reject id)]
    | none => []
  | .host (.answer (.approve _) _ _ _ _), m | .host (.answer (.reject _) _ _ _ _), m => match m.session with
    | some s => [.api s .signups]
    | none => []
  | .signOut, m => if m.signedIn then [.forget] else []
  | _, _ => []

/-- The two rules, enforced over any command list. -/
def guard (m : Model) (cs : List Cmd) : List Cmd :=
  let cs := if m.session.isNone then cs.filter (fun c => !c.isApi) else cs
  if m.busy then cs.filter (fun c => !c.isAction) else cs

def cmds (msg : Msg) (m : Model) : List Cmd := guard m (plan msg m)

end LeanMailApp
