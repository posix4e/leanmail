import LeanMailApp.Cmds

/-! # `update`: the next model

Every guard here is the one `plan` (Cmds.lean) uses for the same message, so a message that asks for
nothing changes nothing that would leave the model waiting for an answer. -/

namespace LeanMailApp
open Lean

/-- What a failed answer says to the person. -/
def failure (status : Nat) (body : Json) (error : String) : String :=
  if !error.isEmpty then error
  else if status == 401 then "The server refused the credential (401). Sign out and sign in again."
  else
    let e := str body "error"
    if e.isEmpty then s!"The server answered {status}." else s!"{e} ({status})"

def actionLabel : String → String
  | "done" => "Done" | "snooze_1d" => "Snoozed until tomorrow" | "snooze_3d" => "Snoozed for 3 days"
  | "snooze_week" => "Snoozed until next week" | "toread" => "Marked to read" | "untoread" => "No longer to read"
  | "junk" => "Moved to junk" | "notjunk" => "Not junk" | "forever" => "Kept forever" | "inbox" => "Moved to the inbox"
  | a => a

/-- A reply's draft: to the Reply-To (else the sender), `Re:` once, the original quoted. -/
def replyDraft (d : Detail) : Draft :=
  let i := d.item
  let subject := if i.subject.toLower.startsWith "re:" then i.subject else s!"Re: {i.subject}"
  let quoted := "\n".intercalate ((d.text.take 4000).toString.splitOn "\n" |>.map ("> " ++ ·))
  { to := if d.replyTo.isEmpty then i.fromAddr else d.replyTo, subject
    body := s!"\n\nOn {formatDate i.receivedAt}, {i.sender} wrote:\n{quoted}\n", replyOf := some i.id }

/-- Whether a list answer is for the list the model shows now. -/
def forCurrentList (m : Model) : Call → Bool
  | .list f _ => m.query.isEmpty && f == filterName m.filter
  | .search q _ => q == m.query
  | _ => false

def withDraft (m : Model) (f : Draft → Draft) : Model :=
  { m with compose := m.compose.map f }

/-- An answer to an API call. -/
def onAnswer (m : Model) (call : Call) (status : Nat) (body : Json) (file : String) (error : String) : Model :=
  let ok := error.isEmpty && status == 200
  match call with
  | .list _ before | .search _ before =>
    if !forCurrentList m call then m
    else if !ok then { m with busy := false, error := failure status body error }
    else
      let got := itemsOf body
      { m with busy := false, error := "",
               items := if before.isNone then got else m.items ++ got,
               next := (body.getObjValAs? String "next").toOption }
  | .message id =>
    if id != m.opened then m
    else if !ok then { m with busy := false, error := failure status body error }
    else { m with busy := false, error := "", detail := some (detailOf body) }
  | .thread id =>
    if ok && (m.detail.map (·.item.threadId)) == some id then { m with thread := itemsOf body } else m
  | .html id =>
    if id != m.opened then m
    else if status == 404 && error.isEmpty then { m with busy := false, error := "This message has no HTML part." }
    else if !ok || file.isEmpty then { m with busy := false, error := failure status body error }
    else { m with busy := false, error := "", htmlPath := file }
  | .attachment id idx _ =>
    if id != m.opened then m
    else if !ok || file.isEmpty then { m with busy := false, error := failure status body error }
    else { m with busy := false, error := "", files := (idx, file) :: m.files.filter (·.1 != idx) }
  | .account =>
    if ok then { m with accountEmail := str body "email", isAdmin := m.isAdmin || flag body "is_admin" } else m
  | .signups =>
    if ok then { m with isAdmin := true, signups := (arr body "items").map signupOf }
    else if m.screen == .admin then { m with error := failure status body error } else m
  | .act id action =>
    if !ok then { m with busy := false, error := failure status body error }
    else
      let it := itemOf body
      let subject := it.title
      if leavesList action then
        { m with busy := false, error := "", items := m.items.filter (·.id != id), screen := .inbox,
                 detail := none, opened := "", thread := [], notice := s!"{actionLabel action}: {subject}" }
      else
        { m with busy := false, error := "", notice := s!"{actionLabel action}: {subject}",
                 items := m.items.map (fun x => if x.id == id then it else x),
                 detail := m.detail.map fun d => if d.item.id == id then { d with item := it } else d }
  | .send _ _ =>
    if !ok then { m with busy := false, error := failure status body error }
    else if str body "send_status" == "sent" then { m with busy := false, error := "", compose := none, notice := "Sent." }
    else
      let why := str body "send_error"
      { m with busy := false,
               error := s!"Not delivered{if why.isEmpty then "" else s!" ({why})"}. The copy is in Sent; Send again retries the same message." }
  | .approve _ | .reject _ =>
    if !ok then { m with busy := false, error := failure status body error }
    else { m with busy := false, error := "", notice := if call matches .approve _ then "Approved." else "Rejected." }

def update : Msg → Model → Model
  | .originInput s, m => { m with originInput := s }
  | .email s, m => { m with email := s, error := "" }
  | .code s, m => { m with code := s, error := "" }
  | .tokenInput s, m => { m with tokenInput := s, error := "" }
  | .tokenMode b, m => { m with tokenMode := b, error := "" }
  | .devAccount b, m => { m with devAccount := b && m.debug, error := "" }
  | .sendCode, m =>
    if m.busy || m.signedIn || (trim m.email).isEmpty then m else { m with busy := true, error := "", notice := "" }
  | .redeem, m =>
    if m.busy || m.signedIn || (trim m.code).isEmpty then m else { m with busy := true, error := "" }
  | .signIn, m =>
    if m.busy || m.signedIn then m else match enteredSession m with
    | some s => { m with session := some s, busy := true, error := "", screen := .inbox }
    | none => { m with error := if m.tokenMode then "Paste an API token: it starts with lm1." else "Enter your email first." }
  | .restart, m => { m with codeSent := false, code := "", error := "", notice := "" }
  | .host (.booted j), m =>
    let saved := bootSession j
    let testOrigin := str j "testOrigin"
    let testToken := str j "testToken"
    { m with booted := true, debug := flag j "debug", session := saved, busy := saved.isSome,
             originInput := if !testOrigin.isEmpty then testOrigin else (saved.map (·.origin)).getD m.originInput,
             tokenInput := testToken, tokenMode := !testToken.isEmpty, error := str j "error" }
  | .host .ready, m => if m.signedIn then { m with busy := true, screen := .inbox } else m
  | .host (.codeSent ok e), m =>
    if ok then { m with busy := false, codeSent := true, error := "", notice := "Check your mail for the 6-digit code." }
    else { m with busy := false, error := e }
  | .host (.redeemed t e), m =>
    if e.isEmpty && !t.isEmpty then
      { m with session := some { origin := normalizeOrigin m.originInput, cred := .token t }, busy := true, error := "",
               code := "", codeSent := false, notice := "", screen := .inbox }
    else { m with busy := false, error := if e.isEmpty then "The server sent no token." else e }
  | .host (.stored ok e), m =>
    { m with busy := m.signedIn,
             notice := if ok then m.notice else s!"Signed in for this run only: the Keychain refused the session ({e})." }
  | .host (.newId id), m => withDraft m fun d => if d.clientId.isEmpty then { d with clientId := id } else d
  | .host (.answer call status body file e), m => onAnswer m call status body file e
  | .filter i, m =>
    if m.busy || !m.signedIn then m
    else { m with filter := i, query := "", search := "", items := [], next := none, busy := true, error := "", notice := "" }
  | .search s, m => { m with search := s }
  | .runSearch, m =>
    if m.busy || !m.signedIn then m
    else { m with query := trim m.search, items := [], next := none, busy := true, error := "", notice := "" }
  | .clearSearch, m =>
    if m.busy || !m.signedIn then m
    else { m with search := "", query := "", items := [], next := none, busy := true, error := "", notice := "" }
  | .refresh, m => if m.busy || !m.signedIn then m else { m with busy := true, error := "", notice := "" }
  | .loadMore, m => if m.busy || !m.signedIn || m.next.isNone then m else { m with busy := true, error := "" }
  | .select id, m =>
    if m.busy || !m.signedIn then m
    else { m with opened := id, screen := .message, detail := none, thread := [], htmlPath := "", files := [],
                  snoozing := false, busy := true, error := "", notice := "" }
  | .back, m =>
    match m.screen with
    | .admin => { m with screen := .settings, error := "", notice := "" }
    | _ => { m with screen := .inbox, detail := none, opened := "", thread := [], htmlPath := "", files := [],
                    snoozing := false, error := "" }
  | .act _, m => if m.busy || !m.signedIn || m.detail.isNone then m else { m with busy := true, snoozing := false, error := "" }
  | .snoozeMenu b, m => { m with snoozing := b }
  | .openHtml, m => if m.busy || !m.signedIn || m.detail.isNone then m else { m with busy := true, error := "" }
  | .download idx, m =>
    if m.busy || !m.signedIn then m else match m.detail with
    | some d => if (d.attachments.find? (·.idx == idx)).isSome then { m with busy := true, error := "" } else m
    | none => m
  | .dismissNotice, m => { m with notice := "" }
  | .compose, m => if m.signedIn then { m with compose := some {}, error := "" } else m
  | .reply, m => match m.detail with
    | some d => if m.signedIn then { m with compose := some (replyDraft d), error := "" } else m
    | none => m
  | .composeFrom s, m => withDraft m ({ · with from_ := s })
  | .composeTo s, m => withDraft m ({ · with to := s })
  | .composeCc s, m => withDraft m ({ · with cc := s })
  | .composeSubject s, m => withDraft m ({ · with subject := s })
  | .composeBody s, m => withDraft m ({ · with body := s })
  | .send, m => match m.compose with
    | some d =>
      if m.busy || !m.signedIn then m
      else if (trim d.to).isEmpty then { m with error := "Add a recipient." }
      else if d.clientId.isEmpty then m
      else { m with busy := true, error := "" }
    | none => m
  | .cancelCompose, m => { m with compose := none, error := "" }
  | .settings, m => if m.signedIn then { m with screen := .settings, error := "", notice := "" } else m
  | .admin, m => if m.signedIn then { m with screen := .admin, error := "", notice := "" } else m
  | .approve _, m | .reject _, m => if m.busy || !m.signedIn then m else { m with busy := true, error := "" }
  | .signOut, m =>
    if !m.signedIn then m
    else
      let fresh : Model := {}
      { fresh with booted := true, debug := m.debug, notice := "Signed out.",
                   originInput := (m.session.map (·.origin)).getD m.originInput }

end LeanMailApp
