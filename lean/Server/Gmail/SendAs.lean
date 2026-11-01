import Lean.Data.Json
import Server.Gmail.Client
import Server.Gmail.OAuth

/-! # Gmail: sending as the connected address

A port of MailVault's `src/gmail/send.ts`. Mail from the connected Gmail address, or from an
alias Gmail reports as verified (`settings.sendAs`, cached in `settings` at connect time and on
the daily tick), can only be sent through Gmail. `Server.Send` stores the Sent copy first, as for
any send, then hands the whole message (with its `Bcc`, which Gmail reads and strips) to
`users.messages.send`, in the Gmail thread of the message it answers when there is one. The new
Gmail id goes on the Sent copy's `gm_msgid`, so the poll sees Gmail's copy as a duplicate. -/

namespace Server.Gmail.SendAs

open Lean Server.Db Server.Gmail

/-- Every address the account can send as through Gmail, lower-cased. -/
def identities (st : State) : IO (List String) := do
  let some c ← conn? st | return []
  if !c.authorized then return []
  let aliases : List String := match ← setting? st OAuth.sendAsKey with
    | some s => match Json.parse s with
      | .ok (.arr xs) => xs.toList.filterMap fun | .str a => some a.toLower | _ => none
      | _ => []
    | none => []
  return (c.email.toList ++ aliases).eraseDups

def isIdentity (st : State) (addr : String) : IO Bool := do
  return (← identities st).contains addr.toLower

/-- Send `raw` through Gmail: the new Gmail message id, or the error code. -/
def send (st : State) (raw : ByteArray) (replyOf : Option String) : IO (Except String String) := do
  try
    let tok ← OAuth.accessToken st
    let threadId ← match replyOf with
      | some pid => do
        match (← st.db.queryOne "SELECT gm_msgid FROM messages WHERE id = ?" #[pid]).bind (·.text? "gm_msgid") with
        | some gm => threadIdOf st tok gm
        | none => pure none
      | none => pure none
    let (id, _) ← sendRaw st tok raw threadId
    Log.info "gmail.sent" [("threaded", Json.bool threadId.isSome)]
    return .ok id
  catch e =>
    let code := codeOf e
    Log.warn "gmail.send_failed" [("code", Json.str code)]
    return .error code

end Server.Gmail.SendAs
