import LeanMailApp.Api

/-! # Messages: what the person does (`Msg`) and what the host answers (`Host`)

A host answer reaches `update` only as `Msg.host`. -/

namespace LeanMailApp
open Lean

inductive Host where
  /-- the boot command: `{debug, saved?: Session, testOrigin?, testToken?}` -/
  | booted (j : Json)
  /-- posted by `Cmd.ready` once a session is in the model -/
  | ready
  /-- `POST /auth/login` answered (a 202 is `ok`) -/
  | codeSent (ok : Bool) (error : String)
  /-- `POST /auth/code` answered with a token, or an error -/
  | redeemed (token : String) (error : String)
  /-- the session was written to the Keychain (`ok := false`: kept in memory only) -/
  | stored (ok : Bool) (error : String)
  /-- a new `/api/send` client id -/
  | newId (id : String)
  /-- an API call answered: its HTTP status and JSON body (`file`: where a file answer was written),
  or a transport `error` -/
  | answer (call : Call) (status : Nat) (body : Json) (file : String) (error : String)
  deriving ToJson, FromJson, Inhabited

inductive Msg where
  -- sign-in
  | originInput (s : String)
  | email (s : String)
  | code (s : String)
  | tokenInput (s : String)
  | tokenMode (on : Bool)
  | devAccount (on : Bool)
  | sendCode
  | redeem
  | signIn
  | restart
  -- the list
  | filter (i : Nat)
  | search (s : String)
  | runSearch
  | clearSearch
  | refresh
  | loadMore
  | select (id : String)
  | back
  -- a message
  | act (action : String)
  | snoozeMenu (open_ : Bool)
  | openHtml
  | download (idx : Nat)
  | dismissNotice
  -- writing
  | compose
  | reply
  | composeFrom (s : String)
  | composeTo (s : String)
  | composeCc (s : String)
  | composeSubject (s : String)
  | composeBody (s : String)
  | send
  | cancelCompose
  -- settings and admin
  | settings
  | admin
  | approve (id : String)
  | reject (id : String)
  | signOut
  | host (h : Host)
  deriving Inhabited

end LeanMailApp
