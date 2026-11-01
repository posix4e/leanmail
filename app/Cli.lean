import ElmLean.CmdCli
import LeanMailApp.App

/-! The app on stdin/stdout (PROTOCOL.md): `leanmail-app-cli selftest` explores it from its initial model
with sample host answers and re-checks, in every reached model, the two command rules that
`Gate.lean` proves. -/

open ElmLean Lean LeanMailApp

def sampleSession : Session := { origin := "http://127.0.0.1:8080", cred := .dev }

def sampleItem : Json := Json.mkObj [("id", "01M2DJXNE8G9V26HHRYK3ZA727"), ("thread_id", "T1"),
  ("from", Json.mkObj [("name", "Nadia"), ("address", "nadia@example.com")]), ("subject", "Site walk"),
  ("snippet", "Thursday"), ("received_at", (1789309933000 : Nat)), ("has_attachments", true)]

def samples (m : Model) : List Host :=
  [ .booted (Json.mkObj [("debug", true)]),
    .booted (Json.mkObj [("debug", true), ("saved", toJson sampleSession)]),
    .ready, .codeSent true "", .redeemed "lm1.dev.x.y" "", .stored true "", .newId "01M2DJXNE8G9V26HHRYK3ZA727",
    .answer (listCall m none) 200 (Json.mkObj [("items", Json.arr #[sampleItem]), ("next", "c")]) "" "",
    .answer (.message m.opened) 200 (sampleItem.setObjValAs! "attachments"
      (Json.arr #[Json.mkObj [("idx", (0 : Nat)), ("filename", "a.pdf"), ("size", (10 : Nat))]])) "" "",
    .answer (.act m.opened "done") 200 sampleItem "" "",
    .answer .signups 200 (Json.mkObj [("items", Json.arr #[Json.mkObj [("id", "S1"), ("email", "x@example.com")]])]) "" "",
    .answer (.list "inbox" none) 0 Json.null "" "offline" ]

/-- The rules over every message in the model (a runtime mirror of the gate's theorems). -/
def probeMsgs (m : Model) : List Msg :=
  [.sendCode, .redeem, .signIn, .refresh, .loadMore, .select "x", .act "done", .openHtml, .download 0, .send,
   .settings, .admin, .approve "S1", .reject "S1", .signOut, .filter 1, .runSearch, .compose, .reply] ++
  (samples m).map Msg.host

def rulesHold (m : Model) : Bool :=
  (probeMsgs m).all fun msg =>
    let cs := cmds msg m
    (m.session.isSome || cs.all (!·.isApi)) && (!m.busy || cs.all (!·.isAction))

instance : ToJson Host := inferInstance

def main (args : List String) : IO UInt32 :=
  CmdCli.main pureApp args { cli := { check := rulesHold, maxSteps := 4000 }, samples }
