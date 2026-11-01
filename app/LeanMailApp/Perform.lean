import Std.Time
import ElmLean.Native
import ElmLean.Native.Client.Http
import ElmLean.Native.Client.Keychain
import Core.Ulid
import LeanMailApp.App

/-! # Effects: the commands, run against the shell's capabilities

* HTTPS through `http.request` (`Client.Http.send`): the shell's URLSession; Lean's runtime has no TLS.
  JSON answers are parsed here; HTML (up to 2 MB) and attachments (up to 20 MB) are written under the
  `paths` temporary directory and shown with the share sheet.
* The session in the Keychain (`keychain.*`), one item: `session` in the app's own service.
* `build` (Debug or not) gates the test hooks: `LEANMAIL_TEST_ORIGIN` and `LEANMAIL_TEST_TOKEN` prefill
  the sign-in screen and `LEANMAIL_RESET=1` forgets the saved session, in a Debug build only.
* Randomness for `/api/send`'s `client_id`: `IO.getRandomBytes` (the runtime's `CCRandomGenerateBytes`),
  with the wall clock (`Std.Time`) as the ULID's time. No shell capability is needed for either.

Anything slow runs in a task: `perform` is called on the shell's main thread. -/

namespace LeanMailApp
open ElmLean ElmLean.Native Lean

def sessionItem : Client.Keychain.Item := { key := "session" }

def nowMs : IO Nat := do
  return (← Std.Time.Timestamp.now).toMillisecondsSinceUnixEpoch.val.toNat

def newUlid : IO String := do
  return Core.Ulid.encode (← nowMs) (← IO.getRandomBytes 10)

def errorOf (e : Client.Error) : String :=
  let code := match e.code with | some c => s!" {c}" | none => ""
  let detail := if e.detail.isEmpty then "" else s!": {e.detail}"
  s!"Could not reach the server ({e.name}{code}){detail}"

def jsonOf (b : ByteArray) : Json :=
  match String.fromUTF8? b with
  | some s => (Json.parse s).toOption.getD Json.null
  | none => Json.null

/-- A file name safe in one path component. -/
def safeName (s : String) : String :=
  let t := String.ofList ((s.toList.map fun c => if c.isAlphanum || c == '.' || c == '-' || c == '_' || c == ' ' then c else '_').take 100)
  let t := String.ofList (t.toList.dropWhile (· == '.'))
  if t.isEmpty then "attachment" else t

/-- The temporary directory the shell names (`paths`). -/
def tmpDir (fx : Effects Host) : IO System.FilePath := do
  let j ← IO.wait (← fx.ask "paths" (Json.mkObj []))
  let t := str j "tmp"
  return if t.isEmpty then "/tmp" else t

def maxBytesFor : Call → Nat
  | .html _ => 2_000_000
  | .attachment .. => 20_000_000
  | _ => 4_000_000

/-- One authenticated request, answered as `Host.answer`. -/
def callApi (fx : Effects Host) (s : Session) (c : Call) : IO Host := do
  let (method, path, body) := c.request
  let headers := [("Accept", if c.isFile then "*/*" else "application/json")] ++ credHeaders s.cred ++
    (if body.isSome then [("Content-Type", "application/json"), ("X-Requested-With", "LeanMail")] else [])
  let req : Client.Http.SendRequest :=
    { url := s.origin ++ path, httpMethod := method, headers, maxBytes := maxBytesFor c, timeout := some 30
      body := body.map (·.compress.toUTF8) }
  let r ← (Client.Http.send req).call fx
  match r with
  | .error e => return .answer c 0 Json.null "" (if e.name == "tooLarge" then "Too large to open in the app." else errorOf e)
  | .ok resp =>
    if c.isFile && resp.status == 200 then
      let (dir, name) := match c with
        | .attachment id idx n => (s!"{id}-{idx}", safeName n)
        | .html id => (id, "message.html")
        | _ => ("other", "file")
      let d := (← tmpDir fx) / "leanmail" / safeName dir
      IO.FS.createDirAll d
      let f := d / name
      IO.FS.writeBinFile f resp.body
      return .answer c 200 Json.null f.toString ""
    else return .answer c resp.status (jsonOf resp.body) "" ""

/-- A public `/auth/*` request: status and JSON body, or the transport error. -/
def callPublic (fx : Effects Host) (origin path : String) (body : Json) : IO (Except String (Nat × Json)) := do
  let req : Client.Http.SendRequest :=
    { url := origin ++ path, httpMethod := "POST", body := some body.compress.toUTF8, timeout := some 30
      headers := [("Accept", "application/json"), ("Content-Type", "application/json")] }
  let r ← (Client.Http.send req).call fx
  return match r with
    | .error e => .error (errorOf e)
    | .ok resp => .ok (resp.status, jsonOf resp.body)

def boot (fx : Effects Host) : IO Unit := do
  let debug := flag (← IO.wait (← fx.ask "build" (Json.mkObj []))) "debug"
  let env (k : String) : IO String := do
    if debug then return (← IO.getEnv k).getD "" else return ""
  if (← env "LEANMAIL_RESET") == "1" then discard <| (Client.Keychain.delete sessionItem).call fx
  let (saved, error) ← match ← (Client.Keychain.getText sessionItem).call fx with
    | .ok (some t) => pure ((Json.parse t).toOption >>= fun j => (fromJson? j : Except String Session).toOption, "")
    | .ok none => pure (none, "")
    -- an ad-hoc Mac build has no keychain access group (-34018): sign in for the run only
    | .error e => pure (none, if e.kind == .unsupported then "" else s!"The Keychain could not be read ({e.name}).")
  fx.post (.booted (Json.mkObj ([("debug", Json.bool debug), ("testOrigin", Json.str (← env "LEANMAIL_TEST_ORIGIN")),
    ("testToken", Json.str (← env "LEANMAIL_TEST_TOKEN")), ("error", Json.str error)] ++
    (match saved with | some s => [("saved", toJson s)] | none => []))))

def perform (c : Cmd) (fx : Effects Host) : IO Unit :=
  match c with
  | .boot => discard <| IO.asTask (prio := .dedicated) (boot fx)
  | .ready => fx.post .ready
  | .requestCode origin email => discard <| IO.asTask (prio := .dedicated) do
    match ← callPublic fx origin "/auth/login" (Json.mkObj [("email", Json.str email)]) with
    | .error e => fx.post (.codeSent false e)
    | .ok (st, j) =>
      if st == 200 || st == 202 then fx.post (.codeSent true "")
      else if st == 404 then fx.post (.codeSent false "This server does not mail codes (no /auth/login): sign in with an API token.")
      else fx.post (.codeSent false (failure st j ""))
  | .redeemCode origin email code => discard <| IO.asTask (prio := .dedicated) do
    match ← callPublic fx origin "/auth/code"
        (Json.mkObj [("email", Json.str email), ("code", Json.str code), ("name", Json.str "LeanMail app")]) with
    | .error e => fx.post (.redeemed "" e)
    | .ok (200, j) => fx.post (.redeemed (str j "token") "")
    | .ok (st, j) => fx.post (.redeemed "" (if st == 400 then "That code is wrong or has expired." else failure st j ""))
  | .store s => discard <| IO.asTask (prio := .dedicated) do
    match ← (Client.Keychain.setText sessionItem (toJson s).compress).call fx with
    | .ok () => fx.post (.stored true "")
    | .error e => fx.post (.stored false e.name)
  | .forget => discard <| IO.asTask (prio := .dedicated) do
    discard <| (Client.Keychain.delete sessionItem).call fx
  | .newId => discard <| IO.asTask do fx.post (.newId (← newUlid))
  | .api s call => discard <| IO.asTask (prio := .dedicated) do
    let h ← try callApi fx s call catch e => pure (.answer call 0 Json.null "" s!"{e}")
    fx.post h

def app : Native.App Model Msg Cmd Host :=
  { pureApp with perform, boot := [.boot] }

initialize Native.register app

end LeanMailApp
