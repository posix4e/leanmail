import Lean.Data.Json
import Core.Http
import Core.Base64
import Server.State
import Server.R2
import Server.Clock
import Server.Log
import Server.Crypto

/-! # Gmail: the connection row, the owner's switches, and the API calls

A port of MailVault's `src/gmail/client.ts` without the sealing. Everything that talks to Google
goes through `State.gmail` (`GmailApi`), so the tests answer with a fake Google.

The one `gmail_connection` row holds the owner's OAuth client, the refresh token, the poll cursor
(`history_id`) and the watch's expiry. The secrets are stored as they are: the container's disk
and its Litestream replica are the trust boundary.

Failures are `IO.userError "google:<code>"` (`googleFail`), read back with `codeOf`: an enum and
never a message, so logs and `last_error` stay content-free. The codes are MailVault's:
`no_client`, `not_authorized`, `refresh_rejected`, `forbidden`, `insufficient_scope`,
`http_<status>`, `token_<status>`, `network`. -/

namespace Server.Gmail

open Lean Server.Db

def tokenUrl : String := "https://oauth2.googleapis.com/token"
def authUrl : String := "https://accounts.google.com/o/oauth2/v2/auth"
def apiBase : String := "https://gmail.googleapis.com/gmail/v1/users/me"

/-- `gmail.modify` reads everything and can move labels (the mirror, spam, send) but cannot
delete outright; the two settings scopes read the send-as identities (and, later, forwarding). -/
def scopes : List String := [
  "https://www.googleapis.com/auth/gmail.modify",
  "https://www.googleapis.com/auth/gmail.settings.basic",
  "https://www.googleapis.com/auth/gmail.settings.sharing"]

/-! ## Errors -/

def googleFail (code : String) : IO α := throw (IO.userError s!"google:{code}")

/-- The code of a `googleFail`, else `error`. -/
def codeOf (e : IO.Error) : String :=
  let s := toString e
  if s.startsWith "google:" then String.ofList (s.toList.drop 7) else "error"

private def httpStatus? (code : String) : Option Nat :=
  if code.startsWith "http_" then (String.ofList (code.toList.drop 5)).toNat? else none

/-- Trouble a person has to clear: a revoked grant, a missing client, a narrower scope. -/
def needsAPerson (code : String) : Bool :=
  ["refresh_rejected", "forbidden", "insufficient_scope", "not_authorized", "no_client"].contains code

/-- "Not now" rather than "not ever": the page or batch replays later. -/
def transient (code : String) : Bool :=
  code == "network" || code == "forbidden" || code == "http_429" ||
    ((httpStatus? code).map (fun n => decide (n ≥ 500))).getD false

/-- Gmail has no such message: a 404, or the 400 it gives for an id it cannot parse. -/
def isMissing (code : String) : Bool := code == "http_404" || code == "http_400"

/-! ## JSON -/

def parseBody (b : ByteArray) : Json :=
  (Json.parse ((String.fromUTF8? b).getD "")).toOption.getD Json.null

/-- A field Google sends as a string or a number (`historyId`, `internalDate`, `expiration`). -/
def jStrNum? (j : Json) (k : String) : Option String :=
  match j.getObjVal? k with
  | .ok (.str s) => if s.isEmpty then none else some s
  | .ok (.num n) => some n.toString
  | _ => none

def jNat? (j : Json) (k : String) : Option Nat := (jStrNum? j k).bind (·.toNat?)

def jStr? (j : Json) (k : String) : Option String := (j.getObjValAs? String k).toOption

def jArr (j : Json) (k : String) : Array Json := (j.getObjValAs? (Array Json) k).toOption.getD #[]

def jStrs (j : Json) (k : String) : Array String := (jArr j k).filterMap fun | .str s => some s | _ => none

def strArr (xs : List String) : Json := Json.arr (xs.toArray.map Json.str)

def optNum : Option Nat → Json | some n => Json.num n | none => Json.null
def optStr : Option String → Json | some s => Json.str s | none => Json.null

/-! ## Settings and prefs -/

def setting? (st : State) (key : String) : IO (Option String) := do
  return (← st.db.queryOne "SELECT value FROM settings WHERE key = ?" #[key]).map (·.text "value")

def settingRow? (st : State) (key : String) : IO (Option (String × Nat)) := do
  return (← st.db.queryOne "SELECT value, updated_at FROM settings WHERE key = ?" #[key]).map fun r =>
    (r.text "value", r.nat "updated_at")

def setSetting (st : State) (key value : String) : IO Unit := do
  st.db.exec "INSERT INTO settings(key, value, updated_at) VALUES (?, ?, ?)
    ON CONFLICT(key) DO UPDATE SET value = excluded.value, updated_at = excluded.updated_at" #[key, value, ← Clock.nowMs]

def delSetting (st : State) (key : String) : IO Unit := st.db.exec "DELETE FROM settings WHERE key = ?" #[key]

/-- An owner's switch from `prefs`: `1`/`true`/`on` or `0`/`false`/`off`, else the default. -/
def prefOn (st : State) (key : String) (default : Bool) : IO Bool := do
  match ← st.db.queryOne "SELECT value FROM prefs WHERE key = ?" #[key] with
  | some r =>
    let v := (r.text "value").toLower
    if ["1", "true", "on", "yes"].contains v then return true
    if ["0", "false", "off", "no"].contains v then return false
    return default
  | none => return default

def setPref (st : State) (key : String) (on : Bool) : IO Unit := do
  st.db.exec "INSERT INTO prefs(key, value, updated_at) VALUES (?, ?, ?)
    ON CONFLICT(key) DO UPDATE SET value = excluded.value, updated_at = excluded.updated_at"
    #[key, if on then "1" else "0", ← Clock.nowMs]

/-- Take mail marked done out of Gmail's inbox (and put it back on undo). Default on. -/
def prefMirror : String := "gmail.mirror_archive"
/-- Mark mail reported as junk as spam in Gmail (and take it back on "not junk"). Default on. -/
def prefSpam : String := "gmail.report_spam"
/-- Whether the backfill lists Spam and Trash too. Default on, as MailVault's import was. -/
def prefSpamTrash : String := "gmail.backfill_spam_trash"

/-! ## The connection -/

structure Conn where
  email : Option String
  clientId : String
  clientSecret : String
  refreshToken : Option String
  historyId : Option String
  watchExpiration : Option Nat
  lastPushAt : Option Nat
  lastPollAt : Option Nat
  enabled : Bool
  lastError : Option String
  createdAt : Nat
  updatedAt : Nat
  deriving Inhabited

def conn? (st : State) : IO (Option Conn) := do
  let some r ← st.db.queryOne "SELECT * FROM gmail_connection WHERE id = 1" | return none
  let nonEmpty (o : Option String) := o.bind fun s => if s.isEmpty then none else some s
  return some {
    email := nonEmpty (r.text? "email"), clientId := r.text "client_id", clientSecret := r.text "client_secret"
    refreshToken := nonEmpty (r.text? "refresh_token"), historyId := nonEmpty (r.text? "history_id")
    watchExpiration := r.nat? "watch_expiration", lastPushAt := r.nat? "last_push_at"
    lastPollAt := r.nat? "last_poll_at", enabled := r.bool "enabled" true, lastError := r.text? "last_error"
    createdAt := r.nat "created_at", updatedAt := r.nat "updated_at" }

/-- Connected: a refresh token, and not switched off. -/
def Conn.authorized (c : Conn) : Bool := c.enabled && c.refreshToken.isSome

/-- Google refused the stored grant: the owner must connect again. -/
def Conn.needsReauth (c : Conn) : Bool := ((c.lastError.getD "").startsWith "refresh_rejected")

/-- A connection the lanes may use. -/
def usable? (st : State) : IO (Option Conn) := do
  match ← conn? st with
  | some c => return if c.authorized && !c.needsReauth then some c else none
  | none => return none

/-- `UPDATE gmail_connection SET <set>, updated_at = now`. -/
def updateConn (st : State) (set : String) (args : Array Value) : IO Unit := do
  st.db.exec s!"UPDATE gmail_connection SET {set}, updated_at = ? WHERE id = 1" (args.push (.int (← Clock.nowMs).toInt64))

/-! ## HTTP -/

def formBody (kv : List (String × String)) : ByteArray :=
  ("&".intercalate (kv.map fun (k, v) => R2.uriEncode k ++ "=" ++ R2.uriEncode v)).toUTF8

def query (kv : List (String × String)) : String :=
  if kv.isEmpty then "" else "?" ++ "&".intercalate (kv.map fun (k, v) => R2.uriEncode k ++ "=" ++ R2.uriEncode v)

/-- One request to Google; a transport failure is `network`. -/
def call (st : State) (r : Core.Http.Request) : IO Core.Http.Response := do
  let res ← (try (some <$> st.gmail.request r) catch _ => pure none)
  match res with
  | some x => return x
  | none => googleFail "network"

private def contains (s sub : String) : Bool := (s.splitOn sub).length > 1

/-- A 403 for a token granted a narrower scope than the call needs. -/
def insufficientScope (body : ByteArray) : Bool :=
  let t := (String.fromUTF8? body).getD ""
  contains t "insufficientPermissions" || contains t "ACCESS_TOKEN_SCOPE_INSUFFICIENT" ||
    contains t "PERMISSION_DENIED"

/-- `method {apiBase}{path}` with the access token; the answer as JSON (`null` for an empty
body). -/
def api (st : State) (token method path : String) (body : Option Json := none) : IO Json := do
  let r ← call st {
    method, url := apiBase ++ path, timeoutMs := 60000
    headers := [("Authorization", s!"Bearer {token}")] ++ (if body.isSome then [("Content-Type", "application/json")] else [])
    body := (body.map (·.compress.toUTF8)).getD .empty }
  if r.status == 403 && insufficientScope r.body then googleFail "insufficient_scope"
  if r.status == 401 || r.status == 403 then googleFail "forbidden"
  if !r.isOk then googleFail s!"http_{r.status}"
  return parseBody r.body

def enc (s : String) : String := R2.uriEncode s

/-! ## The calls -/

/-- `users.getProfile`: `emailAddress`, `messagesTotal`, `historyId`. -/
def profile (st : State) (tok : String) : IO Json := api st tok "GET" "/profile"

/-- One page of `users.history.list` (message additions only). -/
def historyPage (st : State) (tok start : String) (pageToken : Option String) : IO Json :=
  api st tok "GET" ("/history" ++ query ([("startHistoryId", start), ("historyTypes", "messageAdded")] ++
    (pageToken.map fun p => [("pageToken", p)]).getD []))

structure ListPage where
  ids : Array String
  nextPageToken : Option String
  total : Option Nat

/-- One page of `users.messages.list`, newest first. -/
def listMessages (st : State) (tok : String) (pageToken : Option String) (max : Nat) (search : Option String := none)
    (includeSpamTrash : Bool := true) : IO ListPage := do
  let q := [("maxResults", toString (Nat.min 500 (Nat.max 1 max))), ("includeSpamTrash", if includeSpamTrash then "true" else "false")] ++
    (pageToken.map fun p => [("pageToken", p)]).getD [] ++ (search.map fun s => [("q", s)]).getD []
  let j ← api st tok "GET" ("/messages" ++ query q)
  return { ids := (jArr j "messages").filterMap (jStr? · "id") |>.filter (· ≠ "")
           nextPageToken := jStr? j "nextPageToken", total := jNat? j "resultSizeEstimate" }

structure RawMessage where
  raw : ByteArray
  labels : Array String
  internalDate : Option Nat
  threadId : Option String

/-- `users.messages.get format=raw`, the base64url decoded. -/
def rawMessage (st : State) (tok id : String) : IO RawMessage := do
  let j ← api st tok "GET" s!"/messages/{enc id}?format=raw"
  let some raw := jStr? j "raw" | googleFail "no_raw"
  return { raw := Core.Base64.decode raw, labels := jStrs j "labelIds", internalDate := jNat? j "internalDate"
           threadId := jStr? j "threadId" }

/-- A message's labels, or `none` when Gmail no longer has it. -/
def labelsOf (st : State) (tok id : String) : IO (Option (Array String)) := do
  try
    let j ← api st tok "GET" s!"/messages/{enc id}?format=minimal&fields=id,labelIds"
    return some (jStrs j "labelIds")
  catch e =>
    if isMissing (codeOf e) then return none
    throw e

/-- `users.messages.modify`: labels on and off one message. -/
def modify (st : State) (tok id : String) (add remove : List String) : IO Unit := do
  let fields := (if add.isEmpty then [] else [("addLabelIds", strArr add)]) ++
    (if remove.isEmpty then [] else [("removeLabelIds", strArr remove)])
  discard <| api st tok "POST" s!"/messages/{enc id}/modify" (some (Json.mkObj fields))

/-- The Gmail thread of a message, `none` when Gmail does not have it. -/
def threadIdOf (st : State) (tok id : String) : IO (Option String) := do
  try return jStr? (← api st tok "GET" s!"/messages/{enc id}?format=minimal") "threadId"
  catch _ => return none

/-- `users.messages.send` with the whole message (Gmail reads and strips a `Bcc`): the new Gmail
id and thread. -/
def sendRaw (st : State) (tok : String) (raw : ByteArray) (threadId : Option String) : IO (String × Option String) := do
  let body := Json.mkObj ([("raw", Json.str (Core.Base64.encodeUrl raw))] ++
    (threadId.map fun t => [("threadId", Json.str t)]).getD [])
  let j ← api st tok "POST" "/messages/send" (some body)
  let some id := jStr? j "id" | googleFail "send_no_id"
  return (id, jStr? j "threadId")

/-- `users.watch`: Gmail publishes changes to the Pub/Sub topic until `expiration` (epoch ms). -/
def watch (st : State) (tok topic : String) : IO Json :=
  api st tok "POST" "/watch" (some (Json.mkObj [("topicName", Json.str topic)]))

def stopWatch (st : State) (tok : String) : IO Unit := discard <| api st tok "POST" "/stop"

/-- `users.settings.sendAs.list`: the verified addresses Gmail sends as, lower-cased. -/
def sendAsList (st : State) (tok : String) : IO (List String) := do
  let j ← api st tok "GET" "/settings/sendAs"
  return (jArr j "sendAs").toList.filterMap fun x =>
    let ok := (x.getObjValAs? Bool "isPrimary").toOption == some true || jStr? x "verificationStatus" == some "accepted"
    if ok then (jStr? x "sendAsEmail").map (·.toLower) else none

/-! ## Waking the job loop early -/

/-- Set by a hook (`Gmail.Mirror.onAction`) that queued work for Google: the in-process job loop
runs the mirror and spam lanes within a second instead of at its next minute. -/
initialize kicked : IO.Ref Bool ← IO.mkRef false

def kick : IO Unit := kicked.set true

end Server.Gmail
