import Lean.Data.Json
import Core.Version
import Core.Http
import Core.Text.Str
import Server.Http
import Server.Clock
import Core.Ulid
import Cli.Base
import Cli.Config

/-! # Requests to the server

Every request carries `User-Agent: leanmail-cli/<version>`, `Authorization: Bearer <token>` when a
token is saved and `x-account-id: <account>` when an account is. A connection failure exits 2; a
non-2xx answer exits 1 with one line naming the status and the server's `error` field. -/

namespace Cli

open Lean

structure Client where
  origin : String
  token : Option String := none
  account : Option String := none
  timeoutMs : UInt32 := 60000

/-- The client for the saved configuration; exit 2 when no server is configured. -/
def Client.ofConfig (c : Config) : CliM Client := do
  if c.server.isEmpty then usageError "no server configured; run `leanmail login <server-origin>` first"
  return { origin := c.server, token := c.token, account := c.account }

def isUnreserved (c : Char) : Bool :=
  c.isAlphanum && c.toNat < 128 || c == '-' || c == '.' || c == '_' || c == '~'

private def hexDigit (n : Nat) : Char := "0123456789ABCDEF".toList[n % 16]!

/-- RFC 3986 percent-encoding of every byte outside the unreserved set. -/
def percentEncode (s : String) : String := Id.run do
  let mut out := ""
  for b in s.toUTF8.toList do
    let c := Char.ofNat b.toNat
    if b < 128 && isUnreserved c then out := out.push c
    else out := out.push '%' |>.push (hexDigit (b.toNat / 16)) |>.push (hexDigit b.toNat)
  return out

def queryString (q : List (String × String)) : String :=
  if q.isEmpty then "" else
  "?" ++ "&".intercalate (q.map fun (k, v) => percentEncode k ++ "=" ++ percentEncode v)

def Client.headers (c : Client) (extra : List (String × String)) : List (String × String) :=
  [("User-Agent", s!"leanmail-cli/{Core.version}")] ++
  (c.token.map fun t => [("Authorization", s!"Bearer {t}")]).getD [] ++
  (c.account.map fun a => [("x-account-id", a)]).getD [] ++ extra

def bodyString (r : Core.Http.Response) : String := (String.fromUTF8? r.body).getD ""

/-- The server's `error` field when the body is JSON with one, else the start of the body. -/
def errorDetail (r : Core.Http.Response) : String :=
  let text := bodyString r
  match Json.parse text with
  | .ok j => match j.getObjValAs? String "error" with
    | .ok e => e
    | .error _ => ""
  | .error _ =>
    let line := ((Core.Text.Str.trim text).splitOn "\n").headD ""
    String.ofList (line.toList.take 200)

/-- Send a request; exit 2 when the server cannot be reached. Any status comes back. -/
def Client.send (c : Client) (method path : String) (query : List (String × String) := [])
    (headers : List (String × String) := []) (body : ByteArray := .empty) : CliM Core.Http.Response := do
  let url := c.origin ++ path ++ queryString query
  liftIO (code := 2) (pre := s!"cannot reach {c.origin}: ")
    (Server.Http.request { method, url, headers := c.headers headers, body, timeoutMs := c.timeoutMs })

/-- Exit 1 with `<method> <path>: <status> <error>` unless the answer is 2xx. -/
def check (method path : String) (r : Core.Http.Response) : CliM Core.Http.Response := do
  if r.isOk then return r
  let d := errorDetail r
  fail s!"{method} {path}: {r.status}{if d.isEmpty then "" else " " ++ d}"

def Client.request (c : Client) (method path : String) (query : List (String × String) := [])
    (headers : List (String × String) := []) (body : ByteArray := .empty) : CliM Core.Http.Response := do
  check method path (← c.send method path query headers body)

def Client.get (c : Client) (path : String) (query : List (String × String) := []) : CliM Core.Http.Response :=
  c.request "GET" path query

def Client.postRaw (c : Client) (path : String) (body : ByteArray) (headers : List (String × String) := []) :
    CliM Core.Http.Response :=
  c.request "POST" path [] headers body

def Client.postJson (c : Client) (path : String) (j : Json) : CliM Core.Http.Response :=
  c.request "POST" path [] [("Content-Type", "application/json")] j.compress.toUTF8

/-- The body as JSON; exit 1 when it is not. -/
def parseJson (r : Core.Http.Response) : CliM Json :=
  match Json.parse (bodyString r) with
  | .ok j => pure j
  | .error e => fail s!"the server's answer is not JSON: {e}"

def Client.getJson (c : Client) (path : String) (query : List (String × String) := []) : CliM (Core.Http.Response × Json) := do
  let r ← c.get path query
  return (r, ← parseJson r)

/-- A request with a JSON body (`PATCH`, `PUT`, `POST`, `DELETE`), exit 1 unless 2xx. -/
def Client.jsonReq (c : Client) (method path : String) (j : Json) (headers : List (String × String) := []) :
    CliM Core.Http.Response :=
  c.request method path [] ([("Content-Type", "application/json")] ++ headers) j.compress.toUTF8

/-- `If-Match: <revision>` when one is given. -/
def ifMatch (rev : Option Nat) : List (String × String) :=
  (rev.map fun r => [("If-Match", toString r)]).getD []

/-- A fresh ULID (draft ids, client ids, mutation ids). -/
def newUlid : IO String := do
  return Core.Ulid.encode (← Server.Clock.nowMs) (← IO.getRandomBytes 10)

end Cli
