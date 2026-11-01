import Lean.Data.Json
import Server.State
import Server.Crypto
import Server.Log
import Core.Text.Str

/-! # Routing types

Handlers see a plain request record (`Req`) and return a plain `Resp`, so tests call them
directly and `Server.Main` adapts `Std.Http` on either side. Every route names the credential it
needs (`Auth`); `Server.Router.authorize` is the one place that checks it and hands the handler
the `Principal` it let in. The types live here so the `Auth/*` modules can define routes without
importing the router that calls them. -/

namespace Server.Router

open Lean

/-- What a route requires. -/
inductive Auth where
  | «public»
  /-- A browser session (`lm_session` cookie). -/
  | session
  /-- An API token with this scope (`read`, `act`, `draft`, `send`, `junk`); a session also passes. -/
  | token (scope : String)
  /-- The Worker, with `X-Internal-Secret`. -/
  | internal
  /-- An operator administrator (operator container only; see `Server.Operator`). -/
  | admin
  deriving Repr, BEq, Inhabited

/-- Who a request was let in as. -/
inductive Principal where
  | anon
  | internal
  /-- `DEV_AUTH_BYPASS`: an admin session without a cookie. -/
  | dev
  | session (sid : String)
  | token (id : String) (scopes : List String)
  /-- An operator administrator, named by the account id that acts. -/
  | admin (actor : String)
  deriving Repr, BEq, Inhabited

structure Req where
  method : String
  path : List String
  query : List (String × String) := []
  /-- Lower-case names. -/
  headers : List (String × String) := []
  body : ByteArray := .empty
  deriving Inhabited

def Req.header? (r : Req) (name : String) : Option String :=
  let n := name.toLower
  (r.headers.find? (·.1 == n)).map (·.2)

def Req.param? (r : Req) (name : String) : Option String :=
  (r.query.find? (·.1 == name)).map (·.2)

def Req.natParam (r : Req) (name : String) (d : Nat) : Nat :=
  ((r.param? name).bind (·.toNat?)).getD d

/-- A cookie by name from the `Cookie` header. -/
def Req.cookie? (r : Req) (name : String) : Option String := do
  let h ← r.header? "cookie"
  let part ← (h.splitOn ";").find? fun p =>
    match p.splitOn "=" with
    | k :: _ => Core.Text.Str.trim k == name
    | [] => false
  let v := String.intercalate "=" ((part.splitOn "=").drop 1)
  some (Core.Text.Str.trim v)

/-- The `Authorization: Bearer …` credential. -/
def Req.bearer? (r : Req) : Option String := do
  let h ← r.header? "authorization"
  let h := Core.Text.Str.trim h
  if h.toLower.startsWith "bearer " then some (Core.Text.Str.trim (String.ofList (h.toList.drop 7))) else none

/-- The body as a JSON object (`Json.null` when absent or malformed). -/
def Req.json (r : Req) : Json :=
  match String.fromUTF8? r.body with
  | some s => match Json.parse s with
    | .ok j@(.obj _) => j
    | _ => Json.null
  | none => Json.null

def jstr? (j : Json) (k : String) : Option String := (j.getObjValAs? String k).toOption
def jbool? (j : Json) (k : String) : Option Bool := (j.getObjValAs? Bool k).toOption

structure Resp where
  status : Nat
  headers : List (String × String) := []
  body : ByteArray := .empty
  deriving Inhabited

def Resp.json (status : Nat) (j : Json) : Resp :=
  { status, headers := [("Content-Type", "application/json")], body := j.compress.toUTF8 }

def Resp.ok (j : Json) : Resp := Resp.json 200 j

def Resp.error (status : Nat) (msg : String) : Resp := Resp.json status (Json.mkObj [("error", Json.str msg)])

def Resp.notFound : Resp := Resp.error 404 "not found"

def Resp.bodyText (r : Resp) : String := String.fromUTF8? r.body |>.getD ""

def Resp.withHeader (r : Resp) (k v : String) : Resp := { r with headers := r.headers ++ [(k, v)] }

/-- `{"ok":true}` with this status. -/
def Resp.okStatus (status : Nat := 200) : Resp := Resp.json status (Json.mkObj [("ok", Json.bool true)])

/-- What the request was allowed in on. -/
structure Ctx where
  st : State
  account : String
  auth : Auth
  /-- Path parameters by name. -/
  params : List (String × String) := []
  principal : Principal := .anon

def Ctx.param (c : Ctx) (name : String) : String := ((c.params.find? (·.1 == name)).map (·.2)).getD ""

inductive Seg where
  | lit (s : String)
  | param (name : String)
  deriving Repr, BEq, Inhabited

/-- `/api/msg/:id` → segments. -/
def pattern (s : String) : List Seg :=
  (s.splitOn "/").filter (· ≠ "") |>.map fun p =>
    if p.startsWith ":" then .param (String.ofList (p.toList.drop 1)) else .lit p

structure Route where
  method : String
  pattern : List Seg
  auth : Auth
  handler : Ctx → Req → IO Resp

def route (method path : String) (auth : Auth) (handler : Ctx → Req → IO Resp) : Route :=
  { method, pattern := pattern path, auth, handler }

def matchPath : List Seg → List String → Option (List (String × String))
  | [], [] => some []
  | .lit a :: ps, s :: ss => if a == s then matchPath ps ss else none
  | .param n :: ps, s :: ss => (matchPath ps ss).map ((n, s) :: ·)
  | _, _ => none

/-- Internal routes carry the shared secret, compared in constant time. With no configured
secret they are open only on a bare localhost run (`Config.bareDev`). A request the Worker
forwarded from a client carries `x-lm-public: 1` (the Worker stamps it and strips any a client
sent), and is never internal, whatever secret it carries. -/
def internalOk (st : State) (req : Req) : Bool :=
  if (req.header? "x-lm-public").isSome then false
  else if st.config.internalSecret.isEmpty then st.config.bareDev
  else match req.header? "x-internal-secret" with
    | some v => Crypto.constantTimeEq v.toUTF8 st.config.internalSecret.toUTF8
    | none => false

/-- The raw (still percent-encoded) request path, checked before routing: no empty segment
(`//`; one trailing `/` is allowed), no `.`/`..` segment, no percent-encoded `/`, `\`, `.` or NUL
in any segment, and a first segment of plain `[A-Za-z0-9_.-]+` (so `/%69nternal/…` never routes
as `/internal/…`). Anything else is answered 404 by `Server.Main`. -/
def rawPathOk (raw : String) : Bool := Id.run do
  if raw == "/" || raw.isEmpty then return true
  if !raw.startsWith "/" then return false
  let segs := (String.ofList (raw.toList.drop 1)).splitOn "/"
  let segs := if segs.getLast? == some "" then segs.dropLast else segs
  if segs.isEmpty then return false
  for s in segs do
    if s.isEmpty || s == "." || s == ".." then return false
    let l := s.toLower
    if ["%2f", "%5c", "%2e", "%00"].any fun x => (l.splitOn x).length > 1 then return false
  let first := segs.headD ""
  return first.toList.all fun c => c.isAlphanum || c == '_' || c == '.' || c == '-'

/-- Lower-cased, trimmed; `none` unless it looks like one address. -/
def normEmail? (s : String) : Option String :=
  let e := (Core.Text.Str.trim s).toLower
  match e.splitOn "@" with
  | [l, d] =>
    if l.isEmpty || d.isEmpty || !(d.contains '.') || e.length > 320 ||
       e.toList.any (fun c => c.toNat ≤ 0x20 || c == '<' || c == '>' || c == ',' || c == ';' || c == '"')
    then none else some e
  | _ => none

end Server.Router
