import LeanMailApp.Model

/-! # The server's JSON API, as data

Each `Call` is one authenticated request to the LeanMail server (`lean/Server/{Api,Actions,Send,
Account,Operator}.lean`). `Call.request` gives its method, path and body; `perform` adds the origin
and the credential. `Call.isAction` marks the ones that change something on the server: the
message actions, sending, and approving or rejecting a sign-up. -/

namespace LeanMailApp
open Lean

inductive Call where
  /-- `GET /api/msgs?filter=&before=` -/
  | list (filter : String) (before : Option String)
  /-- `GET /api/search?q=&before=` -/
  | search (q : String) (before : Option String)
  /-- `GET /api/msg/:id` -/
  | message (id : String)
  /-- `GET /api/thread/:id` -/
  | thread (id : String)
  /-- `GET /api/msg/:id/html`, written to a file -/
  | html (id : String)
  /-- `GET /api/msg/:id/attachments/:idx`, written to a file named `name` -/
  | attachment (id : String) (idx : Nat) (name : String)
  /-- `GET /api/account` -/
  | account
  /-- `GET /admin/signups` (the operator container only) -/
  | signups
  /-- `PATCH /api/msg/:id {action}` -/
  | act (id action : String)
  /-- `POST /api/send` -/
  | send (clientId : String) (draft : Draft)
  /-- `POST /admin/signups/:id/approve` -/
  | approve (id : String)
  /-- `POST /admin/signups/:id/reject` -/
  | reject (id : String)
  deriving BEq, Inhabited, Repr

instance : ToJson Draft where
  toJson d := Json.mkObj [("from", d.from_), ("to", d.to), ("cc", d.cc), ("subject", d.subject), ("body", d.body),
    ("replyOf", toJson d.replyOf), ("clientId", d.clientId)]

instance : FromJson Draft where
  fromJson? j :=
    let d : Draft := { from_ := str j "from", to := str j "to", cc := str j "cc", subject := str j "subject", body := str j "body"
                       replyOf := (j.getObjValAs? String "replyOf").toOption, clientId := str j "clientId" }
    pure d

deriving instance ToJson, FromJson for Call

/-- A request that changes something on the server. -/
def Call.isAction : Call → Bool
  | .act .. | .send .. | .approve _ | .reject _ => true
  | _ => false

/-- The answer is a file (the bytes are written to disk), not JSON. -/
def Call.isFile : Call → Bool
  | .html _ | .attachment .. => true
  | _ => false

/-- RFC 3986 percent-encoding of everything but the unreserved characters. -/
def percentEncode (s : String) : String :=
  let hex := "0123456789ABCDEF".toList
  s.toUTF8.foldl (init := "") fun acc b =>
    let c := Char.ofNat b.toNat
    if c.isAlphanum || c == '-' || c == '.' || c == '_' || c == '~' then acc.push c
    else (acc.push '%').push (hex[(b >>> 4).toNat]!) |>.push (hex[(b &&& 15).toNat]!)

def query (ps : List (String × Option String)) : String :=
  let kvs := ps.filterMap fun (k, v) => v.map fun v => s!"{k}={percentEncode v}"
  if kvs.isEmpty then "" else "?" ++ "&".intercalate kvs

/-- `/api/send`'s body: recipients as the strings typed (the server splits and checks them). -/
def sendBody (clientId : String) (d : Draft) : Json :=
  Json.mkObj ([("client_id", Json.str clientId), ("to", Json.str d.to), ("subject", Json.str d.subject),
    ("text", Json.str d.body)] ++
    (if (trim d.cc).isEmpty then [] else [("cc", Json.str d.cc)]) ++
    (if (trim d.from_).isEmpty then [] else [("from", Json.str (trim d.from_))]) ++
    (match d.replyOf with | some r => [("reply_of", Json.str r)] | none => []))

/-- Method, path (with its query) and JSON body. -/
def Call.request : Call → String × String × Option Json
  | .list f b => ("GET", "/api/msgs" ++ query [("filter", some f), ("limit", some "50"), ("before", b)], none)
  | .search q b => ("GET", "/api/search" ++ query [("q", some q), ("limit", some "50"), ("before", b)], none)
  | .message id => ("GET", s!"/api/msg/{percentEncode id}", none)
  | .thread id => ("GET", s!"/api/thread/{percentEncode id}", none)
  | .html id => ("GET", s!"/api/msg/{percentEncode id}/html", none)
  | .attachment id idx _ => ("GET", s!"/api/msg/{percentEncode id}/attachments/{idx}", none)
  | .account => ("GET", "/api/account", none)
  | .signups => ("GET", "/admin/signups", none)
  | .act id a => ("PATCH", s!"/api/msg/{percentEncode id}", some (Json.mkObj [("action", Json.str a)]))
  | .send cid d => ("POST", "/api/send", some (sendBody cid d))
  | .approve id => ("POST", s!"/admin/signups/{percentEncode id}/approve", some (Json.mkObj []))
  | .reject id => ("POST", s!"/admin/signups/{percentEncode id}/reject", some (Json.mkObj []))

/-- The headers that carry a credential. -/
def credHeaders : Cred → List (String × String)
  | .token t => [("Authorization", s!"Bearer {t}")]
  | .dev => [("x-account-id", "dev")]

/-- Whether a message action takes the message out of the list it was in. -/
def leavesList (action : String) : Bool :=
  ["done", "snooze_1d", "snooze_3d", "snooze_week", "junk", "notjunk", "inbox"].contains action

/-- The origin as typed, without a trailing slash; `https://` added when no scheme was given. -/
def normalizeOrigin (s : String) : String :=
  let t := trim s
  let t := if t.startsWith "http://" || t.startsWith "https://" then t else "https://" ++ t
  if t.endsWith "/" then String.ofList (t.toList.dropLast) else t

end LeanMailApp
