import Lean.Data.Json
import Core.Mime.Date
import Core.Text.Str

/-! # The app's state

One flat `Model` (the mailvault-ios-leannative convention): `busy` while a blocking request is in flight,
`error` for the last failure shown to the person. Everything fetched from the server is held here, in
memory only; nothing is cached on disk except the sign-in credential, which lives in the Keychain. -/

namespace LeanMailApp
open Lean

/-- How the app proves who it is to the server. -/
inductive Cred where
  /-- an API token, `lm1.<account>.<rand>.<mac>` (minted by `POST /auth/code` or pasted from Settings) -/
  | token (t : String)
  /-- a bare development server with `DEV_AUTH_BYPASS=1`: the header `x-account-id: dev` (debug builds) -/
  | dev
  deriving BEq, Inhabited, Repr, ToJson, FromJson

/-- A signed-in session: the server and the credential. -/
structure Session where
  origin : String
  cred : Cred
  deriving BEq, Inhabited, Repr, ToJson, FromJson

/-- One row of a list (`/api/msgs`, `/api/search`, `/api/thread/:id`). -/
structure Item where
  id : String
  threadId : String := ""
  fromName : String := ""
  fromAddr : String := ""
  subject : String := ""
  snippet : String := ""
  receivedAt : Nat := 0
  hasAttachments : Bool := false
  hasInvite : Bool := false
  archived : Bool := false
  snoozed : Bool := false
  toRead : Bool := false
  junk : Bool := false
  keptForever : Bool := false
  deriving BEq, Inhabited, Repr

structure Attachment where
  idx : Nat
  filename : String
  mime : String
  size : Nat
  deriving BEq, Inhabited, Repr

/-- `GET /api/msg/:id`. -/
structure Detail where
  item : Item
  to : List String := []
  cc : List String := []
  replyTo : String := ""
  text : String := ""
  truncated : Bool := false
  attachments : List Attachment := []
  deriving BEq, Inhabited, Repr

/-- A pending sign-up (`GET /admin/signups`, operator only). -/
structure Signup where
  id : String
  email : String
  note : String := ""
  createdAt : Nat := 0
  deriving BEq, Inhabited, Repr

/-- The message being written. `clientId` is the `/api/send` idempotency key (a ULID, made once per
draft, so a retry after a failed delivery is the same send). -/
structure Draft where
  /-- empty: the account's default address -/
  from_ : String := ""
  to : String := ""
  cc : String := ""
  subject : String := ""
  body : String := ""
  replyOf : Option String := none
  clientId : String := ""
  deriving BEq, Inhabited, Repr

inductive Screen where
  | inbox
  | message
  | settings
  | admin
  deriving BEq, Inhabited, Repr, DecidableEq

/-- The list filters of `GET /api/msgs?filter=`: the label shown, the server's name. -/
def filters : List (String × String) :=
  [("Inbox", "inbox"), ("To read", "toread"), ("Snoozed", "snoozed"), ("Done", "archived"),
   ("Sent", "sent"), ("Junk", "junk"), ("All", "all")]

def filterLabels : List String := filters.map (·.1)

def filterName (i : Nat) : String := ((filters[i]?).map (·.2)).getD "inbox"

def filterLabel (i : Nat) : String := ((filters[i]?).map (·.1)).getD "Inbox"

def defaultOrigin : String := "https://mail.leanmail.io"

structure Model where
  /-- the boot command has answered (debug flag, saved session) -/
  booted : Bool := false
  /-- a Debug build: the dev-account toggle is offered -/
  debug : Bool := false
  -- sign-in
  originInput : String := defaultOrigin
  email : String := ""
  code : String := ""
  codeSent : Bool := false
  tokenMode : Bool := false
  tokenInput : String := ""
  devAccount : Bool := false
  -- signed in
  session : Option Session := none
  busy : Bool := false
  error : String := ""
  notice : String := ""
  screen : Screen := .inbox
  filter : Nat := 0
  /-- the search field -/
  search : String := ""
  /-- the search the list shows; empty: the filter's list -/
  query : String := ""
  items : List Item := []
  next : Option String := none
  detail : Option Detail := none
  /-- the id being opened (its row stays visible while the detail loads) -/
  opened : String := ""
  thread : List Item := []
  /-- the HTML part written to a file, for the share sheet -/
  htmlPath : String := ""
  /-- downloaded attachments: index and file -/
  files : List (Nat × String) := []
  snoozing : Bool := false
  compose : Option Draft := none
  accountEmail : String := ""
  isAdmin : Bool := false
  signups : List Signup := []
  deriving Inhabited

def Model.signedIn (m : Model) : Bool := m.session.isSome

/-! ## Reading the server's JSON (lenient: a missing field is its default) -/

def str (j : Json) (k : String) : String := (j.getObjValAs? String k).toOption.getD ""
def nat (j : Json) (k : String) : Nat := (j.getObjValAs? Nat k).toOption.getD 0
def flag (j : Json) (k : String) : Bool := (j.getObjValAs? Bool k).toOption.getD false
/-- A field that is set: neither absent nor `null`. -/
def present (j : Json) (k : String) : Bool :=
  match j.getObjVal? k with
  | .ok .null | .error _ => false
  | .ok _ => true
def arr (j : Json) (k : String) : List Json := ((j.getObjValAs? (Array Json) k).toOption.getD #[]).toList

def itemOf (j : Json) : Item :=
  let f := (j.getObjVal? "from").toOption.getD Json.null
  { id := str j "id", threadId := str j "thread_id", fromName := str f "name", fromAddr := str f "address",
    subject := str j "subject", snippet := str j "snippet", receivedAt := nat j "received_at",
    hasAttachments := flag j "has_attachments", hasInvite := flag j "has_invite",
    archived := present j "archived_at", snoozed := present j "snoozed_until", toRead := present j "to_read_at",
    junk := present j "junk_kind", keptForever := !present j "expires_at" }

def itemsOf (j : Json) : List Item := (arr j "items").map itemOf

/-- An address list entry: a string, or `{name, address}`. -/
def addrOf (j : Json) : String :=
  match j with
  | .str s => s
  | _ =>
    let a := str j "address"
    let n := str j "name"
    if n.isEmpty then a else s!"{n} <{a}>"

def detailOf (j : Json) : Detail :=
  { item := itemOf j, to := (arr j "to").map addrOf, cc := (arr j "cc").map addrOf, replyTo := str j "reply_to",
    text := str j "text", truncated := flag j "text_truncated"
    attachments := (arr j "attachments").map fun a =>
      { idx := nat a "idx", filename := str a "filename", mime := str a "mime", size := nat a "size" } }

def signupOf (j : Json) : Signup :=
  { id := str j "id", email := str j "email", note := str j "note", createdAt := nat j "created_at" }

/-! ## Formatting -/

def pad2 (n : Nat) : String := if n < 10 then s!"0{n}" else toString n

def monthNames : List String := ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

/-- `31 Oct 2026, 14:02` (UTC: the app has no time zone of its own yet). -/
def formatDate (ms : Nat) : String :=
  if ms == 0 then "" else
  let secs := ms / 1000
  let (y, mo, d) := Core.Mime.Date.civilFromDays (Int.ofNat (secs / 86400))
  let t := secs % 86400
  s!"{d} {(monthNames[mo - 1]?).getD "?"} {y}, {pad2 (t / 3600)}:{pad2 (t % 3600 / 60)} UTC"

def formatSize (n : Nat) : String :=
  if n < 1024 then s!"{n} B" else if n < 1024 * 1024 then s!"{n / 1024} KB" else s!"{n / (1024 * 1024)} MB"

def Item.sender (i : Item) : String := if i.fromName.isEmpty then i.fromAddr else i.fromName

def Item.title (i : Item) : String := if i.subject.isEmpty then "(no subject)" else i.subject

/-- The row's markers, as words. -/
def Item.markers (i : Item) : List String :=
  (if i.toRead then ["To read"] else []) ++ (if i.snoozed then ["Snoozed"] else []) ++
  (if i.archived then ["Done"] else []) ++ (if i.junk then ["Junk"] else []) ++
  (if i.hasAttachments then ["Attachment"] else []) ++ (if i.hasInvite then ["Invite"] else []) ++
  (if i.keptForever then ["Kept"] else [])

def trim (s : String) : String := Core.Text.Str.trim s

end LeanMailApp
