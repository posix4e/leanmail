import Lean.Data.Json
import Core.Html
import Core.Mime.Date
import Server.Router
import Server.R2
import Server.Log
import Server.Clock

/-! # Pages: the shared frame

Every page is built with `Core.Html` (all user data escaped) and answered by `htmlResp`, which
sets the page CSP, `Referrer-Policy: no-referrer`, `X-Content-Type-Options: nosniff` and
`Cache-Control: no-store`.

Page routes are registered `.public` and checked here (`guarded`), because the router's own
refusal is a JSON 401 and a browser should land on `/signin` instead. `guarded` asks
`Router.authorize` for a `.session` as if the request were a GET, so the router's
`X-Requested-With` rule (which a plain HTML form cannot meet) does not apply, and then applies the
form rule (`formCsrfOk`) itself to mutations: `Origin` equal to `PUBLIC_ORIGIN`, or with no
`Origin` either `Sec-Fetch-Site: same-origin` or `X-Requested-With: LeanMail`. The dev bypass
skips it, as it does for the API. -/

namespace Server.Pages

open Lean Core.Html Server.Db Server.Router

def csp : String :=
  "default-src 'self'; img-src 'self' data:; frame-src 'self'; style-src 'self'; script-src 'self'; form-action 'self'; base-uri 'none'; frame-ancestors 'none'"

def secHeaders : List (String × String) := [
  ("Content-Security-Policy", csp), ("Referrer-Policy", "no-referrer"),
  ("X-Content-Type-Options", "nosniff"), ("Cache-Control", "no-store")]

def htmlResp (status : Nat) (doc : Node) : Resp :=
  { status, headers := ("Content-Type", "text/html; charset=utf-8") :: secHeaders, body := (document doc).toUTF8 }

def redirect (loc : String) (status : Nat := 302) : Resp :=
  { status, headers := [("Location", loc), ("Cache-Control", "no-store")] }

/-! ## Requests -/

def wantsHtml (req : Req) : Bool := ((req.header? "accept").getD "").contains "text/html"

/-- A `fetch` from our script, or an explicit JSON `Accept` without HTML. -/
def wantsJson (req : Req) : Bool :=
  req.header? "x-requested-with" == some "LeanMail" ||
    (let a := (req.header? "accept").getD ""; a.contains "application/json" && !a.contains "text/html")

private def trimSlash (s : String) : String := if s.endsWith "/" then String.ofList s.toList.dropLast else s

/-- The CSRF rule for a cookie-authenticated form POST (see the module doc). -/
def formCsrfOk (c : Config) (req : Req) : Bool :=
  match req.header? "origin" with
  | some o => trimSlash o == c.origin
  | none => req.header? "sec-fetch-site" == some "same-origin" || req.header? "x-requested-with" == some "LeanMail"

private def hexNibble (c : Char) : Option Nat :=
  if '0' ≤ c && c ≤ '9' then some (c.toNat - 48)
  else if 'a' ≤ c && c ≤ 'f' then some (c.toNat - 87)
  else if 'A' ≤ c && c ≤ 'F' then some (c.toNat - 55)
  else none

/-- `application/x-www-form-urlencoded` (and query) decoding: `+` is a space, `%XX` a byte. -/
def urlDecode (s : String) : String := Id.run do
  let cs := s.toList.toArray
  let mut out : ByteArray := .empty
  let mut i := 0
  while i < cs.size do
    let c := cs[i]!
    if c == '+' then out := out.push 32; i := i + 1
    else if c == '%' && i + 2 < cs.size then
      match hexNibble cs[i+1]!, hexNibble cs[i+2]! with
      | some h, some l => out := out.push (h * 16 + l).toUInt8; i := i + 3
      | _, _ => out := out ++ c.toString.toUTF8; i := i + 1
    else
      out := out ++ c.toString.toUTF8; i := i + 1
  return (String.fromUTF8? out).getD (String.ofList (s.toList.map fun c => if c == '+' then ' ' else c))

def parseForm (body : ByteArray) : List (String × String) :=
  let s := (String.fromUTF8? body).getD ""
  (s.splitOn "&").filterMap fun kv =>
    if kv.isEmpty then none else
    match kv.splitOn "=" with
    | k :: rest => some (urlDecode k, urlDecode ("=".intercalate rest))
    | [] => none

def formField (f : List (String × String)) (k : String) : String := (f.lookup k).getD ""

/-- `/inbox?filter=x&before=y`, skipping empty values. -/
def href (path : String) (q : List (String × String)) : String :=
  let q := q.filter (!·.2.isEmpty)
  if q.isEmpty then path else path ++ "?" ++ "&".intercalate (q.map fun (k, v) => R2.uriEncode k ++ "=" ++ R2.uriEncode v)

/-! ## Dates (UTC on the server; `app.js` rewrites `<time data-ts>` to local time) -/

def dayMs : Nat := 86400000
def months : Array String := #["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
def weekdays : Array String := #["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
def pad2 (n : Nat) : String := if n < 10 then s!"0{n}" else toString n

def ymd (ms : Nat) : Int × Nat × Nat := Core.Mime.Date.civilFromDays (ms / dayMs : Nat)
def weekday (ms : Nat) : String := weekdays[(ms / dayMs + 4) % 7]!
def hhmm (ms : Nat) : String := let r := ms % dayMs; s!"{pad2 (r / 3600000)}:{pad2 (r / 60000 % 60)}"

def iso (ms : Nat) : String :=
  let (y, m, d) := ymd ms
  let r := ms % dayMs
  s!"{y}-{pad2 m}-{pad2 d}T{pad2 (r / 3600000)}:{pad2 (r / 60000 % 60)}:{pad2 (r / 1000 % 60)}Z"

/-- `Thu, 10 Sep 2026 09:00 UTC`. -/
def fullDate (ms : Nat) : String :=
  let (y, m, d) := ymd ms
  s!"{weekday ms}, {d} {months[m - 1]!} {y} {hhmm ms} UTC"

/-- A day heading: `Today`, `Yesterday`, `Thu 10 Sep`, or with the year when it is another year. -/
def dayLabel (ms now : Nat) : String :=
  let day := ms / dayMs
  let today := now / dayMs
  if day == today then "Today" else if day + 1 == today then "Yesterday" else
  let (y, m, d) := ymd ms
  let (ny, _, _) := ymd now
  if y == ny then s!"{weekday ms} {d} {months[m - 1]!}" else s!"{weekday ms} {d} {months[m - 1]!} {y}"

/-- The time of a row: the clock today, else the date. -/
def shortTime (ms now : Nat) : String :=
  if ms / dayMs == now / dayMs then hhmm ms else
  let (y, m, d) := ymd ms
  let (ny, _, _) := ymd now
  if y == ny then s!"{d} {months[m - 1]!}" else s!"{d} {months[m - 1]!} {y}"

def timeEl (ms : Nat) (label : String) (kind : String := "short") : Node :=
  el "time" [("datetime", iso ms), ("data-ts", toString ms), ("data-fmt", kind), ("title", fullDate ms)] [txt label]

def humanSize (n : Nat) : String :=
  if n < 1024 then s!"{n} B" else if n < 1024 * 1024 then s!"{(n + 512) / 1024} KB"
  else s!"{(n * 10 / (1024 * 1024)) / 10}.{(n * 10 / (1024 * 1024)) % 10} MB"

/-! ## Who is signed in -/

structure Me where
  email : String := ""
  displayName : String := ""
  isAdmin : Bool := false
  deriving Inhabited

def loadMe (st : State) (p : Principal) : IO Me := do
  let row ← st.db.queryOne "SELECT email, display_name, is_admin FROM account WHERE id = 1"
  let email := (row.map (·.text "email")).getD ""
  return { email, displayName := (row.map (·.text "display_name")).getD ""
           isAdmin := p == .dev || (row.map (·.bool "is_admin")).getD false || st.config.isAdminEmail email }

/-- What a page handler gets: the router context (with the session's principal), the request,
the account, and the current search text for the header box. -/
structure Page where
  c : Ctx
  req : Req
  me : Me
  q : String := ""

/-! ## The frame -/

def head (title : String) : Node :=
  el "head" [] [
    el "meta" [("charset", "utf-8")],
    el "meta" [("name", "viewport"), ("content", "width=device-width, initial-scale=1")],
    el "meta" [("name", "referrer"), ("content", "no-referrer")],
    el "meta" [("name", "color-scheme"), ("content", "light dark")],
    elt "title" [] s!"LeanMail · {title}",
    el "link" [("rel", "stylesheet"), ("href", "/static/app.css")],
    el "script" [("src", "/static/app.js"), ("defer", "defer")]]

def accountMenu (me : Me) : Node :=
  let who := if me.displayName.isEmpty then (if me.email.isEmpty then "Account" else me.email) else me.displayName
  el "details" [("class", "menu")] [
    el "summary" [("aria-label", "Account menu")] [txt who],
    el "div" [("class", "menu-panel")] [
      when (!me.email.isEmpty) (elt "p" [("class", "menu-email")] me.email),
      elt "a" [("href", "/calendar")] "Calendar",
      elt "a" [("href", "/expiring")] "Expiring",
      elt "a" [("href", "/settings")] "Settings",
      elt "a" [("href", "/settings/routing")] "Routing",
      elt "a" [("href", "/settings/gmail")] "Gmail",
      elt "a" [("href", "/settings/blocks")] "Blocked senders",
      when me.isAdmin (elt "a" [("href", "/admin/signups")] "Admin"),
      el "form" [("method", "post"), ("action", "/signout")] [elt "button" [("type", "submit")] "Sign out"]]]

def header (me : Option Me) (q : String) : Node :=
  el "header" [("class", "top")] [
    el "div" [("class", "bar")] ([
      elt "a" [("class", "brand"), ("href", "/inbox")] "LeanMail"] ++
      (match me with
       | some me => [
          el "form" [("class", "search"), ("action", "/search"), ("method", "get"), ("role", "search")] [
            el "input" [("type", "search"), ("name", "q"), ("value", q), ("placeholder", "Search mail"),
                        ("autocomplete", "off"), ("aria-label", "Search mail"), ("id", "q")]],
          accountMenu me]
       | none => [])),
    el "div" [("id", "live"), ("class", "live"), ("hidden", "hidden"), ("aria-live", "polite")] []]

/-- A full page. `me := none` for the signed-out pages. -/
def layout (title : String) (me : Option Me) (body : List Node) (q : String := "") (pageKind : String := "") : Node :=
  el "html" [("lang", "en")] [
    head title,
    el "body" [("data-page", pageKind)] [
      header me q,
      el "main" [("class", "wrap"), ("id", "main")] body]]

def page (p : Page) (title : String) (body : List Node) (pageKind : String := "") (status : Nat := 200) : Resp :=
  htmlResp status (layout title (some p.me) body p.q pageKind)

/-- The 404, 403 and 500 pages. -/
def errorDoc (status : Nat) (me : Option Me) : Node :=
  let (title, msg) := match status with
    | 404 => ("Not found", "There is nothing here. It may have been deleted, or the link is wrong.")
    | 403 => ("Forbidden", "This request was refused. Reload the page and try again.")
    | 400 => ("Bad request", "That request did not make sense.")
    | _ => ("Something went wrong", "The server failed to answer. Try again in a moment.")
  layout title me [el "section" [("class", "error")] [
    elt "h1" [] title, elt "p" [] msg, el "p" [] [elt "a" [("href", "/inbox")] "Back to the inbox"]]] (pageKind := "error")

def errorPage (status : Nat) (me : Option Me := none) : Resp := htmlResp status (errorDoc status me)

def notFoundPage (p : Page) : Resp := errorPage 404 (some p.me)

/-- A page route: the session (or the dev bypass) checked here; `mutating` routes also need
`formCsrfOk`. On no session an HTML request is sent to `/signin`; anything else gets the router's
refusal. A thrown error becomes the HTML 500 page. -/
def guarded (mutating : Bool) (h : Page → IO Resp) : Ctx → Req → IO Resp := fun c req => do
  match ← Router.authorize c.st .session { req with method := "GET" } with
  | .error r =>
    if r.status == 401 && (wantsHtml req || (mutating && !wantsJson req)) then
      return redirect "/signin" (if mutating then 303 else 302)
    return r
  | .ok (principal, extra) =>
    if mutating && principal != .dev && !formCsrfOk c.st.config req then return errorPage 403
    let c := { c with principal, auth := .session }
    let me ← loadMe c.st principal
    let q := (req.param? "q").getD ""
    let resp ← try h { c, req, me, q } catch e => do
      Log.error "page.failed" [("path", Json.str ("/" ++ "/".intercalate req.path)), ("error", Json.str (toString e))]
      pure (errorPage 500 (some me))
    return { resp with headers := extra ++ resp.headers }

/-- A signed-out page: no session needed, errors still render as HTML. -/
def openPage (h : Ctx → Req → IO Resp) : Ctx → Req → IO Resp := fun c req => do
  try h c req catch e => do
    Log.error "page.failed" [("path", Json.str ("/" ++ "/".intercalate req.path)), ("error", Json.str (toString e))]
    pure (errorPage 500)

/-- A small form with one submit button (POST). -/
def postButton (action label : String) (cls : String := "") (hidden : List (String × String) := []) : Node :=
  el "form" [("method", "post"), ("action", action), ("class", "inline")]
    ((hidden.map fun (k, v) => el "input" [("type", "hidden"), ("name", k), ("value", v)]) ++
      [elt "button" [("type", "submit"), ("class", cls)] label])

def flash (kind msg : String) : Node := elt "p" [("class", s!"flash {kind}"), ("role", "status")] msg

end Server.Pages
