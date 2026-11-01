import Server.Pages.Common
import Server.Auth.Magic
import Server.Auth.Session

/-! # Pages: sign-in

`/signin` asks for the email. Behind the Worker the form posts to the Worker's `/auth/login`
(which finds the account and asks its container for a link); on a bare localhost run there is no
Worker, so it posts to `POST /signin` here, which does what `/internal/auth/magic` does for this
account's email. Either way the answer never says whether the email is known. The "check your
mail" page carries the 6-digit code form (`POST /signin/code`): the code is spent with
`Auth.Magic.spendCode`, the same check `POST /auth/code` uses, and starts a browser session
instead of minting an API token. The public POSTs apply the form CSRF rule too, so another site
cannot sign a browser in to this account (login CSRF).

Above the form, a "Sign in with a passkey" button (hidden until `app.js` finds WebAuthn) asks the
Worker for a challenge and posts the assertion to `/auth/passkey/login` (`Server.Auth.Passkey`).
Below it, a link to `/signup` unless sign-ups are closed. -/

namespace Server.Pages

open Lean Core.Html Server.Db Server.Router

def signinAction (c : Config) : String := if c.bareDev then "/signin" else "/auth/login"

def codeForm (email : String) (hidden : Bool := false) : Node :=
  el "form" ([("method", "post"), ("action", "/signin/code"), ("class", "stack"), ("id", "code-form")] ++
      (if hidden then [("hidden", "hidden")] else [])) [
    el "input" [("type", "hidden"), ("name", "email"), ("value", email), ("id", "code-email")],
    el "label" [("class", "field")] [
      elt "span" [] "6-digit code from the mail",
      el "input" [("type", "text"), ("name", "code"), ("inputmode", "numeric"), ("pattern", "[0-9]{6}"),
                  ("maxlength", "6"), ("autocomplete", "one-time-code"), ("required", "required"), ("id", "code")]],
    elt "button" [("type", "submit"), ("class", "primary")] "Sign in"]

def signinDoc (c : Config) (notice : Node := empty) : Node :=
  layout "Sign in" none (pageKind := "signin") [
    el "section" [("class", "card narrow")] [
      elt "h1" [] "Sign in",
      notice,
      -- shown by app.js only where the browser has WebAuthn; the email form works without it
      el "div" [("id", "passkey-signin"), ("class", "stack"), ("hidden", "hidden")] [
        elt "button" [("type", "button"), ("class", "primary"), ("id", "passkey-signin-btn")] "Sign in with a passkey",
        elt "p" [("class", "hint or")] "or get a link by mail"],
      el "form" [("method", "post"), ("action", signinAction c), ("class", "stack"), ("id", "email-form")] [
        el "label" [("class", "field")] [
          elt "span" [] "Email",
          el "input" [("type", "email"), ("name", "email"), ("required", "required"), ("autocomplete", "email"),
                      ("autofocus", "autofocus"), ("id", "email")]],
        elt "button" [("type", "submit"), ("class", "primary")] "Email me a sign-in link"],
      el "div" [("id", "code-step"), ("hidden", "hidden")] [
        elt "p" [("class", "hint")] "If that address has an account, a link and a code are on their way.",
        codeForm "" ],
      elt "p" [("class", "hint")] "We mail you a link and a 6-digit code. Either one signs you in once.",
      when (c.signupMode != "closed") (el "p" [("class", "hint")] [txt "No account yet? ", elt "a" [("href", "/signup")] "Sign up"])]]

def checkMailDoc (email : String) (notice : Node := empty) : Node :=
  layout "Check your mail" none (pageKind := "signin") [
    el "section" [("class", "card narrow")] [
      elt "h1" [] "Check your mail",
      notice,
      elt "p" [] "If that address has an account, a sign-in link and a 6-digit code are on their way. The link works once and expires in 15 minutes.",
      codeForm email,
      el "p" [("class", "hint")] [elt "a" [("href", "/signin")] "Use another address"]]]

def signinGet (c : Ctx) (req : Req) : IO Resp := do
  let notice := if req.param? "out" == some "1" then flash "ok" "You are signed out." else empty
  return htmlResp 200 (signinDoc c.st.config notice)

def signinPost (c : Ctx) (req : Req) : IO Resp := do
  if !formCsrfOk c.st.config req then return errorPage 403
  let email := Core.Text.Str.trim (formField (parseForm req.body) "email")
  let some e := normEmail? email | return htmlResp 400 (signinDoc c.st.config (flash "err" "That does not look like an email address."))
  if let some mine ← Auth.Magic.accountEmail c.st then
    if mine.toLower == e then discard <| Auth.Magic.sendLink c.st mine "signin"
  return htmlResp 200 (checkMailDoc e)

def codePost (c : Ctx) (req : Req) : IO Resp := do
  if !formCsrfOk c.st.config req then return errorPage 403
  let f := parseForm req.body
  let email := Core.Text.Str.trim (formField f "email")
  let bad := htmlResp 400 (checkMailDoc email (flash "err" "That code did not work. Check it, or ask for a new one."))
  if !(← Auth.Magic.spendCode c.st email (formField f "code")) then return bad
  let some cookie ← Auth.Session.create c.st ((req.header? "user-agent").getD "") | return bad
  return { redirect "/inbox" 303 with
    headers := (redirect "/inbox" 303).headers ++ [("Set-Cookie", Auth.Session.cookieHeader c.st.config cookie)] }

end Server.Pages
