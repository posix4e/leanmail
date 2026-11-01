import Server.Pages.Common
import Server.Operator

/-! # Pages: sign-up

`GET /signup` asks for an email and an optional note for the operator. Behind the Worker the form
posts to the Worker's `/auth/signup`, which hands the operator container the JSON
`{email, note}` and answers the form with a 303 to `/signup?sent=1` (`worker/src/auth.ts`). On a
bare localhost run (no Worker) it posts to `POST /signup` here, which runs `Operator.signup`
itself in the operator container. Either way the thanks page says the same thing whether or not
the address already has an account. With `SIGNUP_MODE=closed` the page says sign-ups are closed
and has no form. The Worker sends a signed-out `GET`/`POST /signup` to the operator container. -/

namespace Server.Pages

open Lean Core.Html Server.Db Server.Router

def signupAction (c : Config) : String := if c.bareDev then "/signup" else "/auth/signup"

def signupClosed (c : Config) : Bool := c.signupMode == "closed"

def signupDoc (c : Config) (sent : Bool) (notice : Node := empty) : Node :=
  layout "Sign up" none (pageKind := "signup") [
    el "section" [("class", "card narrow")] (
      if signupClosed c then [
        elt "h1" [] "Sign-ups are closed",
        elt "p" [] "This LeanMail install is not taking new accounts. Ask its operator if you need one.",
        el "p" [("class", "hint")] [elt "a" [("href", "/signin")] "Sign in"]]
      else if sent then [
        elt "h1" [] "Thanks",
        elt "p" [] (if c.signupMode == "open" then
            "Your account is being set up. A welcome mail with a sign-in link is on its way to that address."
          else "The operator approves each sign-up by hand. Once yours is approved, a welcome mail with a sign-in link arrives at that address."),
        el "p" [("class", "hint")] [elt "a" [("href", "/signin")] "Back to sign in"]]
      else [
        elt "h1" [] "Sign up",
        notice,
        elt "p" [] (if c.signupMode == "open" then "Ask for an account. It is set up at once and a welcome link is mailed to you."
          else "Ask for an account. The operator of this install approves each sign-up by hand."),
        el "form" [("method", "post"), ("action", signupAction c), ("class", "stack"), ("id", "signup-form")] [
          el "label" [("class", "field")] [
            elt "span" [] "Email",
            el "input" [("type", "email"), ("name", "email"), ("required", "required"), ("autocomplete", "email"),
                        ("autofocus", "autofocus"), ("maxlength", "320"), ("id", "signup-email")]],
          el "label" [("class", "field")] [
            elt "span" [] "A note for the operator (optional)",
            el "textarea" [("name", "note"), ("rows", "3"), ("maxlength", "1000"), ("id", "signup-note")] []],
          elt "button" [("type", "submit"), ("class", "primary")] "Ask for an account"],
        el "p" [("class", "hint")] [txt "Already have an account? ", elt "a" [("href", "/signin")] "Sign in"]])]

def signupGet (c : Ctx) (req : Req) : IO Resp := do
  let notice := match req.param? "error" with
    | some "email" => flash "err" "That does not look like an email address."
    | some "rate" => flash "err" "Too many sign-ups from here. Try again in an hour."
    | some _ => flash "err" "The sign-up did not go through. Try again in a moment."
    | none => empty
  let sent := req.param? "sent" == some "1"
  return htmlResp 200 (signupDoc c.st.config sent notice)

/-- `POST /signup` (form): the operator's sign-up, then the thanks page by redirect. -/
def signupPost (c : Ctx) (req : Req) : IO Resp := do
  let cfg := c.st.config
  if !formCsrfOk cfg req then return errorPage 403
  if signupClosed cfg || !cfg.isOperator then return htmlResp 404 (signupDoc { cfg with signupMode := "closed" } false)
  let f := parseForm req.body
  let body := Json.mkObj [("email", Json.str (Core.Text.Str.trim (formField f "email"))), ("note", Json.str (formField f "note"))]
  let r ← Operator.signup c { req with body := body.compress.toUTF8, headers := req.headers.filter (·.1 != "content-type") ++ [("content-type", "application/json")] }
  return match r.status with
    | 202 => redirect "/signup?sent=1" 303
    | 400 => redirect "/signup?error=email" 303
    | 429 => redirect "/signup?error=rate" 303
    | _ => redirect "/signup?error=failed" 303

def signupRoutes : Array Route := #[
  route "GET" "/signup" .«public» (openPage signupGet),
  route "POST" "/signup" .«public» (openPage signupPost)
]

end Server.Pages
