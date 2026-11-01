import Server.Pages.Common

/-! # Pages: the operator's admin shells

An administrator's session lives in their own account container, but sign-ups and accounts live
in the operator's. So `/admin/signups` and `/admin/accounts` render here as shells and `app.js`
loads the data with `fetch` from the same paths, which the Worker is to forward to the operator
with `x-admin-account` (a navigation, `Accept: text/html`, comes here; the script's JSON request
goes to the operator). With JavaScript off the shell says so. A JSON request that reaches this
container anyway (no operator routing) gets a JSON 404, so the script can say that instead. In the
operator container itself the operator's JSON routes come first and win. -/

namespace Server.Pages

open Lean Core.Html Server.Router

def adminTabs (cur : String) : Node :=
  el "nav" [("class", "tabs"), ("aria-label", "Admin")] <|
    [("signups", "Sign-ups"), ("accounts", "Accounts")].map fun (k, label) =>
      let a := [("href", s!"/admin/{k}")]
      elt "a" (if k == cur then a ++ [("class", "on"), ("aria-current", "page")] else a) label

def adminShell (kind title : String) (p : Page) : IO Resp := do
  if !p.me.isAdmin then return errorPage 403 (some p.me)
  return page p title (pageKind := "admin") [
    elt "h1" [] "Admin", adminTabs kind,
    el "div" [("id", "admin"), ("data-admin", kind), ("data-src", s!"/admin/{kind}")] [
      elt "p" [("class", "hint js-only")] "Loading…"],
    el "noscript" [] [elt "p" [("class", "flash err")] "The admin pages load their data with JavaScript. Turn it on to review sign-ups and accounts."]]

def adminSignups : Page → IO Resp := adminShell "signups" "Sign-ups"
def adminAccounts : Page → IO Resp := adminShell "accounts" "Accounts"

end Server.Pages
