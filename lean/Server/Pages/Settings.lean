import Server.Pages.Common
import Server.Pages.Inbox
import Server.Account
import Server.Auth.Session
import Server.Auth.ApiTokens
import Server.Auth.Passkey

/-! # Pages: settings

Profile (the `PATCH /api/account` logic, fed from the form), passkeys (added by `app.js` over
`/api/passkeys`, removed by a form), sessions with revoke, API tokens
with create (shown once) and revoke, and sign out / sign out everywhere. Every form posts to a
`guarded` route, which applies the form CSRF rule. -/

namespace Server.Pages

open Lean Core.Html Server.Db Server.Router

def field (label name value : String) (type_ : String := "text") (extra : List (String × String) := []) : Node :=
  el "label" [("class", "field")] [
    elt "span" [] label,
    el "input" ([("type", type_), ("name", name), ("value", value)] ++ extra)]

def settingsPage (p : Page) (notice : Node := empty) (newToken : Option (String × String) := none) (status : Nat := 200) :
    IO Resp := do
  let st := p.c.st
  let now ← Clock.nowMs
  let acct ← Account.row st
  let cur := match p.c.principal with | .session sid => sid | _ => ""
  let sessions ← st.db.query
    "SELECT id, created_at, last_seen_at, expires_at, ua FROM sessions WHERE revoked_at IS NULL AND expires_at > ? ORDER BY last_seen_at DESC" #[now]
  let tokens ← st.db.query "SELECT id, name, scopes, created_at, last_used_at, revoked_at FROM api_tokens ORDER BY created_at DESC"
  let passkeys ← Auth.Passkey.rows st
  let saved := match p.req.param? "saved" with
    | some "profile" => flash "ok" "Profile saved."
    | some "session" => flash "ok" "Session revoked."
    | some "token" => flash "ok" "Token revoked."
    | some "passkey" => flash "ok" "Passkey removed."
    | some "passkey-added" => flash "ok" "Passkey added."
    | _ => empty
  let profile : Node := match acct with
    | none => elt "p" [("class", "hint")] "This account has not been provisioned yet."
    | some a =>
      el "form" [("method", "post"), ("action", "/settings/profile"), ("class", "stack")] [
        el "p" [("class", "hint")] [txt "Signed in as ", elt "strong" [] (a.text "email")],
        field "Display name" "display_name" (a.text "display_name") "text" [("maxlength", "200"), ("autocomplete", "name")],
        field "Default From address" "default_from" (a.text "default_from") "email" [("placeholder", a.text "email")],
        field "Keep mail for (days, empty for the server default)" "retention_days"
          ((a.nat? "retention_days").map toString |>.getD "") "number" [("min", "1"), ("max", "3650"), ("inputmode", "numeric")],
        elt "button" [("type", "submit"), ("class", "primary")] "Save profile"]
  let sessionRows : List Node := sessions.toList.map fun s =>
    let sid := s.text "id"
    let seen := s.nat "last_seen_at"
    el "li" [("class", "item")] [
      el "div" [] [
        elt "span" [("class", "ua")] (if (s.text "ua").isEmpty then "Unknown browser" else s.text "ua"),
        when (sid == cur) (badge "cur" "This session")],
      el "div" [("class", "meta")] [txt "Last seen ", timeEl seen (shortTime seen now), txt " · started ",
        timeEl (s.nat "created_at") (shortTime (s.nat "created_at") now)],
      when (sid != cur) (postButton s!"/settings/sessions/{sid}/revoke" "Revoke")]
  let tokenRows : List Node := tokens.toList.map fun t =>
    let revoked := (t.nat? "revoked_at").isSome
    el "li" [("class", if revoked then "item revoked" else "item")] [
      el "div" [] [elt "span" [("class", "tname")] (t.text "name"),
        elt "span" [("class", "scopes")] (" ".intercalate (Auth.ApiTokens.scopesOf (t.text "scopes")))],
      el "div" [("class", "meta")] [txt "Created ", timeEl (t.nat "created_at") (shortTime (t.nat "created_at") now),
        match t.nat? "last_used_at" with
        | some u => frag [txt " · last used ", timeEl u (shortTime u now)]
        | none => txt " · never used",
        when revoked (txt " · revoked")],
      when (!revoked) (postButton s!"/settings/tokens/{t.text "id"}/revoke" "Revoke")]
  let passkeyRows : List Node := passkeys.toList.map fun k =>
    el "li" [("class", "item")] [
      el "div" [] [elt "span" [("class", "tname")] (k.text "name"),
        when (k.bool "backup_eligible") (badge "synced" "Synced")],
      el "div" [("class", "meta")] [txt "Added ", timeEl (k.nat "created_at") (shortTime (k.nat "created_at") now),
        match k.nat? "last_used_at" with
        | some u => frag [txt " · last used ", timeEl u (shortTime u now)]
        | none => txt " · never used"],
      postButton s!"/settings/passkeys/{k.text "id"}/remove" "Remove"]
  let shown : Node := match newToken with
    | some (name, tok) => el "div" [("class", "flash ok token-once"), ("role", "status")] [
        el "p" [] [txt "New token ", elt "strong" [] name, txt ". Copy it now; it is not shown again."],
        el "input" [("type", "text"), ("readonly", "readonly"), ("value", tok), ("class", "mono"), ("aria-label", "New API token")]]
    | none => empty
  return page p "Settings" (pageKind := "settings") (status := status) [
    elt "h1" [] "Settings", notice, saved,
    el "section" [("class", "card"), ("id", "profile")] [elt "h2" [] "Profile", profile],
    el "section" [("class", "card"), ("id", "more")] [
      elt "h2" [] "More",
      el "ul" [("class", "links")] [
        el "li" [] [elt "a" [("href", "/settings/routing")] "Mail routing", txt ": domains, addresses and Cloudflare"],
        el "li" [] [elt "a" [("href", "/settings/gmail")] "Gmail", txt ": connection, backfill and mirroring"],
        el "li" [] [elt "a" [("href", "/settings/blocks")] "Blocked senders"],
        el "li" [] [elt "a" [("href", "/settings/ai")] "AI and privileged mail", txt ": triage, summaries, and what is never shared"],
        el "li" [] [elt "a" [("href", "/calendar")] "Calendar"],
        el "li" [] [elt "a" [("href", "/expiring")] "Expiring this week"]]],
    el "section" [("class", "card"), ("id", "passkeys")] [
      elt "h2" [] "Passkeys",
      elt "p" [("class", "hint")] "Sign in with Touch ID, Face ID, Windows Hello or a security key instead of a mailed link.",
      if passkeyRows.isEmpty then elt "p" [("class", "hint")] "No passkeys yet." else el "ul" [("class", "items")] passkeyRows,
      el "div" [("id", "passkey-add"), ("class", "stack"), ("hidden", "hidden")] [
        field "Name" "passkey_name" "" "text" [("maxlength", "100"), ("placeholder", "laptop"), ("id", "passkey-name")],
        elt "button" [("type", "button"), ("id", "passkey-add-btn")] "Add a passkey"],
      elt "p" [("class", "hint"), ("id", "passkey-nojs")] "Adding a passkey needs JavaScript and a browser with passkey support."],
    el "section" [("class", "card"), ("id", "sessions")] [
      elt "h2" [] "Sessions",
      if sessionRows.isEmpty then elt "p" [("class", "hint")] "No browser sessions." else el "ul" [("class", "items")] sessionRows],
    el "section" [("class", "card"), ("id", "tokens")] [
      elt "h2" [] "API tokens",
      elt "p" [("class", "hint")] "For the leanmail CLI, MCP clients and scripts.",
      shown,
      if tokenRows.isEmpty then elt "p" [("class", "hint")] "No tokens yet." else el "ul" [("class", "items")] tokenRows,
      el "form" [("method", "post"), ("action", "/settings/tokens"), ("class", "stack")] [
        field "Name" "name" "" "text" [("maxlength", "100"), ("placeholder", "laptop cli")],
        el "fieldset" [("class", "scopes-pick")] ([elt "legend" [] "Scopes"] ++
          Auth.ApiTokens.allScopes.map fun s =>
            el "label" [("class", "check")] [
              el "input" ([("type", "checkbox"), ("name", "scope"), ("value", s)] ++ (if s == "read" then [("checked", "checked")] else [])),
              txt s]),
        elt "button" [("type", "submit")] "Create token"]],
    el "section" [("class", "card"), ("id", "signout")] [
      elt "h2" [] "Sign out",
      el "div" [("class", "row-buttons")] [
        postButton "/signout" "Sign out",
        postButton "/signout-everywhere" "Sign out everywhere" "danger"]]]

def settings (p : Page) : IO Resp := settingsPage p

def profilePost (p : Page) : IO Resp := do
  let f := parseForm p.req.body
  let ret := Core.Text.Str.trim (formField f "retention_days")
  let retJson : Json := if ret.isEmpty then .null else match ret.toNat? with
    | some n => Json.num n
    | none => Json.str ret
  let body := Json.mkObj [("display_name", Json.str (formField f "display_name")),
    ("default_from", Json.str (formField f "default_from")), ("retention_days", retJson)]
  let r ← Account.patch p.c { p.req with body := body.compress.toUTF8 }
  if r.status == 200 then return redirect "/settings?saved=profile" 303
  let msg := ((Json.parse r.bodyText).toOption.bind fun j => (j.getObjValAs? String "error").toOption).getD "not saved"
  settingsPage p (flash "err" s!"Profile not saved: {msg}.") (status := 400)

def sessionRevokePost (p : Page) : IO Resp := do
  let r ← Auth.Session.revoke p.c p.req
  if r.status != 200 then return notFoundPage p
  return redirect "/settings?saved=session" 303

def tokenCreatePost (p : Page) : IO Resp := do
  let f := parseForm p.req.body
  let name := Core.Text.Str.trim (formField f "name")
  let picked := f.filterMap fun (k, v) => if k == "scope" then some v else none
  if picked.isEmpty || !picked.all Auth.ApiTokens.allScopes.contains then
    return ← settingsPage p (flash "err" "Pick at least one scope.") (status := 400)
  let scopes := Auth.ApiTokens.allScopes.filter picked.contains
  let name := if name.isEmpty then "token" else name
  let (_, tok) ← Auth.ApiTokens.mint p.c.st name scopes
  settingsPage p (newToken := some (name, tok))

def tokenRevokePost (p : Page) : IO Resp := do
  let r ← Auth.ApiTokens.revoke p.c p.req
  if r.status != 200 then return notFoundPage p
  return redirect "/settings?saved=token" 303

/-- The logout handlers clear the cookie; the page follows with a redirect to `/signin`. -/
def signoutWith (h : Ctx → Req → IO Resp) (p : Page) : IO Resp := do
  let r ← h p.c p.req
  let cookie := r.headers.filter (·.1 == "Set-Cookie")
  return { redirect "/signin?out=1" 303 with headers := (redirect "/signin?out=1" 303).headers ++ cookie }

def passkeyRemovePost (p : Page) : IO Resp := do
  if !(← Auth.Passkey.removeId p.c.st (p.c.param "id")) then return notFoundPage p
  return redirect "/settings?saved=passkey" 303

/-- The settings routes `Server.Pages.routes` does not list (registered in `Server.Routes`). -/
def passkeyPageRoutes : Array Route := #[
  route "POST" "/settings/passkeys/:id/remove" .«public» (guarded true passkeyRemovePost)
]

def signoutPost : Page → IO Resp := signoutWith Auth.Session.logout
def signoutEverywherePost : Page → IO Resp := signoutWith Auth.Session.logoutEverywhere

end Server.Pages
