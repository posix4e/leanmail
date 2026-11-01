import LeanMailApp.View.Common

namespace LeanMailApp.View
open ElmLean

def settings (m : Model) : Html Msg :=
  let s := m.session.getD { origin := "", cred := .dev }
  .screen "Settings" (some .back)
    [ .list
      ([ .section "Account" ""
          [ .identified "account-email" (.labeled "Email" (if m.accountEmail.isEmpty then "—" else m.accountEmail))
          , .identified "server-origin" (.labeled "Server" s.origin)
          , .labeled "Signed in with" (match s.cred with | .dev => "Dev account" | .token _ => "API token") ] ] ++
       (if m.isAdmin then [.section "Operator" "" [.identified "admin-signups" (Html.navigates (.button "Sign-ups" .plain (some .admin)))]] else []) ++
       [ .section "" "Signing out deletes the token from this device's Keychain. Revoke it on the web to end it everywhere."
          [ .identified "sign-out" (.button "Sign out" .destructive (some .signOut)) ] ]) ]
    (largeTitle := false) (depth := 1)

def admin (m : Model) : Html Msg :=
  .screen "Sign-ups" (some .back)
    [ .list
      ([ .section "" "" (spinner m "Working…" ++ errorText m "admin-error" ++ noticeText m "admin-notice") ] ++
       (if m.signups.isEmpty then [.section "" "" [.text "No sign-ups are waiting." .footnote]]
        else m.signups.map fun s =>
          .section s.email (if s.note.isEmpty then formatDate s.createdAt else s!"{s.note} · {formatDate s.createdAt}")
            [ act m s!"approve-{s.id}" "Approve" .primary (.approve s.id)
            , act m s!"reject-{s.id}" "Reject" .destructive (.reject s.id) ])) ]
    (onRefresh := some .admin) (largeTitle := false) (depth := 2)

end LeanMailApp.View
