import LeanMailApp.View.Common

namespace LeanMailApp.View
open ElmLean

def signin (m : Model) : Html Msg :=
  let server := .section "Server" "Your LeanMail server: mail.leanmail.io, or your own."
    [.identified "origin" (.input .plain "Server" "https://mail.leanmail.io" m.originInput Msg.originInput none)]
  let byCode :=
    if m.codeSent then
      [ .section "Code" s!"Sent to {trim m.email}. It works once and expires in 15 minutes."
          [ .identified "code" (.input .number "Code" "6 digits" m.code Msg.code (some .redeem))
          , act m "redeem" "Sign in" .primary .redeem
          , .identified "restart" (.button "Use another email" .plain (some .restart)) ] ]
    else
      [ .section "Email" "We mail you a sign-in code."
          [ .identified "email" (.input .plain "Email" "you@example.com" m.email Msg.email (some .sendCode))
          , act m "send-code" "Send code" .primary .sendCode ] ]
  let byToken :=
    [ .section "API token" "Mint one in Settings on the web; it is kept in this device's Keychain."
        [ .identified "token" (.input .secure "Token" "lm1…" m.tokenInput Msg.tokenInput (some .signIn))
        , act m "sign-in" "Sign in" .primary .signIn ] ]
  let dev :=
    if m.debug then
      [ .section "Development" "Debug builds only: a bare server with DEV_AUTH_BYPASS=1 (x-account-id: dev)."
          ([ .identified "dev-account" (.toggle "Dev account" m.devAccount Msg.devAccount) ] ++
           (if m.devAccount then [act m "dev-sign-in" "Sign in as dev" .primary .signIn] else [])) ]
    else []
  let mode := [ .section "" "" [.identified "token-mode" (.toggle "Use API token" m.tokenMode Msg.tokenMode)] ]
  .screen "LeanMail" none
    [ .list ([server] ++ (if m.tokenMode then byToken else byCode) ++ mode ++ dev ++
             [.section "" "" (spinner m "Signing in…" ++ errorText m "signin-error" ++ noticeText m "signin-notice")]) ]

end LeanMailApp.View
