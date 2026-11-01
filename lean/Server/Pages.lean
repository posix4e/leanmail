import Server.Pages.Common
import Server.Pages.Inbox
import Server.Pages.Message
import Server.Pages.Settings
import Server.Pages.Signin
import Server.Pages.Admin
import Server.Pages.Compose
import Server.Pages.GmailSettings
import Server.Actions
import Server.Pages.Invite
import Server.Pages.Calendar
import Server.Pages.Blocks
import Server.Pages.Routing
import Server.Pages.Expiring
import Server.Pages.AiSettings

/-! # The server-rendered web app

Page routes are `.public` in the table and checked by `guarded` (session, or the dev bypass;
mutations also need the form CSRF rule), so a signed-out browser is redirected to `/signin`
instead of getting a JSON 401. `PATCH /api/msg/:id` (`Server.Actions`) is a normal token route.
These come after the other route lists, so an existing route of the same method and path wins. -/

namespace Server.Pages

open Server.Router

/-- The admin shells answer browser navigations only; any other request (the script's JSON
fetch, an API client) is a plain 404 here, as it was before the pages existed, and the Worker is
to send those to the operator. -/
def adminRoute (h : Page → IO Resp) : Ctx → Req → IO Resp := fun c req =>
  if wantsHtml req && !wantsJson req then guarded false h c req else pure Resp.notFound

def routes : Array Route := Actions.routes ++ #[
  route "GET" "/" .«public» (fun _ _ => pure (redirect "/inbox")),
  route "GET" "/signin" .«public» (openPage signinGet),
  route "POST" "/signin" .«public» (openPage signinPost),
  route "POST" "/signin/code" .«public» (openPage codePost),
  route "GET" "/inbox" .«public» (guarded false inbox),
  route "GET" "/search" .«public» (guarded false search),
  route "GET" "/m/:id" .«public» (guarded false message),
  route "POST" "/m/:id/action" .«public» (guarded true actionPost),
  route "GET" "/t/:id" .«public» (guarded false threadPage),
  route "GET" "/settings" .«public» (guarded false settings),
  route "POST" "/settings/profile" .«public» (guarded true profilePost),
  route "POST" "/settings/sessions/:id/revoke" .«public» (guarded true sessionRevokePost),
  route "POST" "/settings/tokens" .«public» (guarded true tokenCreatePost),
  route "POST" "/settings/tokens/:id/revoke" .«public» (guarded true tokenRevokePost),
  route "POST" "/signout" .«public» (guarded true signoutPost),
  route "POST" "/signout-everywhere" .«public» (guarded true signoutEverywherePost),
  route "GET" "/admin/signups" .«public» (adminRoute adminSignups),
  route "GET" "/admin/accounts" .«public» (adminRoute adminAccounts)
] ++ composeRoutes ++ gmailRoutes ++ messageJunkRoutes ++ calendarRoutes ++ blocksRoutes ++ routingRoutes ++ expiringRoutes ++ aiRoutes

end Server.Pages
