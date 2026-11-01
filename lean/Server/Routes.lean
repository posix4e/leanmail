import Server.Router
import Server.Api
import Server.Account
import Server.Auth.Magic
import Server.Auth.Session
import Server.Auth.ApiTokens
import Server.Operator
import Server.Pages
import Server.Static
import Server.Send
import Server.Drafts
import Server.Gmail
import Server.Calendar
import Server.Junk
import Server.Routing
import Server.Push
import Server.Expiry
import Server.Health
import Server.Mcp
import Server.AI
import Server.Auth.Passkey
import Server.Pages.Signup

/-! # The route table

Everything the container serves through `Router.dispatch`. The operator's routes exist only in
the operator container (`ROLE=operator`), so elsewhere they are plain 404s. `Gmail.routes` comes
first: its `POST /internal/tick` (`Server.Jobs`) replaces the placeholder in `Api.routes`.
`POST /mcp` (`Server.Mcp`) and the AI's routes (`Server.AI`, 404 while the AI is off) are always
in the table. Passkeys (`Server.Auth.Passkey`), the settings page's passkey form route and the
sign-up page (`Server.Pages.Signup`) are listed here beside `Pages.routes`. -/

namespace Server.Routes

def all (c : Config) : Array Router.Route :=
  Gmail.routes ++ Api.routes ++ Mcp.routes ++ AI.routes ++ Calendar.routes ++ Junk.routes ++ Routing.routes ++ Push.routes ++ Expiry.routes ++
    Send.routes ++ Drafts.routes ++ Account.routes ++ Auth.Magic.routes ++ Auth.Session.routes ++ Auth.ApiTokens.routes ++
    Auth.Passkey.routes ++ (if c.isOperator then Operator.routes ++ Routing.operatorRoutes else #[]) ++ Pages.passkeyPageRoutes ++ Pages.signupRoutes ++
    Pages.routes ++ Static.routes

/-- Route a request through the whole table; an unknown path is the HTML 404 page for a
browser (`Pages.notFoundFor`) and the JSON 404 for everyone else. -/
def dispatch (st : State) (r : Router.Req) : IO Router.Resp :=
  Router.dispatch st (all st.config) r (notFound := Pages.notFoundFor st)

end Server.Routes
