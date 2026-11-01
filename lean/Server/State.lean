import Lean.Data.Json
import Server.Config
import Server.Db
import Server.Store
import Server.Http

namespace Server

/-- The Worker's `/internal/*` API as a function, so tests record the calls instead of doing
HTTP. `call method path body` is the reply as JSON (`Json.null` for an empty 2xx body), `none`
on any failure. The real one is `Server.Worker.http`. -/
structure WorkerClient where
  call : String → String → Lean.Json → IO (Option Lean.Json)

/-- Google's HTTPS endpoints (OAuth token, Gmail API) as one function, so tests answer with a
fake Google. The real one is libcurl (`Server.Http.request`). -/
structure GmailApi where
  request : Core.Http.Request → IO Core.Http.Response

def GmailApi.http : GmailApi := ⟨Server.Http.request⟩

/-- Any other outbound HTTPS (the Cloudflare API, a `List-Unsubscribe` one-click POST) as one
function, so tests answer with a fake. The real one is libcurl. -/
structure WebApi where
  request : Core.Http.Request → IO Core.Http.Response

def WebApi.http : WebApi := ⟨Server.Http.request⟩

/-- Everything a request handler or a job needs: the config, the one database connection, the
object store holding raw mail and the Worker client. -/
structure State where
  config : Config
  db : Db.Db
  store : Store
  worker : WorkerClient
  startedAt : Nat
  schemaVersion : Nat
  fts5 : Bool
  gmail : GmailApi := GmailApi.http
  /-- The Cloudflare API (`Server.Routing`). -/
  cloudflare : WebApi := WebApi.http
  /-- Everything else outbound: the one-click unsubscribe. -/
  web : WebApi := WebApi.http
  /-- The optional AI's chat-completions endpoint (`Server.AI`); tests answer with a fake model. -/
  ai : WebApi := WebApi.http

end Server
