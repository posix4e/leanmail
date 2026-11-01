import Std.Http
import Std.Async
import Lean.Data.Json
import Core
import Server.Config
import Server.Log
import Server.Db
import Server.Schema
import Server.Http
import Server.Crypto
import Server.State
import Server.Store
import Server.R2
import Server.Router
import Server.Api
import Server.Spool
import Server.Routes
import Server.Worker
import Server.Jobs
import Server.Health

/-! # leanmail-server

The container's process: one `Std.Http` server on `PORT`, one SQLite file under `DATA_DIR`,
JSON-lines logs on stdout, SIGTERM for a clean stop, and the job timer (`Server.Jobs.loop`) on
its own thread. Every response may carry `X-Next-Wake: <seconds>`. M0 serves `/health` and `/internal/health`;
the API, the pages and the jobs arrive milestone by milestone. -/

open Std Std.Async Std.Http Lean

namespace Server

def headerValue (req : Request Body.Stream) (name : String) : Option String :=
  (req.line.headers.get? (Header.Name.ofString! name)).map toString

def segments (req : Request Body.Stream) : List String :=
  req.line.uri.pathOrRoot.toDecodedSegments.toList.filter (· ≠ "")

/-- Request bodies are read whole, up to this. -/
def bodyMax : Nat := 32 * 1024 * 1024

/-- The `Std.Http` request as the router's plain record. -/
def toReq (req : Request Body.Stream) : ContextAsync Router.Req := do
  let method := toString req.line.method
  let query := req.line.uri.query.toArray.toList.filterMap fun (k, v) =>
    k.decode.map fun k => (k, (v.bind (·.decode)).getD "")
  let headers := req.line.headers.toList.map fun (k, v) => ((toString k).toLower, toString v)
  let body : ByteArray ←
    if method == "GET" || method == "HEAD" then pure .empty
    else req.body.readAll (maximumSize := some bodyMax.toUInt64)
  return { method, path := segments req, query, headers, body }

def ofResp (r : Router.Resp) : ContextAsync (Response Body.Any) := do
  let status := (Status.ofCode none r.status.toUInt16).getD .internalServerError
  let b := r.headers.foldl (init := Response.withStatus status) fun b (k, v) => (b.header? k v).getD b
  b.fromBytes r.body

def route (st : State) (r : Router.Req) : IO Router.Resp := do
  match r.method, r.path with
  | "GET", ["health"] => return Router.Resp.ok (← Health.json st)
  | "GET", ["internal", "health"] =>
    if Router.internalOk st r then return Router.Resp.ok (← Health.internalJson st)
    else return Router.Resp.error 401 "unauthorized"
  | _, _ => Router.dispatch st (Routes.all st.config) r

/-- Every response carries `X-Next-Wake` when there is a next wake to schedule (`Jobs.nextWake`). -/
def handle (st : State) (req : Request Body.Stream) : ContextAsync (Response Body.Any) := do
  let r ← toReq req
  let resp ← route st r
  ofResp (← Jobs.withNextWake st resp)

def onFailure (e : IO.Error) : Async Unit :=
  Log.warn "http.failure" [("error", Json.str (toString e))]

def boot : IO State := do
  let config ← Config.fromEnv
  Log.setLevel config.logLevel
  IO.FS.createDirAll config.dataDir
  let db ← Db.openFile config.dbPath
  let schemaVersion ← Schema.migrate db
  let fts5 ← Schema.fts5Works db
  let store ← if config.hasR2 then pure (Store.r2 config) else do
    Log.warn "store.memory" [("reason", Json.str "R2_BUCKET or R2_ENDPOINT unset")]
    Store.memory
  Log.info "boot" [("role", Json.str config.role), ("account", Json.str config.accountId),
                   ("schema", Json.num schemaVersion), ("fts5", Json.bool fts5), ("sqlite", Json.str Db.version)]
  if config.devBypass then
    Log.warn "auth.dev_bypass" [("reason", Json.str "DEV_AUTH_BYPASS=1: any request with x-account-id is an admin session")]
  else if (← IO.getEnv "DEV_AUTH_BYPASS") == some "1" then
    Log.error "auth.dev_bypass_refused" [("reason", Json.str "PUBLIC_ORIGIN is not a localhost origin")]
  let st : State := { config, db, store, worker := Worker.http config, startedAt := ← IO.monoMsNow, schemaVersion, fts5 }
  -- the spool never waits for a wake: drain whatever arrived while the container slept
  try
    let n ← Spool.drain st
    Log.info "spool.boot_drain" [("drained", Json.num n)]
  catch e => Log.error "spool.boot_drain_failed" [("error", Json.str (toString e))]
  return st

def run (st : State) : Async Unit := do
  let addr : Net.SocketAddress := .v4 { addr := Net.IPv4Addr.ofParts 0 0 0 0, port := st.config.port }
  let handler := Server.Handler.ofFns (handle st) onFailure
  let server ← Server.serve addr handler { maxBodySize := 32 * 1024 * 1024 }
  Log.info "listening" [("port", Json.num st.config.port.toNat)]
  let term ← Signal.Waiter.mk .sigterm false
  let int ← Signal.Waiter.mk .sigint false
  let _ ← Selectable.one #[
    .case term.selector (fun _ => pure ()),
    .case int.selector (fun _ => pure ())
  ]
  Log.info "stopping" []
  server.shutdownAndWait
  Log.info "stopped" []

end Server

def main : IO UInt32 := do
  let st ← Server.boot
  -- the 60-second job timer while awake (`Server.Jobs.loop`); lanes share leases with the wakes
  let stop ← IO.mkRef false
  let jobs ← IO.asTask (Server.Jobs.loop st stop) Task.Priority.dedicated
  (Server.run st).block
  -- SIGTERM: let an in-flight lane finish, for at most 60 s; an unfinished one's lease expires
  stop.set true
  let deadline := (← IO.monoMsNow) + 60000
  repeat
    if (← IO.hasFinished jobs) || (← IO.monoMsNow) ≥ deadline then break
    IO.sleep 200
  return 0
