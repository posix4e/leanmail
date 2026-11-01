import Server.Router.Types

/-! # Static files

`GET /static/app.css` and `/static/app.js`, read from `WEB_DIR` (default `../web`, which is
`web/` when the server runs from `lean/`; the image sets `/opt/leanmail/web`). Only the files
named in `files` are served, so nothing from the URL reaches the filesystem. They are read at
request time rather than embedded with `include_str`: Lake does not track that dependency (an
edit to `web/` would not rebuild), and the image's build stage holds only `lean/`. -/

namespace Server.Static

open Server.Router

/-- Served name → content type. -/
def files : List (String × String) := [
  ("app.css", "text/css; charset=utf-8"),
  ("app.js", "text/javascript; charset=utf-8")]

def webDir : IO System.FilePath := do
  return ⟨(← IO.getEnv "WEB_DIR").getD "../web"⟩

def serve (c : Ctx) (_ : Req) : IO Resp := do
  let name := c.param "file"
  let some ctype := files.lookup name | return Resp.notFound
  let path := (← webDir) / name
  let body ← try IO.FS.readBinFile path catch _ => return Resp.notFound
  return { status := 200, body, headers := [
    ("Content-Type", ctype), ("Cache-Control", "public, max-age=3600"), ("X-Content-Type-Options", "nosniff")] }

/-- `GET /favicon.ico`: nothing to show, said cheaply (a browser asks on every page). -/
def favicon (_ : Ctx) (_ : Req) : IO Resp :=
  return { status := 204, headers := [("Cache-Control", "public, max-age=86400")] }

def routes : Array Route := #[
  route "GET" "/static/:file" .«public» serve,
  route "GET" "/favicon.ico" .«public» favicon
]

end Server.Static
