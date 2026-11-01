import Core.Http

namespace Server.Http

/-! # Outbound HTTPS over libcurl (`ffi/c/lm_shim.c`). Used by the server and the CLI.

A `safe` request (`Core.Http.Request.safe`) runs under the shim's safe profile: `https` only,
no redirects, no proxy, and a socket opener that refuses loopback, private, link-local, CGNAT,
multicast and reserved addresses (IPv4 and IPv6, mapped forms included), so a name that resolves
inward is never connected to; it fails with `http: blocked: not a public address`. Every
response is capped (16 MiB for a safe request, 64 MiB otherwise; 200 header lines). A URL or
header with NUL, CR or LF is refused here, before libcurl sees it. -/

/-- What the shim returns. Field order matters: two objects, then one 4-byte scalar. -/
structure RawResponse where
  headers : Array String
  body : ByteArray
  status : UInt32

@[extern "lm_http_request"]
opaque requestRaw : @& String → @& String → @& (Array String) → @& ByteArray → UInt32 → UInt32 → IO RawResponse

/-- `curl_global_init`, once. `Server.Main.boot` calls it before any thread makes a request; a
request made without it (the CLI) initializes libcurl itself. -/
@[extern "lm_http_init"]
opaque init : IO Unit

/-- NUL, CR or LF: header injection or a truncated C string. -/
def hasCtl (s : String) : Bool := s.toList.any fun c => c == '\x00' || c == '\r' || c == '\n'

def request (r : Core.Http.Request) : IO Core.Http.Response := do
  if hasCtl r.url || hasCtl r.method then throw (IO.userError "http: control character in the URL")
  if r.headers.any (fun (k, v) => hasCtl k || hasCtl v || k.contains ':') then
    throw (IO.userError "http: control character in a header")
  let lines := (r.headers.map fun (k, v) => s!"{k}: {v}").toArray
  let raw ← requestRaw r.method r.url lines r.body r.timeoutMs (if r.safe then 1 else 0)
  return { status := raw.status
           headers := raw.headers.toList.filterMap Core.Http.splitHeaderLine
           body := raw.body }

instance : Core.Http.Client IO := ⟨request⟩

end Server.Http
