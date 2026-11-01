import Core.Http

namespace Server.Http

/-! # Outbound HTTPS over libcurl (`ffi/c/lm_shim.c`). Used by the server and the CLI. -/

/-- What the shim returns. Field order matters: two objects, then one 4-byte scalar. -/
structure RawResponse where
  headers : Array String
  body : ByteArray
  status : UInt32

@[extern "lm_http_request"]
opaque requestRaw : @& String → @& String → @& (Array String) → @& ByteArray → UInt32 → IO RawResponse

def request (r : Core.Http.Request) : IO Core.Http.Response := do
  let lines := (r.headers.map fun (k, v) => s!"{k}: {v}").toArray
  let raw ← requestRaw r.method r.url lines r.body r.timeoutMs
  return { status := raw.status
           headers := raw.headers.toList.filterMap Core.Http.splitHeaderLine
           body := raw.body }

instance : Core.Http.Client IO := ⟨request⟩

end Server.Http
