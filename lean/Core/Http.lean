namespace Core.Http

/-! # The one host seam for outbound HTTP

The server and the CLI implement it over libcurl (`Server.Http`); the native app implements it over
LeanNative's `http.request` capability. Everything above this interface is pure. -/

structure Request where
  method : String := "GET"
  url : String
  headers : List (String × String) := []
  body : ByteArray := .empty
  timeoutMs : UInt32 := 30000
  deriving Inhabited

structure Response where
  status : UInt32
  headers : List (String × String)
  body : ByteArray
  deriving Inhabited

namespace Response

def bodyText (r : Response) : String :=
  String.fromUTF8! r.body

/-- The first header with this name, compared case-insensitively. -/
def header? (r : Response) (name : String) : Option String :=
  let n := name.toLower
  (r.headers.find? fun (k, _) => k.toLower == n).map (·.2)

def isOk (r : Response) : Bool := 200 ≤ r.status && r.status < 300

end Response

/-- A client is anything that can perform a request in some monad. -/
class Client (m : Type → Type) where
  request : Request → m Response

/-- Split a `Name: value` header line as libcurl and URLSession hand them back. -/
def splitHeaderLine (line : String) : Option (String × String) :=
  match line.splitOn ":" with
  | [] => none
  | [_] => none
  | k :: rest => some (k.trimAscii.toString, (":".intercalate rest).trimAscii.toString)

end Core.Http
