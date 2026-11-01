import Core.Http
import Core.Mime.Date
import Server.Config
import Server.Crypto
import Server.Http
import Server.Clock
import Server.Log
import Server.Store

namespace Server.R2

/-! # R2 over the S3 API, signed with AWS Signature Version 4

Path-style URLs (`https://<endpoint>/<bucket>/<key>`), region `auto`, service `s3`. R2 refuses
`UNSIGNED-PAYLOAD` on PUT, so every request signs the SHA-256 of its body. The signer is pure and
takes its date, region and host from the caller so the AWS documentation vector can be checked. -/

open Lean

/-- RFC 3986 percent-encoding as SigV4 wants it: unreserved characters kept, everything else as
`%XX` (upper-case hex) over the UTF-8 bytes; `/` kept when `keepSlash`. -/
def uriEncode (s : String) (keepSlash : Bool := false) : String := Id.run do
  let hex := "0123456789ABCDEF".toList.toArray
  let mut out := ""
  for b in s.toUTF8.toList do
    let c := Char.ofNat b.toNat
    if ('A' ≤ c && c ≤ 'Z') || ('a' ≤ c && c ≤ 'z') || ('0' ≤ c && c ≤ '9') ||
        c == '-' || c == '_' || c == '.' || c == '~' || (keepSlash && c == '/') then
      out := out.push c
    else
      out := out.push '%' |>.push hex[(b >>> 4).toNat]! |>.push hex[(b &&& 15).toNat]!
  return out

/-- What a signature covers. `path` is the canonical (already encoded) URI path; `query` is
unencoded pairs; `headers` are the headers to sign other than `host`. -/
structure SignInput where
  method : String
  host : String
  path : String
  query : List (String × String) := []
  headers : List (String × String) := []
  payloadHash : String
  /-- `YYYYMMDDTHHMMSSZ`. -/
  amzDate : String
  region : String
  service : String := "s3"
  accessKeyId : String
  secretAccessKey : String

private def collapseSpaces (s : String) : String :=
  let ws := (s.splitOn " ").filter (· ≠ "")
  " ".intercalate ws

private def sortPairs (xs : List (String × String)) : List (String × String) :=
  (xs.toArray.qsort fun a b => a.1 < b.1 || (a.1 == b.1 && a.2 < b.2)).toList

def canonicalQuery (q : List (String × String)) : String :=
  "&".intercalate <| (sortPairs (q.map fun (k, v) => (uriEncode k, uriEncode v))).map fun (k, v) => k ++ "=" ++ v

/-- Lower-cased names, trimmed and space-collapsed values, sorted; `host` included. -/
def canonicalHeaders (i : SignInput) : List (String × String) :=
  sortPairs <| (("host", i.host) :: i.headers).map fun (k, v) =>
    (k.toLower, collapseSpaces v.trimAscii.toString)

def signedHeaders (i : SignInput) : String :=
  ";".intercalate ((canonicalHeaders i).map (·.1))

def canonicalRequest (i : SignInput) : String :=
  let hs := String.join ((canonicalHeaders i).map fun (k, v) => k ++ ":" ++ v ++ "\n")
  "\n".intercalate [i.method, i.path, canonicalQuery i.query, hs, signedHeaders i, i.payloadHash]

def dateStamp (i : SignInput) : String := String.ofList (i.amzDate.toList.take 8)

def scope (i : SignInput) : String := s!"{dateStamp i}/{i.region}/{i.service}/aws4_request"

def stringToSign (i : SignInput) : String :=
  "\n".intercalate ["AWS4-HMAC-SHA256", i.amzDate, scope i,
    Crypto.sha256Hex (canonicalRequest i).toUTF8]

def signingKey (i : SignInput) : ByteArray :=
  let kDate := Crypto.hmacSha256 ("AWS4" ++ i.secretAccessKey).toUTF8 (dateStamp i).toUTF8
  let kRegion := Crypto.hmacSha256 kDate i.region.toUTF8
  let kService := Crypto.hmacSha256 kRegion i.service.toUTF8
  Crypto.hmacSha256 kService "aws4_request".toUTF8

def signature (i : SignInput) : String :=
  Crypto.toHex (Crypto.hmacSha256 (signingKey i) (stringToSign i).toUTF8)

def authorization (i : SignInput) : String :=
  s!"AWS4-HMAC-SHA256 Credential={i.accessKeyId}/{scope i}, SignedHeaders={signedHeaders i}, Signature={signature i}"

private def pad2 (n : Nat) : String := if n < 10 then s!"0{n}" else toString n

/-- `YYYYMMDDTHHMMSSZ` for epoch milliseconds. -/
def amzDateOf (ms : Nat) : String :=
  let days := ms / 86400000
  let rem := ms % 86400000
  let (y, m, d) := Core.Mime.Date.civilFromDays days
  s!"{y}{pad2 m}{pad2 d}T{pad2 (rem / 3600000)}{pad2 (rem / 60000 % 60)}{pad2 (rem / 1000 % 60)}Z"

/-! ## The client -/

def host (c : Config) : String :=
  let e := c.r2Endpoint
  let e := if e.startsWith "https://" then String.ofList (e.toList.drop 8)
    else if e.startsWith "http://" then String.ofList (e.toList.drop 7) else e
  String.ofList (e.toList.takeWhile (· != '/'))

/-- Sign and send one request against the bucket. `key` is the object key (unencoded). -/
def send (c : Config) (method : String) (key : String) (query : List (String × String) := [])
    (headers : List (String × String) := []) (body : ByteArray := .empty) : IO Core.Http.Response := do
  let h := host c
  let path := "/" ++ uriEncode c.r2Bucket ++ (if key.isEmpty then "" else "/" ++ uriEncode key true)
  let payloadHash := Crypto.sha256Hex body
  let amzDate := amzDateOf (← Clock.nowMs)
  let headers := headers ++ [("x-amz-content-sha256", payloadHash), ("x-amz-date", amzDate)]
  let si : SignInput := { method, host := h, path, query, headers, payloadHash, amzDate,
                          region := c.r2Region, accessKeyId := c.r2AccessKeyId,
                          secretAccessKey := c.r2SecretAccessKey }
  let qs := canonicalQuery query
  let url := "https://" ++ h ++ path ++ (if qs.isEmpty then "" else "?" ++ qs)
  Server.Http.request { method, url, body, timeoutMs := 60000,
                        headers := headers ++ [("Authorization", authorization si)] }

/-- The text between every `<tag>` and `</tag>`, XML's five entities decoded. -/
def xmlValues (xml : String) (tag : String) : Array String := Id.run do
  let parts := xml.splitOn s!"<{tag}>"
  let mut out := #[]
  for p in parts.drop 1 do
    match p.splitOn s!"</{tag}>" with
    | v :: _ :: _ =>
      out := out.push <| ((((v.replace "&lt;" "<").replace "&gt;" ">").replace "&quot;" "\"").replace "&apos;" "'").replace "&amp;" "&"
    | _ => pure ()
  return out

/-- `2026-10-31T12:00:00.000Z` (S3's `LastModified`) → epoch ms. -/
def parseIsoMs (s : String) : Option Nat := do
  let cs := s.toList.toArray
  let num (a b : Nat) : Option Nat := (String.ofList (cs.extract a b).toList).toNat?
  if cs.size < 19 then none
  let y ← num 0 4
  let mo ← num 5 7
  let d ← num 8 10
  let h ← num 11 13
  let mi ← num 14 16
  let sec ← num 17 19
  let ms := if cs.size ≥ 23 && cs[19]! == '.' then (num 20 23).getD 0 else 0
  Core.Mime.Date.toEpochMs y mo d h mi sec ms 0

/-- The `<Contents>` of a ListObjectsV2 answer: each key with its `LastModified`. -/
def listObjects (xml : String) : Array StoreObject :=
  (xml.splitOn "<Contents>").toArray.extract 1 1000000 |>.filterMap fun part =>
    let block := (part.splitOn "</Contents>").headD ""
    match (xmlValues block "Key")[0]? with
    | some k => some { key := k, uploadedAt := (xmlValues block "LastModified")[0]?.bind parseIsoMs }
    | none => none

/-- S3's (and R2's) custom-metadata header prefix. -/
def metaPrefix : String := "x-amz-meta-"

/-- Custom metadata as PUT headers: `x-amz-meta-<key>`, keys lower-cased. -/
def metaHeaders (md : List (String × String)) : List (String × String) :=
  md.map fun (k, v) => (metaPrefix ++ k.toLower, v)

/-- Custom metadata out of GET response headers: every `x-amz-meta-*` header (any case), prefix
stripped, key lower-cased. -/
def metaFromHeaders (hs : List (String × String)) : List (String × String) :=
  hs.filterMap fun (k, v) =>
    let k := k.toLower
    if k.startsWith metaPrefix then some (String.ofList (k.toList.drop metaPrefix.length), v) else none

def store (c : Config) : Store where
  put key body md := do
    let r ← send c "PUT" key (headers := metaHeaders md ++ [("Content-Type", "application/octet-stream")]) (body := body)
    unless r.isOk do throw (IO.userError s!"r2 put {r.status}")
  get key := do
    let r ← send c "GET" key
    if r.status == 404 then return none
    unless r.isOk do throw (IO.userError s!"r2 get {r.status}")
    return some (r.body, metaFromHeaders r.headers)
  delete key := do
    let r ← send c "DELETE" key
    unless r.isOk || r.status == 404 do throw (IO.userError s!"r2 delete {r.status}")
  list pre := do
    let r ← send c "GET" "" (query := [("list-type", "2"), ("prefix", pre), ("max-keys", "1000")])
    unless r.isOk do throw (IO.userError s!"r2 list {r.status}")
    return xmlValues (String.fromUTF8? r.body |>.getD "") "Key"
  listPage pre after max := do
    let q := [("list-type", "2"), ("prefix", pre), ("max-keys", toString (min max 1000))] ++
      (if after.isEmpty then [] else [("start-after", after)])
    let r ← send c "GET" "" (query := q)
    unless r.isOk do throw (IO.userError s!"r2 list {r.status}")
    return listObjects (String.fromUTF8? r.body |>.getD "")

end Server.R2

namespace Server.Store

/-- The R2 bucket named in the config. -/
def r2 (c : Config) : Store := Server.R2.store c

end Server.Store
