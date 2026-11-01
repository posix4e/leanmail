import Server
import Core.Query
import Core.Search
import Core.Mime.Date
import Core.Base64

/-! Server tests: the shims and the schema against a real in-memory SQLite. -/

structure Tally where
  passed : Nat := 0
  failed : Nat := 0

def check (t : IO.Ref Tally) (name : String) (ok : Bool) : IO Unit := do
  if ok then
    IO.println s!"ok   {name}"
    t.modify fun x => { x with passed := x.passed + 1 }
  else
    IO.println s!"FAIL {name}"
    t.modify fun x => { x with failed := x.failed + 1 }

open Server

/-- A fake Worker: records every `/internal/*` call and answers `{ok:true}`. -/
def recorder : IO (IO.Ref (Array (String × String × Lean.Json)) × WorkerClient) := do
  let r ← IO.mkRef #[]
  return (r, { call := fun m p j => do
    r.modify (·.push (m, p, j))
    return some (Lean.Json.mkObj [("ok", Lean.Json.bool true)]) })

def hasRow (rows : Array Db.Row) (id : String) : Bool := rows.any (·.text "id" == id)

def serverChecks (t : IO.Ref Tally) : IO Unit := do
  -- warnings and errors still print; the per-message info lines would bury the results
  Log.setLevel "warn"
  -- SigV4: the AWS S3 documentation example (GET Object with a Range header)
  let si : R2.SignInput := {
    method := "GET", host := "examplebucket.s3.amazonaws.com", path := "/test.txt"
    headers := [("Range", "bytes=0-9"),
                ("x-amz-content-sha256", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"),
                ("x-amz-date", "20130524T000000Z")]
    payloadHash := "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    amzDate := "20130524T000000Z", region := "us-east-1"
    accessKeyId := "AKIAIOSFODNN7EXAMPLE", secretAccessKey := "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY" }
  check t "sigv4 canonical request matches the AWS example" <|
    Crypto.sha256Hex (R2.canonicalRequest si).toUTF8 == "7344ae5b7ee6c3e7e6b0fe0640412a37625d1fbfff95c48bbb2dc43964946972"
  check t "sigv4 string to sign has the AWS scope" <|
    R2.stringToSign si == "AWS4-HMAC-SHA256\n20130524T000000Z\n20130524/us-east-1/s3/aws4_request\n7344ae5b7ee6c3e7e6b0fe0640412a37625d1fbfff95c48bbb2dc43964946972"
  check t "sigv4 signature matches the AWS example" <|
    R2.signature si == "f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41"
  check t "sigv4 encodes the list query canonically" <|
    R2.canonicalQuery [("prefix", "in/dev/"), ("list-type", "2")] == "list-type=2&prefix=in%2Fdev%2F"
  check t "amz date formats epoch ms in UTC" (R2.amzDateOf 1369353600000 == "20130524T000000Z")
  check t "r2 list reads keys out of the XML" <|
    R2.xmlValues "<ListBucketResult><Contents><Key>in/dev/A</Key></Contents><Contents><Key>in/dev/a&amp;b</Key></Contents></ListBucketResult>" "Key"
      == #["in/dev/A", "in/dev/a&b"]

  -- query grammar and the MATCH builder
  let fts (s : String) := Core.Search.toFts (Core.Query.parse s)
  check t "a term with a quote yields only balanced, quoted words" <|
    fts "o\"neil test" == some "\"neil test\""
  check t "from: an address is column-scoped and quoted" (fts "from:alex@example.com" == some "from_:\"alex@example.com\"*")
  check t "a bare negation has no MATCH of its own but a NOT IN string" <|
    fts "-spam" == none && Core.Search.negMatch (Core.Query.parse "-spam") == some "\"spam\"*"
  check t "a term and a negation become AND NOT" (fts "hello -spam" == some "\"hello\"* NOT \"spam\"*")
  check t "an exact phrase is one quoted string" (fts "\"exact phrase\"" == some "\"exact phrase\"")
  check t "has:attachment is a filter, not text" <|
    fts "has:attachment" == none && (Core.Query.parse "has:attachment").filters.hasAttachment == some true
  check t "before: is the start of the following UTC day" <|
    (Core.Query.parse "before:2026-01-01").filters.before == some 1767312000000 && fts "before:2026-01-01" == none
  check t "an impossible date stays as free text" ((Core.Query.parse "before:2026-02-30").terms.size > 0)
  check t "short terms are exact, long ones prefix" (fts "ab abc" == some "\"ab\" AND \"abc\"*")
  check t "the tokenizer keeps an address whole and adds its parts" <|
    Core.Query.tokenize "Mail bob@x.com now" == #["bob@x.com", "bob", "x.com", "mail", "com", "now"]

  -- a fresh server: memory db + memory store
  let db ← Db.openMemory
  discard <| Schema.migrate db
  let store ← Store.memory
  -- the M1 checks run as the Worker-less dev setup: DEV_AUTH_BYPASS on a localhost origin
  let st : State := { config := { devBypass := true }, db, store, worker := (← recorder).2, startedAt := 0,
                      schemaVersion := 2, fts5 := true }
  let dir : System.FilePath := "test/fixtures/eml"
  let files := ((← dir.readDir).filter (·.fileName.endsWith ".eml")).map (·.path) |>.qsort (·.toString < ·.toString)
  let mut byName : List (String × String) := []
  let mut i := 0
  for f in files do
    let raw ← IO.FS.readBinFile f
    let m := Core.Mime.parse raw
    let recAt := ((m.rawHeader? "date").bind Core.Mime.Date.parse?).getD 0
    let ulid := Core.Ulid.encode recAt (ByteArray.mk #[0, 0, 0, 0, 0, 0, 0, 0, 0, i.toUInt8])
    store.put s!"raw/dev/{ulid}" raw [("rcpt", "alex@fralex.art"), ("received_at", toString recAt), ("source", "route"), ("size", toString raw.size)]
    store.put s!"in/dev/{ulid}" .empty []
    byName := (f.fileName.getD "", ulid) :: byName
    i := i + 1
  let idOf (n : String) := (byName.lookup n).getD ""
  let n ← Spool.drain st
  let count ← db.query "SELECT count(*) AS n FROM messages"
  check t s!"the spool drains every fixture ({files.size})" (n == files.size && count[0]!.nat "n" == files.size)
  check t "draining consumes the markers and keeps the raw objects" <|
    (← store.list "in/dev/").isEmpty && (← store.list "raw/dev/").size == files.size
  check t "a second drain is a no-op" ((← Spool.drain st) == 0)
  let texts ← db.query "SELECT count(*) AS n FROM message_text"
  check t "every message has a message_text row" (texts[0]!.nat "n" == files.size)
  let r05 ← db.queryOne "SELECT * FROM messages WHERE id = ?" #[idOf "05.eml"]
  check t "the spool ulid becomes the message id, with raw_key and parsed headers" <|
    match r05 with
    | some r => r.text "raw_key" == s!"raw/dev/{idOf "05.eml"}" && r.text "from_addr" == "tomas@labelforge.io" &&
        r.text "from_name" == "Tomas Reyes" && r.text "message_id" == "05@example.test" &&
        r.text "rcpts" == "[\"alex@fralex.art\"]" && r.text "subject" == "Quote for 500 units, matte finish" &&
        (r.text "snippet").startsWith "Could you send the unit price" && (r.nat? "expires_at").isSome
    | none => false
  let r13 ← db.query "SELECT has_attachments FROM messages WHERE id = ?" #[idOf "13.eml"]
  let a13 ← db.query "SELECT filename, mime, inline FROM attachments WHERE message_id = ?" #[idOf "13.eml"]
  check t "attachment metadata is stored and flags the message" <|
    r13[0]!.bool "has_attachments" && a13.size == 1 && a13[0]!.text "filename" == "invoice-2026-0912.pdf" && !a13[0]!.bool "inline"
  let inv ← db.query "SELECT has_invite FROM messages WHERE id = ?" #[idOf "14.eml"]
  check t "a text/calendar part marks an invite" (inv[0]!.bool "has_invite")

  -- dedupe
  let raw01 ← IO.FS.readBinFile (dir / "01.eml")
  let again ← Ingest.ingest st { raw := raw01, key := none, rcpt := some "other@fralex.art", source := "route", receivedAt := 5 }
  let dups ← db.query "SELECT count(*) AS n FROM duplicates"
  let rc ← db.query "SELECT rcpts FROM messages WHERE id = ?" #[idOf "01.eml"]
  check t "re-ingesting the same raw is a duplicate, recorded, with the rcpt merged" <|
    again == .duplicate (idOf "01.eml") && dups[0]!.nat "n" == 1 &&
      rc[0]!.text "rcpts" == "[\"alex@fralex.art\",\"other@fralex.art\"]"
  let altered := (String.fromUTF8! raw01).replace "Three spreads" "Four spreads"
  let other ← Ingest.ingest st { raw := altered.toUTF8, key := none, rcpt := none, source := "route", receivedAt := 6 }
  check t "a reused Message-ID with different content is a new message" <|
    match other with | .stored i => i != idOf "01.eml" | _ => false
  db.exec "UPDATE messages SET deleted = 1 WHERE id = ?" #[other.id]

  -- search
  let s1 ← Search.run st "matte"
  check t "an FTS word from one subject finds exactly that message" (s1.size == 1 && hasRow s1 (idOf "05.eml"))
  let s1b ← Search.run st "MATT"
  check t "a bare term prefix-matches case-insensitively" (s1b.size == 1 && hasRow s1b (idOf "05.eml"))
  let s2 ← Search.run st "from:labelforge.io"
  check t "from:<sender domain> finds the sender's message" (s2.size == 1 && hasRow s2 (idOf "05.eml"))
  let s3 ← Search.run st "matte -units"
  check t "-word excludes a message alongside a term" s3.isEmpty
  let s4 ← Search.run st "-matte" 100
  check t "-word alone excludes only the matching message" (s4.size == files.size - 1 && !hasRow s4 (idOf "05.eml"))
  let s5 ← Search.run st "\"unit price\""
  check t "a phrase matches the body" (s5.size == 1 && hasRow s5 (idOf "05.eml"))
  let s6 ← Search.run st "before:2026-09-01" 100
  check t "before: includes the named day and excludes later mail" <|
    s6.size == 3 && hasRow s6 (idOf "09.eml") && hasRow s6 (idOf "11.eml") && !hasRow s6 (idOf "08.eml")
  let s7 ← Search.run st "after:2026-09-13" 100
  check t "after: starts at the named day" (s7.size == 4 && hasRow s7 (idOf "01.eml") && !hasRow s7 (idOf "13.eml"))
  let s8 ← Search.run st "invoice has:attachment"
  check t "text and a has: filter combine" (s8.size == 1 && hasRow s8 (idOf "13.eml"))
  let s9 ← Search.run st "nordic-press.example"
  check t "a dotted domain as a bare term is found through its parts" <|
    s9.size == 2 && hasRow s9 (idOf "01.eml") && hasRow s9 (idOf "13.eml")
  let s9b ← Search.run st "2026-0912"
  check t "a hyphenated word in a subject is found" (s9b.size == 1 && hasRow s9b (idOf "13.eml"))
  let s9c ← Search.run st "to:wuli.nu"
  check t "to: matches a recipient domain" (s9c.size ≥ 3 && hasRow s9c (idOf "12.eml") && !hasRow s9c (idOf "05.eml"))
  let s10 ← Search.run st "subject:\"logo options\" in:inbox"
  check t "subject: with a quoted phrase" (s10.size == 1 && hasRow s10 (idOf "12.eml"))
  let s11 ← Search.run st "o\"neil (*) AND NOT"
  check t "hostile input does not break the MATCH" (s11.size == 0)

  -- threads
  let reply := "From: Alex <alex@fralex.art>\r\nTo: tomas@labelforge.io\r\nSubject: Sounds good\r\nMessage-ID: <reply-05@fralex.art>\r\nIn-Reply-To: <05@example.test>\r\nReferences: <05@example.test>\r\nDate: Thu, 10 Sep 2026 09:00:00 +0000\r\n\r\nYes, matte please.\r\n"
  let rr ← Ingest.ingest st { raw := reply.toUTF8, key := none, rcpt := none, source := "route", receivedAt := 1788944400000 }
  let th ← db.query "SELECT id, thread_id FROM messages WHERE id IN (?, ?)" #[idOf "05.eml", rr.id]
  check t "a reply joins its parent's thread by In-Reply-To" <|
    th.size == 2 && th[0]!.text "thread_id" == th[1]!.text "thread_id" && th[0]!.text "thread_id" != ""
  let thr ← db.query "SELECT count FROM threads WHERE id = ?" #[th[0]!.text "thread_id"]
  check t "the thread counts both messages" (thr[0]!.nat "count" == 2)
  let other03 := "From: x@y.example\r\nSubject: RE: Fwd: Follow-up   appointment\r\nMessage-ID: <x03@y.example>\r\n\r\nok\r\n"
  let r03 ← Ingest.ingest st { raw := other03.toUTF8, key := none, rcpt := none, source := "route", receivedAt := 1789050000000 }
  let t03 ← db.query "SELECT thread_id FROM messages WHERE id IN (?, ?)" #[idOf "03.eml", r03.id]
  check t "a normalized subject within 7 days joins the thread" (t03.size == 2 && t03[0]!.text "thread_id" == t03[1]!.text "thread_id")
  check t "subject normalization strips reply prefixes" (Ingest.subjectNorm "Re: AW: fwd:  Hello   World" == "hello world")

  -- the API, called directly
  let ctx (params : List (String × String)) : Router.Ctx := { st, account := "dev", auth := .token "read", params }
  let get (path : String) (q : List (String × String) := []) : Router.Req :=
    { method := "GET", path := (path.splitOn "/").filter (· ≠ ""), query := q, headers := [("x-account-id", "dev")] }
  let unauth ← Router.dispatch st Api.routes { method := "GET", path := ["api", "msgs"] }
  check t "an /api request without x-account-id is refused" (unauth.status == 401)
  let wrong ← Router.dispatch st Api.routes { method := "GET", path := ["api", "msgs"], headers := [("x-account-id", "eve")] }
  check t "an /api request for another account is refused" (wrong.status == 403)
  -- paginate the whole inbox through the cursor
  let mut seen : Array (Nat × String) := #[]
  let mut cursor : Option String := none
  let mut pages := 0
  repeat
    let q := [("limit", "4")] ++ (cursor.map (fun c => [("before", c)])).getD []
    let resp ← Router.dispatch st Api.routes (get "/api/msgs" q)
    let j := (Lean.Json.parse resp.bodyText).toOption.getD .null
    let items := (j.getObjValAs? (Array Lean.Json) "items").toOption.getD #[]
    for it in items do
      seen := seen.push ((it.getObjValAs? Nat "received_at").toOption.getD 0, (it.getObjValAs? String "id").toOption.getD "")
    cursor := (j.getObjValAs? String "next").toOption
    pages := pages + 1
    if cursor.isNone || pages > 20 then break
  let total ← db.query "SELECT count(*) AS n FROM messages WHERE deleted = 0"
  let ordered := (List.range (seen.size - 1)).all fun k => let a := seen[k]!; let b := seen[k + 1]!; a.1 > b.1 || (a.1 == b.1 && a.2 > b.2)
  check t s!"the list cursor pages the inbox newest first ({seen.size} in {pages} pages)" <|
    seen.size == total[0]!.nat "n" && pages ≥ 3 && ordered && (seen.map (·.2)).toList.eraseDups.length == seen.size
  let one ← Api.msg (ctx [("id", idOf "13.eml")]) (get "/")
  let oj := (Lean.Json.parse one.bodyText).toOption.getD .null
  check t "GET /api/msg/:id returns the full record" <|
    one.status == 200 && ((oj.getObjValAs? String "text").toOption.map (·.startsWith "Invoice attached. Net 30.")) == some true &&
      ((oj.getObjValAs? (Array Lean.Json) "attachments").toOption.map (·.size)) == some 1 &&
      ((oj.getObjValAs? (Array Lean.Json) "to").toOption.map (·.size)) == some 1
  let html ← Api.msgHtml (ctx [("id", idOf "12.eml")]) (get "/")
  check t "GET /api/msg/:id/html serves the HTML part under a strict CSP" <|
    html.status == 200 && (html.headers.lookup "Content-Type") == some "text/html; charset=utf-8" &&
      (html.headers.lookup "Content-Security-Policy") == some Api.htmlCsp && (html.bodyText.splitOn "logo options").length > 1
  let noHtml ← Api.msgHtml (ctx [("id", idOf "05.eml")]) (get "/")
  check t "a message without HTML gives 404 for /html" (noHtml.status == 404)
  let att ← Api.msgAttachment (ctx [("id", idOf "13.eml"), ("idx", "0")]) (get "/")
  check t "an attachment downloads with its type and a safe filename" <|
    att.status == 200 && (att.headers.lookup "Content-Type") == some "application/pdf" &&
      (att.headers.lookup "Content-Disposition") == some "attachment; filename=\"invoice-2026-0912.pdf\"" &&
      att.body.size > 0 && att.body[0]! == 0x25
  check t "filenames are sanitised" <|
    Api.sanitizeFilename "../\"evil\";\r\n.html" == "__evil____.html" && Api.sanitizeFilename "" == "attachment"
  let rawR ← Api.msgRaw (ctx [("id", idOf "01.eml")]) (get "/")
  check t "GET /api/msg/:id/raw returns the bytes as message/rfc822" (rawR.status == 200 && rawR.body == raw01)
  let thrR ← Router.dispatch st Api.routes (get s!"/api/thread/{th[0]!.text "thread_id"}")
  check t "GET /api/thread/:id lists the thread oldest first" <|
    thrR.status == 200 && (thrR.bodyText.splitOn "\"id\"").length == 3
  let srch ← Router.dispatch st Api.routes (get "/api/search" [("q", "matte")])
  check t "GET /api/search answers in the list shape" (srch.status == 200 && (srch.bodyText.splitOn (idOf "05.eml")).length > 1)
  -- import
  let impId := "01K0000000000000000000IMP1"
  let impBody := "From: a@b.example\r\nSubject: imported\r\nMessage-ID: <imp1@b.example>\r\n\r\nhello there\r\n".toUTF8
  let impHeaders := [("x-account-id", "dev"), ("x-id", impId), ("x-received-at", "1700000000000"),
    ("x-rcpts", "[\"a@fralex.art\",\"b@fralex.art\"]"), ("x-archived-at", "1700000000001"), ("x-expires-at", "never")]
  let impReq : Router.Req := { method := "POST", path := ["api", "import"], body := impBody, headers := impHeaders }
  let imp ← Router.dispatch st Api.routes impReq
  let impRow ← db.queryOne "SELECT * FROM messages WHERE id = ?" #[Core.Ulid.normalize impId]
  check t "POST /api/import stores the raw first and keeps the id, rcpts, archive and forever" <|
    imp.status == 201 && (← store.get s!"raw/dev/{Core.Ulid.normalize impId}").isSome &&
      (match impRow with
       | some r => r.text "source" == "import" && r.nat "archived_at" == 1700000000001 && (r.get "expires_at").isNull &&
           r.text "rcpts" == "[\"a@fralex.art\",\"b@fralex.art\"]"
       | none => false)
  let imp2 ← Router.dispatch st Api.routes impReq
  check t "importing the same id again is a duplicate" (imp2.status == 200)
  -- wake drains a new marker
  let late := "From: late@z.example\r\nSubject: late arrival\r\nMessage-ID: <late@z.example>\r\n\r\nbody\r\n"
  let lateId := Core.Ulid.encode 1790000000000 (ByteArray.mk #[1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  store.put s!"raw/dev/{lateId}" late.toUTF8 []
  store.put s!"in/dev/{lateId}" .empty []
  store.put s!"in/dev/{Core.Ulid.encode 1790000000001 (ByteArray.mk #[9, 9, 9, 9, 9, 9, 9, 9, 9, 9])}" .empty []
  let wk ← Router.dispatch st Api.routes { method := "POST", path := ["internal", "wake"], body := "{}".toUTF8 }
  check t "POST /internal/wake drains new markers, dropping one whose raw is missing" <|
    wk.status == 200 && wk.bodyText == "{\"drained\":2}" && (← store.list "in/dev/").isEmpty &&
      (← db.queryOne "SELECT id FROM messages WHERE id = ?" #[lateId]).isSome
  -- oversize: headers only
  let bigBody := String.ofList (List.replicate (9 * 1024 * 1024) 'a')
  let big := "From: big@z.example\r\nSubject: A very large one\r\nMessage-ID: <big@z.example>\r\n\r\n" ++ bigBody
  let rb ← Ingest.ingest st { raw := big.toUTF8, key := none, rcpt := none, source := "route", receivedAt := 1790000000002 }
  let br ← db.queryOne "SELECT text_truncated, snippet, size FROM messages WHERE id = ?" #[rb.id]
  check t "a message over 8 MiB is stored from its headers, truncated, snippet from subject" <|
    match br with
    | some r => r.bool "text_truncated" && r.text "snippet" == "A very large one" && r.nat "size" > 9 * 1024 * 1024
    | none => false

/-! ## M2: accounts, sign-in, sessions, tokens, the operator (no dev bypass) -/

def afterStr (s marker : String) : String :=
  match s.splitOn marker with
  | _ :: rest :: _ => String.ofList (rest.toList.takeWhile fun c => c != '\r' && c != '\n' && c != ' ')
  | _ => ""

def jsonOf (r : Router.Resp) : Lean.Json := (Lean.Json.parse r.bodyText).toOption.getD .null
def jget (r : Router.Resp) (k : String) : String := ((jsonOf r).getObjValAs? String k).toOption.getD ""

/-- The `lm_session` value a response sets, if any. -/
def setCookie? (r : Router.Resp) : Option String :=
  (r.headers.find? (·.1 == "Set-Cookie")).bind fun (_, v) =>
    let first := (v.splitOn ";").headD ""
    if first.startsWith "lm_session=" then some (String.ofList (first.toList.drop 11)) else none

/-- The raw mail of a recorded `/internal/email/send`. -/
def sentRaw (j : Lean.Json) : String :=
  String.fromUTF8? (Core.Base64.decode ((j.getObjValAs? String "raw_b64").toOption.getD "")) |>.getD ""

def authChecks (t : IO.Ref Tally) : IO Unit := do
  let now ← Clock.nowMs
  -- tokens
  let cfg : Config := { accountId := "acct1", sessionSecret := "k1", publicOrigin := "https://mail.test",
                        internalSecret := "s3cret", mailFrom := "no-reply@mail.test" }
  let ses := Auth.Tokens.mintSession cfg "SID1" (now + 1000) 3
  check t "a session token is v1.<acct>.<sid>.<exp>.<epoch>.<mac> and verifies" <|
    (ses.splitOn ".").length == 6 && ses.startsWith "v1.acct1.SID1." &&
      Auth.Tokens.verifySession cfg now ses == some { sid := "SID1", exp := now + 1000, epoch := 3 }
  check t "the MAC is base64url HMAC-SHA256 of everything before the last dot" <|
    let signed := ".".intercalate (ses.splitOn ".").dropLast
    ses == signed ++ "." ++ Crypto.base64Url (Crypto.hmacText "k1" signed)
  let tampered := String.ofList ((ses.toList.dropLast) ++ [if ses.back == 'A' then 'B' else 'A'])
  check t "a tampered MAC is rejected" (Auth.Tokens.verifySession cfg now tampered == none)
  let forged := ses.replace "SID1" "SID2"
  check t "a changed payload is rejected" (Auth.Tokens.verifySession cfg now forged == none)
  check t "another account's token is rejected" <|
    Auth.Tokens.verifySession { cfg with accountId := "acct2" } now ses == none &&
      Auth.Tokens.verifyApi { cfg with accountId := "acct2" } (Auth.Tokens.mintApi cfg "r") == none
  check t "an expired token is rejected" <|
    Auth.Tokens.verifySession cfg (now + 1000) ses == none &&
      Auth.Tokens.verifyMagic cfg (now + 5) (Auth.Tokens.mintMagic cfg "R" (now + 5)) == none
  check t "a token under another secret is rejected" <|
    Auth.Tokens.verifySession { cfg with sessionSecret := "k2" } now ses == none
  let api := Auth.Tokens.mintApi cfg "RAND"
  check t "an API token is lm1.<acct>.<rand>.<mac> and verifies" <|
    api.startsWith "lm1.acct1.RAND." && (api.splitOn ".").length == 4 && Auth.Tokens.verifyApi cfg api == some "RAND"
  let mg := Auth.Tokens.mintMagic cfg "R" (now + 1000)
  check t "a magic link verifies as a link, not as a session or an API token" <|
    Auth.Tokens.verifyMagic cfg now mg == some ("R", now + 1000) && Auth.Tokens.verifySession cfg now mg == none &&
      Auth.Tokens.verifyApi cfg mg == none && Auth.Tokens.verifyMagic cfg now ses == none

  -- an account container behind a Worker
  let db ← Db.openMemory
  discard <| Schema.migrate db
  let (calls, worker) ← recorder
  let st : State := { config := cfg, db, store := ← Store.memory, worker, startedAt := 0, schemaVersion := 3, fts5 := true }
  let routes := Routes.all cfg
  let base : List (String × String) := [("x-account-id", "acct1")]
  let send (m path : String) (hs : List (String × String) := []) (body : String := "") (q : List (String × String) := []) :=
    Router.dispatch st routes { method := m, path := (path.splitOn "/").filter (· ≠ ""), query := q,
                                headers := base ++ hs, body := body.toUTF8 }
  let internalH := [("x-internal-secret", "s3cret")]
  let noAcct ← Router.dispatch st routes { method := "GET", path := ["api", "msgs"] }
  check t "behind a Worker a request without x-account-id is refused" (noAcct.status == 401)
  let devTry ← Router.dispatch st routes { method := "GET", path := ["api", "account"], headers := base }
  check t "without the dev bypass x-account-id alone is not a session" (devTry.status == 401)
  let p0 ← send "POST" "/internal/provision" [] "{\"email\":\"Alex@Example.test\",\"is_admin\":true}"
  check t "an internal route without X-Internal-Secret is refused" (p0.status == 401)
  let p0b ← send "POST" "/internal/provision" [("x-internal-secret", "s3creT")] "{\"email\":\"alex@example.test\"}"
  check t "a wrong internal secret is refused" (p0b.status == 401)
  let p1 ← send "POST" "/internal/provision" internalH "{\"email\":\"Alex@Example.test\",\"is_admin\":true,\"display_name\":\"Alex\"}"
  let p2 ← send "POST" "/internal/provision" internalH "{\"email\":\"alex@example.test\",\"is_admin\":true}"
  let c1 ← calls.get
  check t "provisioning creates the account once and mails one welcome link" <|
    p1.status == 200 && ((jsonOf p1).getObjValAs? Bool "created").toOption == some true &&
      ((jsonOf p2).getObjValAs? Bool "created").toOption == some false && c1.size == 1 &&
      c1[0]!.1 == "POST" && c1[0]!.2.1 == "/internal/email/send" &&
      (sentRaw c1[0]!.2.2).contains "Subject: Welcome to LeanMail" &&
      ((← db.queryOne "SELECT purpose FROM magic_links").map (·.text "purpose")) == some "welcome"
  calls.set #[]
  let m0 ← send "POST" "/internal/auth/magic" internalH "{\"email\":\"eve@example.test\",\"purpose\":\"signin\"}"
  check t "a magic link for an unknown email is 202 and sends nothing" (m0.status == 202 && (← calls.get).isEmpty)
  let m1 ← send "POST" "/internal/auth/magic" internalH "{\"email\":\" ALEX@example.test \",\"purpose\":\"signin\"}"
  let c2 ← calls.get
  let mail := sentRaw (c2[0]?.map (·.2.2) |>.getD .null)
  let link := afterStr mail "https://mail.test/auth/magic?t="
  let code := afterStr mail "enter this code in the app: "
  let req0 := (c2[0]?.map (·.2.2)).getD .null
  check t "a magic link for the account's email sends one mail with the link and the code" <|
    m1.status == 202 && c2.size == 1 && c2[0]!.2.1 == "/internal/email/send" &&
      (req0.getObjValAs? String "from").toOption == some "no-reply@mail.test" &&
      (req0.getObjVal? "to").toOption == some (Lean.Json.arr #[Lean.Json.str "alex@example.test"]) &&
      (Auth.Tokens.verifyMagic cfg now link).isSome && code.length == 6 && code.toList.all Char.isDigit
  check t "the mail has RFC 5322 headers and a ULID Message-ID" <|
    mail.startsWith "From: no-reply@mail.test\r\nTo: alex@example.test\r\nSubject: Sign in to LeanMail\r\nDate: " &&
      mail.contains "MIME-Version: 1.0\r\n" && mail.contains "Content-Type: text/plain; charset=utf-8\r\n" &&
      Core.Ulid.isValid (String.ofList ((afterStr mail "Message-ID: <").toList.takeWhile (· != '@')))
  let bad ← send "GET" "/auth/magic" [] "" [("t", link ++ "x")]
  check t "a link with a bad MAC does not sign in" (bad.status == 400 && setCookie? bad == none)
  let g1 ← send "GET" "/auth/magic" [("user-agent", "test")] "" [("t", link)]
  let cookie := (setCookie? g1).getD ""
  check t "GET /auth/magic redirects to / with an HttpOnly Secure Lax session cookie" <|
    g1.status == 302 && g1.headers.lookup "Location" == some "/" && cookie.startsWith "v1.acct1." &&
      ((g1.headers.lookup "Set-Cookie").map (·.endsWith "; HttpOnly; Secure; SameSite=Lax; Path=/; Max-Age=2592000")) == some true
  let g2 ← send "GET" "/auth/magic" [] "" [("t", link)]
  check t "a magic link works once" (g2.status == 400 && setCookie? g2 == none)
  let ck := [("cookie", s!"theme=dark; lm_session={cookie}")]
  let acc ← send "GET" "/api/account" ck
  check t "the session cookie authorizes GET /api/account" <|
    acc.status == 200 && jget acc "email" == "alex@example.test" && jget acc "display_name" == "Alex" &&
      ((jsonOf acc).getObjValAs? Bool "is_admin").toOption == some true
  let msgsC ← send "GET" "/api/msgs" ck
  check t "a session also satisfies a token route" (msgsC.status == 200)
  let csrf1 ← send "PATCH" "/api/account" ck "{\"display_name\":\"A\"}"
  let csrf2 ← send "PATCH" "/api/account" (ck ++ [("x-requested-with", "LeanMail"), ("origin", "https://evil.test")]) "{\"display_name\":\"A\"}"
  check t "a cookie mutation without X-Requested-With, or from another Origin, is 403" (csrf1.status == 403 && csrf2.status == 403)
  let csrfH := [("x-requested-with", "LeanMail"), ("origin", "https://mail.test")]
  let pa ← send "PATCH" "/api/account" (ck ++ csrfH) "{\"display_name\":\"Alex R\",\"retention_days\":30}"
  check t "PATCH /api/account with Origin and X-Requested-With updates the profile" <|
    pa.status == 200 && jget pa "display_name" == "Alex R" && ((jsonOf pa).getObjValAs? Nat "retention_days").toOption == some 30
  let paBad ← send "PATCH" "/api/account" (ck ++ csrfH) "{\"retention_days\":0}"
  check t "a bad retention_days is 400" (paBad.status == 400)
  -- sliding: pretend the session was last seen two hours ago
  db.exec "UPDATE sessions SET last_seen_at = last_seen_at - 7200000"
  let slid ← send "GET" "/api/account" ck
  check t "a session seen over an hour ago slides and gets a fresh cookie" <|
    slid.status == 200 && ((setCookie? slid).map (fun v => (Auth.Tokens.verifySession cfg now v).isSome)) == some true
  let tokR ← send "POST" "/api/tokens" (ck ++ csrfH) "{\"name\":\"cli\",\"scopes\":[\"read\"]}"
  let tok := jget tokR "token"
  check t "POST /api/tokens returns an lm1 token once" (tokR.status == 201 && tok.startsWith "lm1.acct1.")
  let bearer := [("authorization", s!"Bearer {tok}")]
  let bm ← send "GET" "/api/msgs" bearer
  let bImp ← send "POST" "/api/import" bearer "From: a@b\r\n\r\nx"
  let bTok ← send "GET" "/api/tokens" bearer
  -- M6b: a read token may list the tokens (the CLI's `tokens`); minting still needs a session
  let bMint ← send "POST" "/api/tokens" bearer "{\"name\":\"x\",\"scopes\":[\"read\"]}"
  check t "a read token reads, may not act, and is no session" (bm.status == 200 && bImp.status == 403 && bTok.status == 200 && bMint.status == 401)
  let bTamper ← send "GET" "/api/msgs" [("authorization", s!"Bearer {tok}x")]
  check t "a tampered bearer token is 401" (bTamper.status == 401)
  let lst ← send "GET" "/api/tokens" ck
  check t "GET /api/tokens lists the token without the secret" (lst.status == 200 && !(lst.bodyText.contains tok) && lst.bodyText.contains "\"cli\"")
  let del ← send "DELETE" ("/api/tokens/" ++ jget tokR "id") (ck ++ csrfH)
  let bm2 ← send "GET" "/api/msgs" bearer
  check t "a revoked API token stops working" (del.status == 200 && bm2.status == 401)
  -- the 6-digit code
  calls.set #[]
  discard <| send "POST" "/internal/auth/magic" internalH "{\"email\":\"alex@example.test\",\"purpose\":\"signin\"}"
  let mail2 := sentRaw (((← calls.get)[0]?.map (·.2.2)).getD .null)
  let code2 := afterStr mail2 "enter this code in the app: "
  let wrong := if code2 == "000000" then "000001" else "000000"
  let cw ← send "POST" "/auth/code" [] s!"\{\"email\":\"alex@example.test\",\"code\":\"{wrong}\",\"name\":\"phone\"}"
  let cr ← send "POST" "/auth/code" [] s!"\{\"email\":\"Alex@example.test\",\"code\":\"{code2}\",\"name\":\"phone\"}"
  let appTok := jget cr "token"
  let am ← send "GET" "/api/msgs" [("authorization", s!"Bearer {appTok}")]
  check t "POST /auth/code with the right code yields an lm1 token that reads mail" <|
    cw.status == 400 && cr.status == 200 && appTok.startsWith "lm1.acct1." && am.status == 200
  let cr2 ← send "POST" "/auth/code" [] s!"\{\"email\":\"alex@example.test\",\"code\":\"{code2}\",\"name\":\"phone\"}"
  check t "a code works once" (cr2.status == 400)
  calls.set #[]
  discard <| send "POST" "/internal/auth/magic" internalH "{\"email\":\"alex@example.test\",\"purpose\":\"signin\"}"
  let mail3 := sentRaw (((← calls.get)[0]?.map (·.2.2)).getD .null)
  let code3 := afterStr mail3 "enter this code in the app: "
  let link3 := afterStr mail3 "https://mail.test/auth/magic?t="
  let wrong3 := if code3 == "111111" then "111112" else "111111"
  for _ in [0:5] do
    discard <| send "POST" "/auth/code" [] s!"\{\"email\":\"alex@example.test\",\"code\":\"{wrong3}\"}"
  let c5 ← send "POST" "/auth/code" [] s!"\{\"email\":\"alex@example.test\",\"code\":\"{code3}\"}"
  let l5 ← send "GET" "/auth/magic" [] "" [("t", link3)]
  check t "five wrong codes burn the link: the right code and the link then fail" (c5.status == 400 && l5.status == 400)
  -- sessions, logout, logout everywhere
  let sl ← send "GET" "/api/sessions" ck
  check t "GET /api/sessions lists the current session" (sl.status == 200 && sl.bodyText.contains "\"current\":true")
  let lo0 ← send "POST" "/api/logout" ck
  check t "a cookie logout without the CSRF headers is 403" (lo0.status == 403)
  let lo ← send "POST" "/api/logout" (ck ++ csrfH)
  let after ← send "GET" "/api/account" ck
  check t "logout clears the cookie and revokes the session" <|
    lo.status == 200 && ((lo.headers.lookup "Set-Cookie").map (·.contains "Max-Age=0")) == some true && after.status == 401
  let newCookie (st' : State) : IO String := do return ((← Auth.Session.create st' "ua").getD "")
  let ca ← newCookie st
  let cb ← newCookie st
  let okA ← send "GET" "/api/account" [("cookie", s!"lm_session={ca}")]
  let rv ← send "DELETE" ("/api/sessions/" ++ ((Auth.Tokens.verifySession cfg now cb).map (·.sid)).getD "") ([("cookie", s!"lm_session={ca}")] ++ csrfH)
  let okB ← send "GET" "/api/account" [("cookie", s!"lm_session={cb}")]
  check t "DELETE /api/sessions/:id revokes another session" (okA.status == 200 && rv.status == 200 && okB.status == 401)
  let le ← send "POST" "/api/logout-everywhere" ([("cookie", s!"lm_session={ca}")] ++ csrfH)
  let cc ← newCookie st
  let afterA ← send "GET" "/api/account" [("cookie", s!"lm_session={ca}")]
  let okC ← send "GET" "/api/account" [("cookie", s!"lm_session={cc}")]
  check t "logout-everywhere bumps the epoch: old cookies fail, new ones work" <|
    le.status == 200 && afterA.status == 401 && okC.status == 200
  let wrongAcct ← Router.dispatch st routes { method := "GET", path := ["api", "account"], headers := [("x-account-id", "acct2"), ("cookie", s!"lm_session={cc}")] }
  check t "a request stamped for another account is 403" (wrongAcct.status == 403)
  let notOp ← send "POST" "/auth/signup" [] "{\"email\":\"x@y.test\"}"
  let notOpAdmin ← send "GET" "/admin/signups" internalH
  check t "operator routes do not exist in an account container" (notOp.status == 404 && notOpAdmin.status == 404)
  let http : Config := { cfg with publicOrigin := "http://localhost:8787" }
  check t "cookies drop Secure on an http origin" <|
    !((Auth.Session.cookieHeader http "v").contains "Secure") && (Auth.Session.cookieHeader cfg "v").contains "; Secure;"
  check t "the dev bypass is refused off localhost" <|
    !({ cfg with devBypass := true } : Config).devBypassOn && ({ http with devBypass := true } : Config).devBypassOn
  check t "with no internal secret internal routes are open only on localhost" <|
    Router.internalOk { st with config := { cfg with internalSecret := "" } } { method := "POST", path := [] } == false &&
      Router.internalOk { st with config := { http with internalSecret := "" } } { method := "POST", path := [] }

  -- the operator
  let odb ← Db.openMemory
  discard <| Schema.migrate odb
  let (ocalls, oworker) ← recorder
  let ocfg : Config := { cfg with accountId := "_operator", role := "operator", adminEmails := ["boss@op.test"],
                                  signupMode := "approve" }
  let ost : State := { st with config := ocfg, db := odb, worker := oworker }
  let osend (c : Config) (m path : String) (hs : List (String × String) := []) (body : String := "") :=
    Router.dispatch { ost with config := c } (Routes.all c)
      { method := m, path := (path.splitOn "/").filter (· ≠ ""), headers := [("x-account-id", "_operator")] ++ hs, body := body.toUTF8 }
  let ip (n : String) := [("cf-connecting-ip", n)]
  let s1 ← osend ocfg "POST" "/auth/signup" (ip "1.1.1.1") "{\"email\":\" New@Person.test \",\"note\":\"hi\"}"
  let pend ← odb.queryOne "SELECT id, status FROM accounts WHERE email = 'new@person.test'"
  check t "a sign-up is 202 and leaves a pending account and a signup row" <|
    s1.status == 202 && (pend.map (·.text "status")) == some "pending" && (← ocalls.get).isEmpty &&
      ((← odb.queryOne "SELECT count(*) AS n FROM signups").map (·.nat "n")) == some 1
  let newId := (pend.map (·.text "id")).getD ""
  let boss ← osend ocfg "POST" "/auth/signup" (ip "2.2.2.2") "{\"email\":\"boss@op.test\"}"
  let bossRow ← odb.queryOne "SELECT id, status, is_admin FROM accounts WHERE email = 'boss@op.test'"
  let bossId := (bossRow.map (·.text "id")).getD ""
  check t "a sign-up from ADMIN_EMAILS is approved at once as an admin" <|
    boss.status == 202 && (bossRow.map (·.text "status")) == some "active" && (bossRow.map (·.bool "is_admin")) == some true
  ocalls.set #[]
  let adm := internalH ++ [("x-admin-account", bossId)]
  let noAdm ← osend ocfg "GET" "/admin/signups" (internalH ++ [("x-admin-account", newId)])
  let noSecret ← osend ocfg "GET" "/admin/signups" [("x-admin-account", bossId)]
  check t "/admin/* refuses a non-admin account and a missing internal secret" (noAdm.status == 403 && noSecret.status == 403)
  let ls ← osend ocfg "GET" "/admin/signups" adm
  let items := ((jsonOf ls).getObjValAs? (Array Lean.Json) "items").toOption.getD #[]
  let sid := (items[0]?.bind fun j => (j.getObjValAs? String "id").toOption).getD ""
  check t "GET /admin/signups lists the pending sign-up" (ls.status == 200 && ls.bodyText.contains "new@person.test" && !(ls.bodyText.contains "boss@op.test"))
  let ap ← osend ocfg "POST" s!"/admin/signups/{sid}/approve" adm
  let oc ← ocalls.get
  let acctRow ← odb.queryOne "SELECT status, approved_by FROM accounts WHERE id = ?" #[newId]
  let projBody := (oc[0]?.map (·.2.2)).getD .null
  let wakeBody := (oc[1]?.map (·.2.2)).getD .null
  check t "approve activates, projects the account to /internal/accounts and wakes it with a provision" <|
    ap.status == 200 && (acctRow.map (·.text "status")) == some "active" && (acctRow.map (·.text "approved_by")) == some bossId &&
      oc.size == 2 && oc[0]!.1 == "PUT" && oc[0]!.2.1 == "/internal/accounts" &&
      (projBody.getObjVal? "accounts").toOption == some (Lean.Json.arr #[Lean.Json.mkObj [("id", Lean.Json.str newId),
        ("email", Lean.Json.str "new@person.test"), ("status", Lean.Json.str "active")]]) &&
      oc[1]!.1 == "POST" && oc[1]!.2.1 == s!"/internal/wake/{newId}" &&
      (wakeBody.getObjVal? "provision").toOption == some (Lean.Json.mkObj [("email", Lean.Json.str "new@person.test"),
        ("is_admin", Lean.Json.bool false)])
  let au ← odb.query "SELECT actor, action, target FROM audit WHERE action = 'approve'"
  check t "approval is audited with the admin as actor" (au.size == 1 && au[0]!.text "actor" == bossId && au[0]!.text "target" == newId)
  let again ← osend ocfg "POST" "/auth/signup" (ip "1.1.1.1") "{\"email\":\"new@person.test\"}"
  check t "signing up again once active is a silent 202" <|
    again.status == 202 && ((← odb.queryOne "SELECT count(*) AS n FROM signups WHERE email = 'new@person.test'").map (·.nat "n")) == some 1
  discard <| osend ocfg "POST" "/auth/signup" (ip "3.3.3.3") "{\"email\":\"nope@person.test\"}"
  let rsid := ((← odb.queryOne "SELECT id FROM signups WHERE email = 'nope@person.test'").map (·.text "id")).getD ""
  let rj ← osend ocfg "POST" s!"/admin/signups/{rsid}/reject" adm
  check t "reject records the decision and drops the pending account" <|
    rj.status == 200 && ((← odb.queryOne "SELECT decision FROM signups WHERE id = ?" #[rsid]).map (·.text "decision")) == some "rejected" &&
      (← odb.queryOne "SELECT id FROM accounts WHERE email = 'nope@person.test'").isNone
  ocalls.set #[]
  let dm ← osend ocfg "PUT" s!"/admin/accounts/{newId}/domains" adm "{\"domains\":[\"Person.test\",\"mail.person.test\"]}"
  let dmBody := (((← ocalls.get)[0]?).map (·.2.2)).getD .null
  let taken ← osend ocfg "PUT" s!"/admin/accounts/{bossId}/domains" adm "{\"domains\":[\"person.test\"]}"
  check t "domain grants are stored, projected, and not given to two accounts" <|
    dm.status == 200 && (dmBody.getObjVal? "domains").toOption == some (Lean.Json.arr #[
      Lean.Json.mkObj [("name", Lean.Json.str "mail.person.test"), ("account_id", Lean.Json.str newId)],
      Lean.Json.mkObj [("name", Lean.Json.str "person.test"), ("account_id", Lean.Json.str newId)]]) && taken.status == 409
  let la ← osend ocfg "GET" "/admin/accounts" adm
  ocalls.set #[]
  let ds ← osend ocfg "POST" s!"/admin/accounts/{newId}/disable" adm
  let dsBody := (((← ocalls.get)[0]?).map (·.2.2)).getD .null
  check t "GET /admin/accounts lists domains; disable projects the disabled status" <|
    la.status == 200 && la.bodyText.contains "mail.person.test" && ds.status == 200 &&
      (dsBody.compress.contains "\"status\":\"disabled\"") &&
      ((← odb.query "SELECT action FROM audit").map (·.text "action")).toList.contains "disable"
  let closed ← osend { ocfg with signupMode := "closed" } "POST" "/auth/signup" (ip "4.4.4.4") "{\"email\":\"c@person.test\"}"
  check t "SIGNUP_MODE=closed answers 404" (closed.status == 404)
  ocalls.set #[]
  let op ← osend { ocfg with signupMode := "open" } "POST" "/auth/signup" (ip "5.5.5.5") "{\"email\":\"o@person.test\"}"
  check t "SIGNUP_MODE=open approves at once, projects and provisions" <|
    op.status == 202 && ((← odb.queryOne "SELECT status FROM accounts WHERE email = 'o@person.test'").map (·.text "status")) == some "active" &&
      ((← ocalls.get).map (·.2.1)).toList.any (·.startsWith "/internal/wake/")
  let mut statuses : Array Nat := #[]
  for i in [0:11] do
    let r ← osend ocfg "POST" "/auth/signup" (ip "9.9.9.9") s!"\{\"email\":\"r{i}@person.test\"}"
    statuses := statuses.push r.status
  check t "sign-ups are limited to 10 an hour per IP" <|
    (statuses.extract 0 10).all (· == 202) && statuses[10]! == 429
  let devCfg : Config := { ocfg with publicOrigin := "http://localhost:8787", internalSecret := "", devBypass := true }
  let devAdm ← osend devCfg "GET" "/admin/accounts"
  check t "the dev bypass is an admin on a bare operator" (devAdm.status == 200)

/-! ## M2b: the web pages -/

def has (s sub : String) : Bool := (s.splitOn sub).length > 1
def count (s sub : String) : Nat := (s.splitOn sub).length - 1

/-- The page security headers, all of them. -/
def pageHeadersOk (r : Router.Resp) : Bool :=
  r.headers.lookup "Content-Security-Policy" == some Pages.csp &&
    r.headers.lookup "Referrer-Policy" == some "no-referrer" &&
    r.headers.lookup "X-Content-Type-Options" == some "nosniff" &&
    r.headers.lookup "Content-Type" == some "text/html; charset=utf-8"

def pageChecks (t : IO.Ref Tally) : IO Unit := do
  let now ← Clock.nowMs
  -- the builder
  let n := Core.Html.el "a" [("href", "/x?a=1&b=\"2\"")] [Core.Html.txt "<b>Tom & 'Jerry'</b>", Core.Html.el "br"]
  check t "the HTML builder escapes text and attributes and leaves void elements open" <|
    Core.Html.render n == "<a href=\"/x?a=1&amp;b=&quot;2&quot;\">&lt;b&gt;Tom &amp; &#39;Jerry&#39;&lt;/b&gt;<br></a>" &&
      Core.Html.render (Core.Html.raw "<hr>") == "<hr>"

  -- a bare dev server with the bypass, the fixtures, and one hostile subject
  let db ← Db.openMemory
  discard <| Schema.migrate db
  let store ← Store.memory
  let (_, worker) ← recorder
  let st : State := { config := { devBypass := true }, db, store, worker, startedAt := 0, schemaVersion := 3, fts5 := true }
  let dev := [("x-account-id", "dev")]
  let html := [("accept", "text/html,application/xhtml+xml")]
  let req (m path : String) (hs : List (String × String) := []) (q : List (String × String) := []) (body : String := "") : Router.Req :=
    { method := m, path := (path.splitOn "/").filter (· ≠ ""), query := q, headers := hs, body := body.toUTF8 }
  let go (st' : State) (r : Router.Req) : IO Router.Resp := Router.dispatch st' (Routes.all st'.config) r
  discard <| go st (req "POST" "/internal/provision" dev [] "{\"email\":\"alex@example.test\",\"is_admin\":true,\"display_name\":\"Alex\"}")
  let dir : System.FilePath := "test/fixtures/eml"
  let files := ((← dir.readDir).filter (·.fileName.endsWith ".eml")).map (·.path) |>.qsort (·.toString < ·.toString)
  let mut byName : List (String × String) := []
  let mut i := 0
  for f in files do
    let raw ← IO.FS.readBinFile f
    let m := Core.Mime.parse raw
    let recAt := ((m.rawHeader? "date").bind Core.Mime.Date.parse?).getD 0
    let ulid := Core.Ulid.encode recAt (ByteArray.mk #[0, 0, 0, 0, 0, 0, 0, 0, 1, i.toUInt8])
    store.put s!"raw/dev/{ulid}" raw [("rcpt", "alex@fralex.art"), ("received_at", toString recAt), ("source", "route")]
    store.put s!"in/dev/{ulid}" .empty []
    byName := (f.fileName.getD "", ulid) :: byName
    i := i + 1
  let idOf (n : String) := (byName.lookup n).getD ""
  discard <| Spool.drain st
  let evil := "From: Mallory <m@evil.test>\r\nSubject: <script>x</script>\r\nMessage-ID: <s1@evil.test>\r\n\r\nhi\r\n"
  let ev ← Ingest.ingest st { raw := evil.toUTF8, key := none, rcpt := none, source := "route", receivedAt := now }
  let mut pages : Array Router.Resp := #[]

  let inbox ← go st (req "GET" "/inbox" (dev ++ html))
  pages := pages.push inbox
  let ib := inbox.bodyText
  check t "/inbox lists the fixture subjects with links, under <title>LeanMail · Inbox</title>" <|
    inbox.status == 200 && has ib "<title>LeanMail · Inbox</title>" && has ib "Quote for 500 units, matte finish" &&
      has ib "Logo options, round two" && has ib s!"href=\"/m/{idOf "05.eml"}\"" && has ib "href=\"/static/app.css\""
  check t "/inbox escapes a <script> subject" <|
    has ib "&lt;script&gt;x&lt;/script&gt;" && !has ib "<script>x</script>" && has ib s!"/m/{ev.id}"
  check t "/inbox has the filter tabs, the search box and the account menu" <|
    has ib "href=\"/inbox?filter=archived\"" && has ib "name=\"q\"" && has ib "action=\"/search\"" &&
      has ib "class=\"menu\"" && has ib "href=\"/admin/signups\"" && has ib "action=\"/signout\""
  let root ← go st (req "GET" "/" (dev ++ html))
  check t "/ redirects to /inbox" (root.status == 302 && root.headers.lookup "Location" == some "/inbox")

  let m13 ← go st (req "GET" s!"/m/{idOf "13.eml"}" (dev ++ html))
  pages := pages.push m13
  check t "/m/<id> shows the headers and the attachment download link" <|
    m13.status == 200 && has m13.bodyText "<dt>From</dt>" && has m13.bodyText "<dt>To</dt>" && has m13.bodyText "<dt>Date</dt>" &&
      has m13.bodyText "Invoice 2026-0912" && has m13.bodyText s!"href=\"/api/msg/{idOf "13.eml"}/attachments/0\"" &&
      has m13.bodyText "invoice-2026-0912.pdf"
  let m12 ← go st (req "GET" s!"/m/{idOf "12.eml"}" (dev ++ html))
  pages := pages.push m12
  check t "/m/<id> with an HTML part shows it only in a sandboxed iframe" <|
    m12.status == 200 && has m12.bodyText s!"<iframe sandbox=\"\" src=\"/api/msg/{idOf "12.eml"}/html\"" &&
      has m12.bodyText s!"href=\"/m/{idOf "12.eml"}?view=text\""
  let m12t ← go st (req "GET" s!"/m/{idOf "12.eml"}" (dev ++ html) [("view", "text")])
  check t "?view=text shows the stored text instead" (has m12t.bodyText "<pre class=\"body-text\">" && !has m12t.bodyText "<iframe")
  let missing ← go st (req "GET" "/m/NOPE" (dev ++ html))
  pages := pages.push missing
  check t "an unknown message is the HTML 404 page" (missing.status == 404 && has missing.bodyText "<title>LeanMail · Not found</title>")

  -- actions
  let id05 := idOf "05.eml"
  let form := [("content-type", "application/x-www-form-urlencoded")]
  let done ← go st (req "POST" s!"/m/{id05}/action" (dev ++ html ++ form) [] "action=done")
  let r05 ← db.queryOne "SELECT archived_at, updated_at FROM messages WHERE id = ?" #[id05]
  let inbox2 ← go st (req "GET" "/inbox" (dev ++ html))
  let doneTab ← go st (req "GET" "/inbox" (dev ++ html) [("filter", "archived")])
  pages := pages.push doneTab
  check t "the Done form sets archived_at, redirects, and moves the message from Inbox to Done" <|
    done.status == 303 && done.headers.lookup "Location" == some "/inbox" &&
      ((r05.bind (·.nat? "archived_at")).map (fun x => decide (x ≥ now))) == some true && ((r05.map (·.nat "updated_at")).getD 0) ≥ now &&
      !has inbox2.bodyText s!"/m/{id05}\"" && has doneTab.bodyText s!"/m/{id05}\""
  let id01 := idOf "01.eml"
  let sn ← go st (req "POST" s!"/m/{id01}/action" (dev ++ html ++ form) [] "action=snooze_1d")
  let r01 ← db.queryOne "SELECT snoozed_until FROM messages WHERE id = ?" #[id01]
  let snTab ← go st (req "GET" "/inbox" (dev ++ html) [("filter", "snoozed")])
  check t "snooze sets snoozed_until in the future and lists the message under Snoozed" <|
    sn.status == 303 && ((r01.bind (·.nat? "snoozed_until")).map (fun x => decide (x > now + 23 * 3600 * 1000))) == some true &&
      has snTab.bodyText s!"/m/{id01}\""
  check t "next week is a Monday 08:00 UTC after now" <|
    let w := Actions.nextMonday now
    w > now && w ≤ now + 7 * 86400000 && (w / 86400000 + 3) % 7 == 0 && w % 86400000 == 8 * 3600000
  let jsonH := [("content-type", "application/json")]
  let pt ← go st (req "PATCH" s!"/api/msg/{idOf "12.eml"}" (dev ++ jsonH) [] "{\"action\":\"toread\"}")
  let ptPast ← go st (req "PATCH" s!"/api/msg/{idOf "12.eml"}" (dev ++ jsonH) [] "{\"action\":\"snooze\",\"until\":5}")
  let ptBad ← go st (req "PATCH" s!"/api/msg/{idOf "12.eml"}" (dev ++ jsonH) [] "{\"action\":\"explode\"}")
  let ptNone ← go st (req "PATCH" "/api/msg/NOPE" (dev ++ jsonH) [] "{\"action\":\"done\"}")
  check t "PATCH /api/msg/:id applies an action and answers the list item; bad ones are 400/404" <|
    pt.status == 200 && has pt.bodyText "\"to_read_at\":" && !has pt.bodyText "\"to_read_at\":null" &&
      ptPast.status == 400 && ptBad.status == 400 && ptNone.status == 404
  let fv ← go st (req "PATCH" s!"/api/msg/{idOf "12.eml"}" (dev ++ jsonH) [] "{\"action\":\"forever\"}")
  check t "keep forever clears expires_at" <|
    fv.status == 200 && ((← db.queryOne "SELECT expires_at FROM messages WHERE id = ?" #[idOf "12.eml"]).map (·.get "expires_at" |>.isNull)) == some true

  -- search, settings, admin, sign-in, static
  let sr ← go st (req "GET" "/search" (dev ++ html) [("q", "matte")])
  pages := pages.push sr
  check t "/search?q=matte lists exactly one message" <|
    sr.status == 200 && count sr.bodyText "class=\"row\"" == 1 && has sr.bodyText s!"/m/{id05}\"" && has sr.bodyText "value=\"matte\""
  let se ← go st (req "GET" "/settings" (dev ++ html))
  pages := pages.push se
  check t "/settings shows the profile, sessions and tokens sections and the sign-out buttons" <|
    se.status == 200 && has se.bodyText "id=\"profile\"" && has se.bodyText "id=\"sessions\"" && has se.bodyText "id=\"tokens\"" &&
      has se.bodyText "action=\"/settings/tokens\"" && has se.bodyText "action=\"/signout-everywhere\"" && has se.bodyText "value=\"Alex\""
  let prof ← go st (req "POST" "/settings/profile" (dev ++ html ++ form) [] "display_name=Alex+R%C3%B6ss&default_from=&retention_days=30")
  let profBad ← go st (req "POST" "/settings/profile" (dev ++ html ++ form) [] "display_name=A&retention_days=0")
  let acctRow ← db.queryOne "SELECT display_name, retention_days FROM account WHERE id = 1"
  check t "the profile form saves through the account PATCH logic and re-renders an error" <|
    prof.status == 303 && (acctRow.map (·.text "display_name")) == some "Alex Röss" &&
      (acctRow.map (·.nat "retention_days")) == some 30 && profBad.status == 400 && has profBad.bodyText "bad retention_days"
  let tk ← go st (req "POST" "/settings/tokens" (dev ++ html ++ form) [] "name=laptop&scope=read&scope=act")
  check t "creating a token shows it once on the settings page" <|
    tk.status == 200 && has tk.bodyText "value=\"lm1.dev." && has tk.bodyText "read act" &&
      (tk.headers.lookup "Cache-Control") == some "no-store"
  let adm ← go st (req "GET" "/admin/signups" (dev ++ html))
  let admJs ← go st (req "GET" "/admin/signups" (dev ++ [("accept", "application/json"), ("x-requested-with", "LeanMail")]))
  pages := pages.push adm
  check t "/admin/signups is a shell for a navigation and a JSON 404 for the script in an account container" <|
    adm.status == 200 && has adm.bodyText "data-src=\"/admin/signups\"" && has adm.bodyText "<noscript>" && admJs.status == 404
  let si ← go st (req "GET" "/signin" html)
  pages := pages.push si
  check t "/signin is public and posts to /signin on a bare run" <|
    si.status == 200 && has si.bodyText "action=\"/signin\"" && has si.bodyText "type=\"email\""
  check t "behind a Worker the sign-in form posts to /auth/login" <|
    has (Core.Html.render (Pages.signinDoc { st.config with internalSecret := "x" })) "action=\"/auth/login\""
  let css ← go st (req "GET" "/static/app.css" [])
  let js ← go st (req "GET" "/static/app.js" [])
  let nope ← go st (req "GET" "/static/secret.txt" [])
  check t "/static/app.css and app.js are served with their types and an hour of caching; others are 404" <|
    css.status == 200 && css.headers.lookup "Content-Type" == some "text/css; charset=utf-8" &&
      css.headers.lookup "Cache-Control" == some "public, max-age=3600" && css.body.size > 100 &&
      js.status == 200 && js.headers.lookup "Content-Type" == some "text/javascript; charset=utf-8" && nope.status == 404
  check t s!"every HTML page carries the CSP, Referrer-Policy and nosniff ({pages.size} pages)" (pages.all pageHeadersOk)

  -- no bypass: a browser is sent to /signin; form posts need a same-origin Origin
  let (calls2, worker2) ← recorder
  let db2 ← Db.openMemory
  discard <| Schema.migrate db2
  let st2 : State := { st with config := {}, db := db2, worker := worker2 }
  discard <| go st2 (req "POST" "/internal/provision" [] [] "{\"email\":\"alex@example.test\"}")
  let red ← go st2 (req "GET" "/inbox" html)
  let api401 ← go st2 (req "GET" "/inbox" [("accept", "application/json")])
  check t "without a session /inbox redirects a browser to /signin (302), and other clients get 401" <|
    red.status == 302 && red.headers.lookup "Location" == some "/signin" && api401.status == 401
  let mid ← Ingest.ingest st2 { raw := evil.toUTF8, key := none, rcpt := none, source := "route", receivedAt := now }
  let cookie := ((← Auth.Session.create st2 "test").getD "")
  let ck := [("cookie", s!"lm_session={cookie}")]
  let noOrigin ← go st2 (req "POST" s!"/m/{mid.id}/action" (ck ++ html ++ form) [] "action=junk")
  let evilOrigin ← go st2 (req "POST" s!"/m/{mid.id}/action" (ck ++ html ++ form ++ [("origin", "https://evil.test")]) [] "action=junk")
  let sameOrigin ← go st2 (req "POST" s!"/m/{mid.id}/action" (ck ++ html ++ form ++ [("origin", "http://127.0.0.1:8787")]) [] "action=junk")
  let fetchSite ← go st2 (req "POST" s!"/m/{mid.id}/action" (ck ++ html ++ form ++ [("sec-fetch-site", "same-origin")]) [] "action=notjunk")
  check t "a cookie form post needs Origin = PUBLIC_ORIGIN or Sec-Fetch-Site: same-origin" <|
    noOrigin.status == 403 && evilOrigin.status == 403 && sameOrigin.status == 303 && fetchSite.status == 303 &&
      pageHeadersOk noOrigin
  let mine ← go st2 (req "GET" "/inbox" (ck ++ html))
  check t "the session cookie opens /inbox" (mine.status == 200 && has mine.bodyText "alex@example.test")
  -- the web code sign-in: POST /signin mails a code, POST /signin/code starts a session
  calls2.set #[]
  let sp ← go st2 (req "POST" "/signin" (html ++ form ++ [("origin", "http://127.0.0.1:8787")]) [] "email=Alex%40example.test")
  let spOther ← go st2 (req "POST" "/signin" (html ++ form ++ [("origin", "http://127.0.0.1:8787")]) [] "email=eve%40example.test")
  let sent ← calls2.get
  let code := afterStr (sentRaw ((sent[0]?.map (·.2.2)).getD .null)) "enter this code in the app: "
  check t "POST /signin mails the account's address only and always shows the code form" <|
    sp.status == 200 && spOther.status == 200 && sent.size == 1 && code.length == 6 &&
      has sp.bodyText "action=\"/signin/code\"" && has spOther.bodyText "action=\"/signin/code\""
  let wrong := if code == "000000" then "000001" else "000000"
  let cw ← go st2 (req "POST" "/signin/code" (html ++ form ++ [("origin", "http://127.0.0.1:8787")]) [] s!"email=alex%40example.test&code={wrong}")
  let cr ← go st2 (req "POST" "/signin/code" (html ++ form ++ [("origin", "http://127.0.0.1:8787")]) [] s!"email=alex%40example.test&code={code}")
  let newCk := (setCookie? cr).getD ""
  let after ← go st2 (req "GET" "/settings" (html ++ [("cookie", s!"lm_session={newCk}")]))
  check t "the 6-digit code form signs the browser in with a session cookie" <|
    cw.status == 400 && cr.status == 303 && cr.headers.lookup "Location" == some "/inbox" && newCk.startsWith "v1.dev." &&
      after.status == 200 && has after.bodyText "This session"
  let so ← go st2 (req "POST" "/signout" (html ++ form ++ [("cookie", s!"lm_session={newCk}"), ("origin", "http://127.0.0.1:8787")]))
  let afterOut ← go st2 (req "GET" "/inbox" (html ++ [("cookie", s!"lm_session={newCk}")]))
  check t "sign out clears the cookie and the session" <|
    so.status == 303 && ((so.headers.lookup "Set-Cookie").map (has · "Max-Age=0")) == some true && afterOut.status == 302

/-! ## M3: the RFC 822 builder, sending, drafts, compose -/

def jnat (r : Router.Resp) (k : String) : Nat := ((jsonOf r).getObjValAs? Nat k).toOption.getD 0
def dropNl (s : Option String) : Option String := s.map fun x => if x.endsWith "\n" then String.ofList x.toList.dropLast else x

/-- The builder's output parses back to the same fields. -/
def roundTrip (m : Core.Rfc822.Message) : Bool :=
  let p := Core.Mime.parse (Core.Rfc822.build m "seedXYZ")
  let mbox (a : Core.Rfc822.Addr) := Core.Mime.Addr.mailbox a.name a.address
  dropNl p.text == some (Core.Rfc822.normalizeNewlines m.text) &&
    dropNl p.html == m.html &&
    p.subject? == (if m.subject.isEmpty then none else some m.subject) &&
    p.from? == some (mbox m.from_) && p.to == m.to.map mbox && p.cc == m.cc.map mbox &&
    p.messageId? == some (Core.Rfc822.bracketed m.messageId) && p.dateMs? == some (m.date / 1000 * 1000) &&
    p.inReplyTo? == m.inReplyTo && p.references? == m.references &&
    (p.attachments.toList.map fun a => (a.filename.getD "", a.content.size, a.content.toList)) ==
      m.attachments.map fun a => (a.filename, a.data.size, a.data.toList)

def sendChecks (t : IO.Ref Tally) : IO Unit := do
  let now ← Clock.nowMs
  -- the builder
  let base : Core.Rfc822.Message := {
    from_ := { name := "Alex", address := "alex@fralex.art" }, to := [{ address := "bob@x.test" }]
    subject := "Hello", text := "One line.\nTwo lines.", date := 1788944400000, messageId := "01K0TEST@fralex.art" }
  check t "rfc822: a text-only message round-trips through Core.Mime.parse" (roundTrip base)
  let textOnly := String.fromUTF8! (Core.Rfc822.build base "s")
  check t "rfc822: text only is text/plain quoted-printable with CRLF and the RFC 5322 headers" <|
    has textOnly "\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Transfer-Encoding: quoted-printable\r\n" &&
      has textOnly "MIME-Version: 1.0\r\n" && has textOnly "Message-ID: <01K0TEST@fralex.art>\r\n" &&
      has textOnly "Date: Wed, 09 Sep 2026 09:00:00 +0000\r\n" && !has textOnly "multipart" &&
      (textOnly.replace "\r\n" "").all (· != '\n')
  let alt := { base with html := some "<p>Hello <b>there</b></p>" }
  check t "rfc822: text and HTML are a multipart/alternative that round-trips" <|
    roundTrip alt && has (String.fromUTF8! (Core.Rfc822.build alt "s")) "multipart/alternative; boundary=\"=_lm_alt_s\""
  let intl : Core.Rfc822.Message := { alt with
    from_ := { name := "Zoë Ångström-Lindqvist", address := "zoe@fralex.art" }
    to := [{ name := "Bob, Jr.", address := "bob@x.test" }, { name := "Ünal", address := "u@y.test" }]
    cc := [{ address := "c@y.test" }]
    subject := "Grüße aus Köln — ein ziemlich langer Betreff, der gefaltet werden muss, ganz sicher"
    inReplyTo := some "<p1@x.test>", references := some "<p0@x.test> <p1@x.test>"
    attachments := [{ filename := "notes.txt", mime := "text/plain", data := "abc\r\n=def\n".toUTF8 },
                    { filename := "résumé été 2026 — très long nom de fichier.pdf", mime := "application/pdf",
                      data := ByteArray.mk ((List.range 3000).map (fun i => (i * 7 % 256).toUInt8)).toArray }] }
  let intlRaw := String.fromUTF8! (Core.Rfc822.build intl "seed")
  check t "rfc822: two attachments (one non-ASCII name), a non-ASCII subject and name round-trip" (roundTrip intl)
  check t "rfc822: multipart/mixed wraps the alternative; RFC 2047 and RFC 2231 are used; lines fold at 78" <|
    has intlRaw "Content-Type: multipart/mixed; boundary=\"=_lm_mix_seed\"" && has intlRaw "=?UTF-8?B?" &&
      has intlRaw "filename*0*=UTF-8''r%C3%A9sum%C3%A9" && has intlRaw "Content-Transfer-Encoding: base64" &&
      ((intlRaw.splitOn "\r\n").takeWhile (· ≠ "")).all (·.length ≤ 78) &&
      ((intlRaw.splitOn "\r\n").all (·.length ≤ 998))
  let qp := Core.Rfc822.quotedPrintable ("a=b trailing \n" ++ String.ofList (List.replicate 200 'x') ++ "\nnaïve")
  let qpLines := (qp.splitOn "\r\n").filter (· ≠ "")
  check t "quoted-printable: = is escaped, a trailing space is =20, long lines break softly at ≤ 76" <|
    qpLines.head? == some "a=3Db trailing=20" && qpLines.all (·.length ≤ 76) && qpLines.length == 5 &&
      qpLines[1]!.endsWith "=" && qpLines[3]! == String.ofList (List.replicate 50 'x') && qpLines[4]! == "na=C3=AFve" &&
      qp.endsWith "\r\n"

  -- a server with a recording Worker that sees the database at call time
  let db ← Db.openMemory
  discard <| Schema.migrate db
  let store ← Store.memory
  let calls : IO.Ref (Array (String × Lean.Json × Nat)) ← IO.mkRef #[]
  let failWorker ← IO.mkRef false
  let worker : WorkerClient := { call := fun _ path j => do
    let n := ((← db.queryOne "SELECT count(*) AS n FROM messages WHERE folder = 'sent' AND send_status = 'sending'").map (·.nat "n")).getD 0
    calls.modify (·.push (path, j, n))
    if ← failWorker.get then return none
    return some (Lean.Json.mkObj [("ok", Lean.Json.bool true)]) }
  let st : State := { config := { devBypass := true }, db, store, worker, startedAt := 0, schemaVersion := 4, fts5 := true }
  db.exec "INSERT INTO account(id, email, display_name, default_from, created_at, updated_at) VALUES (1, 'alex@fralex.art', 'Alex', 'alex@fralex.art', 0, 0)"
  let parentRaw := "From: Tomas Reyes <tomas@labelforge.io>\r\nTo: Alex <alex@fralex.art>, Bob <bob@x.test>\r\nCc: carol@y.test\r\nSubject: Quote\r\nMessage-ID: <parent1@labelforge.io>\r\nReferences: <root@labelforge.io>\r\nDate: Thu, 10 Sep 2026 09:00:00 +0000\r\n\r\nLine one\r\nLine two\r\n"
  let parentKey := s!"raw/dev/{Core.Ulid.encode now (ByteArray.mk #[7,7,7,7,7,7,7,7,7,7])}"
  store.put parentKey parentRaw.toUTF8 []
  let parent ← Ingest.ingest st { raw := parentRaw.toUTF8, key := some parentKey, rcpt := some "alex@fralex.art", source := "route", receivedAt := now - 60000 }
  let pid := parent.id
  let routesM3 := Routes.all st.config
  let go (m path : String) (hs : List (String × String) := []) (b : String := "") (q : List (String × String) := []) : IO Router.Resp :=
    let segs := (path.splitOn "/").filter (· ≠ "")
    let req : Router.Req := { method := m, path := segs, query := q, body := b.toUTF8,
                              headers := hs ++ [("x-account-id", "dev"), ("content-type", "application/json")] }
    Router.dispatch st routesM3 req
  let ulidN (n : Nat) : String := Core.Ulid.encode now (ByteArray.mk #[0, 0, 0, 0, 0, 0, 0, 3, (n / 256).toUInt8, (n % 256).toUInt8])
  let wireOf (j : Lean.Json) : Core.Mime.Message := Core.Mime.parse (Core.Base64.decode ((j.getObjValAs? String "raw_b64").toOption.getD ""))

  -- the happy path
  let cid1 := ulidN 1
  let body1 := s!"\{\"client_id\":\"{cid1}\",\"from\":\"alex@fralex.art\",\"to\":[\"Tomas <tomas@labelforge.io>\"],\"bcc\":[\"hidden@z.test\"],\"subject\":\"Grüße\",\"text\":\"Hello =\\nthere \"}"
  let s1 ← go "POST" "/api/send" [] body1
  let c1 ← calls.get
  let w1 := wireOf ((c1[0]?.map (·.2.1)).getD .null)
  let id1 := jget s1 "id"
  let row1 ← db.queryOne "SELECT * FROM messages WHERE id = ?" #[id1]
  let stored1 := (← store.get s!"raw/dev/{id1}").map fun (b, _) => String.fromUTF8? b |>.getD ""
  check t "POST /api/send stores the Sent copy (sending) before the Worker call and sends the wire copy" <|
    s1.status == 200 && jget s1 "send_status" == "sent" && c1.size == 1 && c1[0]!.1 == "/internal/email/send" &&
      c1[0]!.2.2 == 1 &&
      ((c1[0]!.2.1.getObjVal? "to").toOption == some (Lean.Json.arr #[Lean.Json.str "tomas@labelforge.io", Lean.Json.str "hidden@z.test"])) &&
      ((c1[0]!.2.1.getObjValAs? String "from").toOption == some "alex@fralex.art")
  check t "the wire raw parses with the right headers, our Message-ID, and no Bcc" <|
    w1.subject? == some "Grüße" && w1.from? == some (Core.Mime.Addr.mailbox "Alex" "alex@fralex.art") &&
      w1.to == [Core.Mime.Addr.mailbox "Tomas" "tomas@labelforge.io"] && w1.bcc == [] &&
      w1.messageId? == some s!"<{id1.toLower}@fralex.art>" && dropNl w1.text == some "Hello =\nthere "
  check t "the Sent row has source sent, folder sent, send_status sent, the client_id, and its raw (with Bcc)" <|
    (match row1 with
     | some r => r.text "source" == "sent" && r.text "folder" == "sent" && r.text "send_status" == "sent" &&
         r.text "client_id" == cid1 && r.text "raw_key" == s!"raw/dev/{id1}" && r.text "message_id" == s!"{id1.toLower}@fralex.art"
     | none => false) && (stored1.map (has · "Bcc: hidden@z.test")) == some true
  let s1b ← go "POST" "/api/send" [] body1
  check t "sending again with the same client_id answers the same message and sends nothing" <|
    s1b.status == 200 && jget s1b "id" == id1 && ((jsonOf s1b).getObjValAs? Bool "idempotent").toOption == some true &&
      (← calls.get).size == 1
  -- a failing Worker
  failWorker.set true
  let cid2 := ulidN 2
  let body2 := s!"\{\"client_id\":\"{cid2}\",\"from\":\"alex@fralex.art\",\"to\":[\"bob@x.test\"],\"subject\":\"x\",\"text\":\"y\"}"
  let s2 ← go "POST" "/api/send" [] body2
  let row2 ← db.queryOne "SELECT send_status, send_error FROM messages WHERE id = ?" #[jget s2 "id"]
  check t "a failed Worker call leaves send_status failed with send_error" <|
    s2.status == 200 && jget s2 "send_status" == "failed" &&
      (row2.map (·.text "send_status")) == some "failed" && (row2.map (·.text "send_error")) == some "worker_failed"
  failWorker.set false
  let s2b ← go "POST" "/api/send" [] body2
  let old2 ← db.queryOne "SELECT deleted, client_id FROM messages WHERE id = ?" #[jget s2 "id"]
  check t "a retry of a failed client_id sends again and tombstones the failed copy" <|
    s2b.status == 200 && jget s2b "send_status" == "sent" && jget s2b "id" != jget s2 "id" &&
      (old2.map (·.bool "deleted")) == some true && ((old2.map (·.get "client_id" |>.isNull))) == some true
  -- caps
  let many := ", ".intercalate ((List.range 21).map fun i => s!"\"r{i}@x.test\"")
  let s21 ← go "POST" "/api/send" [] s!"\{\"client_id\":\"{ulidN 3}\",\"from\":\"alex@fralex.art\",\"to\":[{many}],\"subject\":\"x\",\"text\":\"y\"}"
  let atts := ", ".intercalate ((List.range 11).map fun i => s!"\{\"filename\":\"f{i}.txt\",\"mime\":\"text/plain\",\"data_b64\":\"YWJj\"}")
  let s11 ← go "POST" "/api/send" [] s!"\{\"client_id\":\"{ulidN 4}\",\"from\":\"alex@fralex.art\",\"to\":[\"bob@x.test\"],\"subject\":\"x\",\"text\":\"y\",\"attachments\":[{atts}]}"
  let big := Core.Base64.encode (ByteArray.mk (List.replicate (3 * 1024 * 1024 + 1) (65 : UInt8)).toArray)
  let sBig ← go "POST" "/api/send" [] s!"\{\"client_id\":\"{ulidN 5}\",\"from\":\"alex@fralex.art\",\"to\":[\"bob@x.test\"],\"subject\":\"x\",\"text\":\"y\",\"attachments\":[\{\"filename\":\"b.bin\",\"mime\":\"application/octet-stream\",\"data_b64\":\"{big}\"}]}"
  check t "caps: 21 recipients is 400, 11 attachments is 400, over 3 MiB of attachments is 413" <|
    s21.status == 400 && jget s21 "error" == "too_many_recipients" && s11.status == 400 &&
      jget s11 "error" == "too_many_attachments" && sBig.status == 413 && (← calls.get).size == 3
  -- a reply
  let sr ← go "POST" "/api/send" [] s!"\{\"client_id\":\"{ulidN 6}\",\"from\":\"alex@fralex.art\",\"to\":[\"tomas@labelforge.io\"],\"subject\":\"Re: Quote\",\"text\":\"Sounds good.\",\"reply_of\":\"{pid}\"}"
  let wr := wireOf (((← calls.get).back?.map (·.2.1)).getD .null)
  let thr ← db.query "SELECT thread_id FROM messages WHERE id IN (?, ?)" #[pid, jget sr "id"]
  check t "reply_of sets In-Reply-To and References from the parent and threads the Sent copy with it" <|
    sr.status == 200 && wr.inReplyTo? == some "<parent1@labelforge.io>" &&
      wr.references? == some "<root@labelforge.io> <parent1@labelforge.io>" &&
      thr.size == 2 && thr[0]!.text "thread_id" == thr[1]!.text "thread_id" && jget sr "thread_id" == thr[0]!.text "thread_id"
  -- who may be sent as
  db.exec "INSERT INTO domains(name, sending, created_at, updated_at) VALUES ('fralex.art', '[\"fralex.art\"]', 0, 0)"
  db.exec "INSERT INTO addresses(address, domain, mail, can_send, created_at, updated_at) VALUES ('nosend@fralex.art', 'fralex.art', 'vault', 0, 0, 0), ('team@other.test', 'other.test', 'vault', 1, 0, 0)"
  let asSession (f : String) (n : Nat) := Send.send st (.session "s1") { clientId := ulidN n, from_ := f, to := [{ address := "bob@x.test" }], subject := "x", text := "y" }
  let rEve ← asSession "eve@evil.test" 7
  let rNo ← asSession "nosend@fralex.art" 8
  let rDom ← asSession "anyone@fralex.art" 9
  let rAddr ← asSession "team@other.test" 10
  let rDef ← asSession "alex@fralex.art" 11
  let statusOf (r : Except Send.Fail Send.Outcome) : Nat := match r with | .error f => f.status | .ok _ => 200
  check t "an unsendable from is 403; a sending domain, a can_send address and default_from are allowed" <|
    statusOf rEve == 403 && statusOf rNo == 403 && statusOf rDom == 200 && statusOf rAddr == 200 && statusOf rDef == 200

  -- drafts
  let did := ulidN 20
  let d1 ← go "PUT" s!"/api/drafts/{did}" [] "{\"mutation_id\":\"m1\",\"to\":[\"Bob <bob@x.test>\"],\"subject\":\"Draft one\",\"text\":\"hi\"}"
  let d1b ← go "PUT" s!"/api/drafts/{did}" [] "{\"mutation_id\":\"m1\",\"to\":[\"other@x.test\"],\"subject\":\"changed\",\"text\":\"hi\"}"
  check t "PUT /api/drafts/:id creates at revision 1 with a send_id; a repeated mutation_id is idempotent" <|
    d1.status == 201 && jnat d1 "revision" == 1 && Core.Ulid.isValid (jget d1 "send_id") &&
      d1b.status == 200 && jnat d1b "revision" == 1 && jget d1b "subject" == "Draft one"
  let d2bad ← go "PUT" s!"/api/drafts/{did}" [("if-match", "0")] "{\"mutation_id\":\"m2\",\"subject\":\"stale\"}"
  let d2noRev ← go "PUT" s!"/api/drafts/{did}" [] "{\"mutation_id\":\"m2\",\"subject\":\"stale\"}"
  let d2 ← go "PUT" s!"/api/drafts/{did}" [("if-match", "1")] "{\"mutation_id\":\"m2\",\"from\":\"alex@fralex.art\",\"to\":[\"Bob <bob@x.test>\"],\"subject\":\"Draft two\",\"text\":\"hello\",\"attachments\":[{\"filename\":\"a.txt\",\"mime\":\"text/plain\",\"data_b64\":\"YWJj\"}]}"
  check t "draft CAS: a stale or missing revision is 409 {error:revision, revision}; the right one bumps it" <|
    d2bad.status == 409 && jget d2bad "error" == "revision" && jnat d2bad "revision" == 1 && d2noRev.status == 409 &&
      d2.status == 200 && jnat d2 "revision" == 2 && jget d2 "send_id" != jget d1 "send_id"
  let dl ← go "GET" "/api/drafts"
  let dg ← go "GET" s!"/api/drafts/{did}"
  check t "GET /api/drafts lists the draft; GET /api/drafts/:id has its content and attachment data" <|
    dl.status == 200 && has dl.bodyText did && !has dl.bodyText "YWJj" && dg.status == 200 && has dg.bodyText "\"data_b64\":\"YWJj\"" &&
      jget dg "subject" == "Draft two"
  let sendId := jget d2 "send_id"
  let ds ← go "POST" s!"/api/drafts/{did}/send" [] "{\"revision\":2}"
  let dsRow ← db.queryOne "SELECT client_id, has_attachments FROM messages WHERE id = ?" #[jget ds "id"]
  let dState ← db.queryOne "SELECT state FROM drafts WHERE id = ?" #[did]
  check t "sending a draft uses its send_id as client_id and marks the draft sent" <|
    ds.status == 200 && jget ds "send_status" == "sent" && (dsRow.map (·.text "client_id")) == some sendId &&
      (dsRow.map (·.bool "has_attachments")) == some true && (dState.map (·.text "state")) == some "sent"
  let ds2 ← go "POST" s!"/api/drafts/{did}/send" [] "{\"revision\":2}"
  check t "sending the same draft revision again is idempotent" (ds2.status == 200 && jget ds2 "id" == jget ds "id")
  let did2 := ulidN 21
  let e1 ← go "PUT" s!"/api/drafts/{did2}" [] "{\"mutation_id\":\"m1\",\"to\":[\"bob@x.test\"],\"subject\":\"d\",\"text\":\"t\"}"
  let e2 ← go "POST" "/api/send" [] s!"\{\"client_id\":\"{jget e1 "send_id"}\",\"from\":\"alex@fralex.art\",\"to\":[\"bob@x.test\"],\"subject\":\"d\",\"text\":\"t\",\"draft_id\":\"{did2}\",\"draft_revision\":5}"
  let e3 ← go "DELETE" s!"/api/drafts/{did2}" [("if-match", "1")]
  let e4 ← go "GET" s!"/api/drafts/{did2}"
  check t "/api/send with a draft_id at the wrong revision is 409; DELETE removes the draft" <|
    e2.status == 409 && jget e2 "error" == "revision" && e3.status == 200 && e4.status == 404

  -- pages
  let html := [("accept", "text/html")]
  let rp ← go "GET" "/compose" html "" [("reply", pid), ("all", "1")]
  let rb := rp.bodyText
  check t "/compose?reply=<id>&all=1 prefills To, Cc without our own address, Re: subject and the quote" <|
    rp.status == 200 && has rb "name=\"to\" value=\"Tomas Reyes &lt;tomas@labelforge.io&gt;\"" &&
      has rb "Bob &lt;bob@x.test&gt;" && has rb "carol@y.test" && !has rb "name=\"cc\" value=\"Alex" &&
      has rb "value=\"Re: Quote\"" && has rb "wrote:\n&gt; Line one\n&gt; Line two" &&
      has rb s!"name=\"reply_of\" value=\"{pid}\"" && has rb "action=\"/compose/send\"" && has rb "enctype=\"multipart/form-data\""
  let fw ← go "GET" "/compose" html "" [("forward", pid)]
  check t "/compose?forward=<id> has Fwd: and the forward_of reference" <|
    fw.status == 200 && has fw.bodyText "value=\"Fwd: Quote\"" && has fw.bodyText s!"name=\"forward_of\" value=\"{pid}\""
  let bnd := "----lmTestBoundary42"
  let mp := s!"--{bnd}\r\nContent-Disposition: form-data; name=\"to\"\r\n\r\nbob@x.test, Carol <carol@y.test>\r\n--{bnd}\r\nContent-Disposition: form-data; name=\"subject\"\r\n\r\nForm send\r\n--{bnd}\r\nContent-Disposition: form-data; name=\"body\"\r\n\r\nLine A\r\nLine B\r\n--{bnd}\r\nContent-Disposition: form-data; name=\"client_id\"\r\n\r\n{ulidN 30}\r\n--{bnd}\r\nContent-Disposition: form-data; name=\"attachment\"; filename=\"résumé.txt\"\r\nContent-Type: text/plain\r\n\r\nfile\r\n--body\r\n--{bnd}--\r\n"
  let parts := Pages.parseMultipart s!"multipart/form-data; boundary={bnd}" mp.toUTF8
  check t "the multipart form parser reads fields and a file, keeping the file's bytes" <|
    match parts with
    | some ps => ps.size == 5 && Pages.partText ps "to" == "bob@x.test, Carol <carol@y.test>" &&
        Pages.partText ps "body" == "Line A\nLine B" &&
        (match Pages.partFiles ps with
         | [f] => f.filename == "résumé.txt" && f.mime == "text/plain" && f.data == "file\r\n--body".toUTF8
         | _ => false)
    | none => false
  let fs ← go "POST" "/compose/send" (html ++ [("content-type", s!"multipart/form-data; boundary={bnd}")]) mp
  let loc := (fs.headers.lookup "Location").getD ""
  let fsId := String.ofList ((loc.drop 3).toString.toList.takeWhile (· != '?'))
  let fsRow ← db.queryOne "SELECT subject, folder, has_attachments, to_json, cc_json FROM messages WHERE id = ?" #[fsId]
  check t "POST /compose/send (multipart) sends and redirects to the Sent copy with a flash" <|
    fs.status == 303 && loc.startsWith "/m/" && loc.endsWith "?sent=sent" &&
      (fsRow.map (·.text "subject")) == some "Form send" && (fsRow.map (·.bool "has_attachments")) == some true
  let mpage ← go "GET" s!"/m/{fsId}" html "" [("sent", "sent")]
  check t "the message page has Reply links and the sent flash" <|
    mpage.status == 200 && has mpage.bodyText s!"href=\"/compose?reply={fsId}\"" && has mpage.bodyText "Sent."
  let sv ← go "POST" "/compose/save" (html ++ [("content-type", "application/x-www-form-urlencoded")])
    s!"draft_id={ulidN 40}&revision=0&to=bob%40x.test&subject=Saved&body=draft+text"
  let svPage ← go "GET" "/compose" html "" [("draft", ulidN 40)]
  let drPage ← go "GET" "/drafts" html
  check t "/compose/save stores a draft and reopens it; /drafts lists it" <|
    sv.status == 303 && ((sv.headers.lookup "Location").map (·.startsWith s!"/compose?draft={ulidN 40}")) == some true &&
      svPage.status == 200 && has svPage.bodyText "value=\"Saved\"" && has svPage.bodyText "draft text" &&
      drPage.status == 200 && has drPage.bodyText s!"/compose?draft={ulidN 40}"
  let sentTab ← go "GET" "/inbox" html "" [("filter", "sent")]
  let inboxTab ← go "GET" "/inbox" html
  let apiSent ← go "GET" "/api/msgs" [] "" [("folder", "sent")]
  check t "the Sent tab and /api/msgs?folder=sent list sent mail; the inbox does not" <|
    sentTab.status == 200 && has sentTab.bodyText s!"/m/{id1}\"" && has sentTab.bodyText "href=\"/inbox?filter=sent\"" &&
      !has inboxTab.bodyText s!"/m/{id1}\"" && apiSent.status == 200 && has apiSent.bodyText id1 && !has apiSent.bodyText pid

  -- the daily cap
  let today := ((← db.queryOne "SELECT count(*) AS n FROM messages WHERE source = 'sent' AND deleted = 0").map (·.nat "n")).getD 0
  for i in [0:(200 - today)] do
    db.exec "INSERT INTO messages (id, dedupe_key, source, folder, received_at, updated_at, send_status) VALUES (?, ?, 'sent', 'sent', ?, ?, 'sent')"
      #[s!"CAP{i}", Crypto.sha256Text s!"cap{i}", now, now]
  let sc ← go "POST" "/api/send" [] s!"\{\"client_id\":\"{ulidN 50}\",\"from\":\"alex@fralex.art\",\"to\":[\"bob@x.test\"],\"subject\":\"x\",\"text\":\"y\"}"
  check t "the 201st send of the UTC day is 429" (sc.status == 429 && jget sc "error" == "send_cap")

/-- Import fidelity: R2 metadata on the wire, every spool metadata key, the import route's state
headers, no push for imports, and a concurrent drain. -/
def spoolChecks (t : IO.Ref Tally) : IO Unit := do
  Log.setLevel "warn"
  check t "r2 custom metadata goes out as x-amz-meta-<lower-case key>" <|
    R2.metaHeaders [("Received_At", "1"), ("rcpt", "a@b")] == [("x-amz-meta-received_at", "1"), ("x-amz-meta-rcpt", "a@b")]
  check t "r2 custom metadata is read back from x-amz-meta-* headers only, any case" <|
    R2.metaFromHeaders [("X-Amz-Meta-Junk_Kind", "spam"), ("x-amz-meta-rcpts", "[]"), ("Content-Type", "x"),
      ("x-amz-md-old", "1")] == [("junk_kind", "spam"), ("rcpts", "[]")]
  check t "r2 metadata survives the header round trip" <|
    let md := [("received_at", "1790000000000"), ("source", "import"), ("expires_at", "never")]
    R2.metaFromHeaders (R2.metaHeaders md) == md
  let db ← Db.openMemory
  discard <| Schema.migrate db
  let store ← Store.memory
  let (calls, worker) ← recorder
  let st : State := { config := { devBypass := true }, db, store, worker, startedAt := 0, schemaVersion := 2, fts5 := true }
  let now ← Clock.nowMs
  let mk (n : Nat) : String := Core.Ulid.encode (1790000000000 + n) (ByteArray.mk #[7, 7, 7, 7, 7, 7, 7, 7, 0, n.toUInt8])
  let body (n : Nat) : ByteArray :=
    s!"From: s{n}@old.example\r\nSubject: spooled {n}\r\nMessage-ID: <sp{n}@old.example>\r\n\r\nbody {n}\r\n".toUTF8
  let spool (n : Nat) (md : List (String × String)) : IO String := do
    let id := mk n
    store.put s!"raw/dev/{id}" (body n) md
    store.put s!"in/dev/{id}" .empty []
    return id
  -- every key the exporter writes
  let snooze := now + 86400000
  let aId ← spool 1 [("rcpt", "a@fralex.art"), ("rcpts", "[\"a@fralex.art\",\"B@fralex.art\",\"c@fralex.art\"]"),
    ("received_at", "1790000000001"), ("source", "import"), ("size", toString (body 1).size), ("folder", "inbox"),
    ("expires_at", "never"), ("archived_at", "1790000000100"), ("snoozed_until", toString snooze),
    ("to_read_at", "1790000000200")]
  let bId ← spool 2 [("rcpt", "a@fralex.art"), ("received_at", "1790000000002"), ("source", "import"),
    ("expires_at", "1800000000000"), ("junk_kind", "spam")]
  let cId ← spool 3 [("received_at", "1790000000003"), ("source", "import"), ("folder", "sent"),
    ("rcpts", "[\"bob@x.test\"]"), ("expires_at", "never")]
  -- what the Worker's email() writes, and nothing else
  let dId ← spool 4 [("rcpt", "a@fralex.art"), ("from", "s4@old.example"), ("received_at", "1790000000004"),
    ("source", "route"), ("size", toString (body 4).size)]
  let drained ← Spool.drain st
  let row (id : String) := db.queryOne "SELECT * FROM messages WHERE id = ?" #[id]
  check t "the spool drains the four metadata markers" (drained == 4 && (← store.list "in/dev/").isEmpty)
  check t "spool metadata: rcpts merge with rcpt, source, received_at, archive, snooze, to-read and never expiry" <|
    match ← row aId with
    | some r => r.text "rcpts" == "[\"a@fralex.art\",\"b@fralex.art\",\"c@fralex.art\"]" && r.text "source" == "import" &&
        r.nat "received_at" == 1790000000001 && r.nat "archived_at" == 1790000000100 && r.nat "snoozed_until" == snooze &&
        r.nat "to_read_at" == 1790000000200 && (r.get "expires_at").isNull && r.text "folder" == "inbox" &&
        (r.get "junk_at").isNull
    | none => false
  check t "spool metadata: junk_kind files it as junk (junk_at set) and a numeric expires_at is kept" <|
    match ← row bId with
    | some r => r.text "junk_kind" == "spam" && (r.nat? "junk_at").isSome && r.nat "expires_at" == 1800000000000
    | none => false
  check t "spool metadata: folder sent and a one-entry rcpts list" <|
    match ← row cId with
    | some r => r.text "folder" == "sent" && r.text "rcpts" == "[\"bob@x.test\"]" && (r.get "expires_at").isNull
    | none => false
  check t "spool metadata: the Worker's keys alone keep the defaults (inbox, route, retention expiry)" <|
    match ← row dId with
    | some r => r.text "folder" == "inbox" && r.text "source" == "route" && r.text "rcpts" == "[\"a@fralex.art\"]" &&
        r.nat "expires_at" > 1790000000004 && (r.get "archived_at").isNull && (r.get "junk_at").isNull
    | none => false
  let list (filter : String) : IO String := do
    let r ← Router.dispatch st Api.routes
      { method := "GET", path := ["api", "msgs"], query := [("filter", filter)], headers := [("x-account-id", "dev")] }
    return r.bodyText
  let inbox ← list "inbox"
  let archived ← list "archived"
  let junk ← list "junk"
  check t "a spooled archived message is in Archived, not the inbox; junk is in Junk; route mail is in the inbox" <|
    !has inbox aId && has archived aId && !has inbox bId && has junk bId && has inbox dId
  -- the import route's state headers
  let impId := "01K0000000000000000000IMP9"
  let impHs := [("x-account-id", "dev"), ("x-id", impId), ("x-received-at", "1790000000050"), ("x-source", "import"),
    ("x-folder", "inbox"), ("x-rcpts", "[\"a@fralex.art\"]"), ("x-expires-at", "never"),
    ("x-snoozed-until", toString snooze), ("x-to-read-at", "1790000000060"), ("x-junk-kind", "phish")]
  let impBody := "From: imp9@old.example\r\nSubject: imported nine\r\nMessage-ID: <imp9@old.example>\r\n\r\nnine\r\n".toUTF8
  let impReq : Router.Req := { method := "POST", path := ["api", "import"], headers := impHs, body := impBody }
  let imp ← Router.dispatch st Api.routes impReq
  check t "POST /api/import keeps x-snoozed-until, x-to-read-at and x-junk-kind (junk_at set)" <|
    imp.status == 201 &&
      match ← row (Core.Ulid.normalize impId) with
      | some r => r.nat "snoozed_until" == snooze && r.nat "to_read_at" == 1790000000060 && r.text "junk_kind" == "phish" &&
          (r.nat? "junk_at").isSome && (r.get "expires_at").isNull && r.text "source" == "import"
      | none => false
  let plainHs := [("x-account-id", "dev"), ("x-id", "01K0000000000000000000IMPA"), ("x-received-at", "1790000000070")]
  let plainBody := "From: impa@old.example\r\nSubject: imported ten\r\nMessage-ID: <impa@old.example>\r\n\r\nten\r\n".toUTF8
  let plain ← Router.dispatch st Api.routes { impReq with headers := plainHs, body := plainBody }
  check t "an import without state headers is inbox mail with no snooze, to-read or junk" <|
    plain.status == 201 &&
      match ← row (Core.Ulid.normalize "01K0000000000000000000IMPA") with
      | some r => (r.get "snoozed_until").isNull && (r.get "to_read_at").isNull && (r.get "junk_at").isNull &&
          r.text "folder" == "inbox" && r.text "source" == "import"
      | none => false
  -- pushes
  let base : Ingest.Incoming := { raw := .empty, key := none, rcpt := none, source := "route", receivedAt := 0 }
  check t "only new route/gmail mail pushes: never an import, a Sent copy or junk" <|
    Ingest.shouldPush base && Ingest.shouldPush { base with source := "gmail" } &&
      !Ingest.shouldPush { base with source := "import" } && !Ingest.shouldPush { base with source := "sent" } &&
      !Ingest.shouldPush { base with folder := "sent" } && !Ingest.shouldPush { base with junkKind := some "spam" }
  check t "no push reaches the Worker for imported mail" <|
    !((← calls.get).any fun (_, p, _) => has p "push")
  -- a concurrent drain
  let mut ids : Array String := #[]
  for k in [0:50] do
    ids := ids.push (← spool (100 + k) [("rcpt", "a@fralex.art"), ("received_at", toString (1790000000100 + k)), ("source", "route")])
  let before := ((← db.queryOne "SELECT count(*) AS n FROM messages").map (·.nat "n")).getD 0
  let n50 ← Spool.drain st
  let after := ((← db.queryOne "SELECT count(*) AS n FROM messages").map (·.nat "n")).getD 0
  let mut allThere := true
  for id in ids do
    if (← row id).isNone then allThere := false
  check t s!"a drain of 50 markers, 8 at a time, stores all 50 and consumes every marker" <|
    n50 == 50 && after == before + 50 && allThere && (← store.list "in/dev/").isEmpty

/-! ## M4: Gmail, against a fake Google -/

/-- A fake Google: OAuth's token endpoint and the Gmail API calls the lanes make, with the
mailbox in refs. Every request is recorded as (method, url, body). -/
structure FakeG where
  calls : IO.Ref (Array (String × String × String))
  /-- id → (raw, labels, internalDate) -/
  msgs : IO.Ref (List (String × String × Array String × Nat))
  history : IO.Ref (Array String)
  history404 : IO.Ref Bool
  pages : IO.Ref (Array (Array String))
  gapIds : IO.Ref (Array String)
  refreshFails : IO.Ref Bool
  /-- ids whose next raw fetch answers 500 -/
  failOnce : IO.Ref (List String)
  profileHid : IO.Ref String

def FakeG.new : IO FakeG := do
  return { calls := ← IO.mkRef #[], msgs := ← IO.mkRef [], history := ← IO.mkRef #[], history404 := ← IO.mkRef false,
           pages := ← IO.mkRef #[], gapIds := ← IO.mkRef #[], refreshFails := ← IO.mkRef false,
           failOnce := ← IO.mkRef [], profileHid := ← IO.mkRef "100" }

def gResp (status : UInt32) (j : Lean.Json) : Core.Http.Response :=
  { status, headers := [("Content-Type", "application/json")], body := j.compress.toUTF8 }

def qparam (q k : String) : Option String :=
  ((q.splitOn "&").find? (·.startsWith (k ++ "="))).map fun p => Pages.urlDecode (String.ofList (p.toList.drop (k.length + 1)))

def gmailMsg (n : Nat) (subject : String) : String :=
  s!"From: Ann <ann{n}@example.com>\r\nTo: alex@gmail.com\r\nSubject: {subject}\r\nMessage-ID: <g{n}@example.com>\r\nDate: Thu, 10 Sep 2026 09:00:00 +0000\r\n\r\nbody {n}\r\n"

def FakeG.addMsg (g : FakeG) (id : String) (n : Nat) (labels : Array String) : IO Unit :=
  g.msgs.modify ((id, gmailMsg n s!"Gmail message {n}", labels, 1788944400000 + n) :: ·)

def FakeG.labels (g : FakeG) (id : String) : IO (Array String) := do
  return ((← g.msgs.get).find? (·.1 == id)).map (·.2.2.1) |>.getD #[]

def FakeG.api (g : FakeG) : GmailApi where
  request r := do
    let bodyS := (String.fromUTF8? r.body).getD ""
    g.calls.modify (·.push (r.method, r.url, bodyS))
    if r.url == Gmail.tokenUrl then
      if has bodyS "grant_type=authorization_code" then
        return gResp 200 (Lean.Json.mkObj [("access_token", Lean.Json.str "at1"), ("refresh_token", Lean.Json.str "rt1"), ("expires_in", Lean.Json.num 3599)])
      if ← g.refreshFails.get then return gResp 400 (Lean.Json.mkObj [("error", Lean.Json.str "invalid_grant")])
      return gResp 200 (Lean.Json.mkObj [("access_token", Lean.Json.str "at2"), ("expires_in", Lean.Json.num 3599)])
    let rest := match r.url.splitOn Gmail.apiBase with | _ :: x :: _ => x | _ => ""
    let (path, q) := match rest.splitOn "?" with
      | [p] => (p, "")
      | p :: qs => (p, "?".intercalate qs)
      | [] => ("", "")
    let segs := (path.splitOn "/").filter (· ≠ "")
    let msg? (id : String) := do return (← g.msgs.get).find? (·.1 == id)
    match r.method, segs with
    | "GET", ["profile"] =>
      return gResp 200 (Lean.Json.mkObj [("emailAddress", Lean.Json.str "Alex@Gmail.com"), ("messagesTotal", Lean.Json.num 3), ("historyId", Lean.Json.str (← g.profileHid.get))])
    | "GET", ["settings", "sendAs"] =>
      return gResp 200 (Lean.Json.mkObj [("sendAs", Lean.Json.arr #[
        Lean.Json.mkObj [("sendAsEmail", Lean.Json.str "alex@gmail.com"), ("isPrimary", Lean.Json.bool true)],
        Lean.Json.mkObj [("sendAsEmail", Lean.Json.str "alias@fralex.art"), ("verificationStatus", Lean.Json.str "accepted")],
        Lean.Json.mkObj [("sendAsEmail", Lean.Json.str "pending@fralex.art"), ("verificationStatus", Lean.Json.str "pending")]])])
    | "GET", ["history"] =>
      if ← g.history404.get then return gResp 404 (Lean.Json.mkObj [("error", Lean.Json.mkObj [("code", Lean.Json.num 404)])])
      let ids ← g.history.get
      return gResp 200 (Lean.Json.mkObj [("historyId", Lean.Json.str "105"), ("history", Lean.Json.arr #[Lean.Json.mkObj [("id", Lean.Json.str "101"),
        ("messagesAdded", Lean.Json.arr (ids.map fun i => Lean.Json.mkObj [("message", Lean.Json.mkObj [("id", Lean.Json.str i)])]))]])])
    | "GET", ["messages"] =>
      if (qparam q "q").isSome then
        return gResp 200 (Lean.Json.mkObj [("messages", Lean.Json.arr ((← g.gapIds.get).map fun i => Lean.Json.mkObj [("id", Lean.Json.str i)]))])
      let pages ← g.pages.get
      let i := ((qparam q "pageToken").bind fun p => (String.ofList (p.toList.drop 1)).toNat?).getD 0
      let ids := pages.getD i #[]
      return gResp 200 (Lean.Json.mkObj ([("messages", Lean.Json.arr (ids.map fun x => Lean.Json.mkObj [("id", Lean.Json.str x)])), ("resultSizeEstimate", Lean.Json.num 3)] ++
        (if i + 1 < pages.size then [("nextPageToken", Lean.Json.str s!"p{i + 1}")] else [])))
    | "GET", ["messages", id] =>
      let some (_, raw, labels, at_) ← msg? id | return gResp 404 (Lean.Json.mkObj [])
      if (qparam q "format") == some "raw" then
        if (← g.failOnce.get).contains id then
          g.failOnce.modify (·.erase id)
          return gResp 500 (Lean.Json.mkObj [])
        return gResp 200 (Lean.Json.mkObj [("id", Lean.Json.str id), ("raw", Lean.Json.str (Core.Base64.encodeUrl raw.toUTF8)),
          ("labelIds", Lean.Json.arr (labels.map Lean.Json.str)), ("internalDate", Lean.Json.str (toString at_)), ("threadId", Lean.Json.str s!"t-{id}")])
      if (qparam q "fields").isSome then return gResp 200 (Lean.Json.mkObj [("id", Lean.Json.str id), ("labelIds", Lean.Json.arr (labels.map Lean.Json.str))])
      return gResp 200 (Lean.Json.mkObj [("id", Lean.Json.str id), ("threadId", Lean.Json.str s!"t-{id}")])
    | "POST", ["messages", id, "modify"] =>
      let some _ ← msg? id | return gResp 404 (Lean.Json.mkObj [])
      let b := (Lean.Json.parse bodyS).toOption.getD .null
      let strs (k : String) := ((b.getObjValAs? (Array String) k).toOption).getD #[]
      g.msgs.modify fun ms => ms.map fun (i, raw, ls, at_) =>
        if i == id then (i, raw, ((ls.filter (!(strs "removeLabelIds").contains ·)) ++ (strs "addLabelIds")), at_) else (i, raw, ls, at_)
      return gResp 200 (Lean.Json.mkObj [("id", Lean.Json.str id)])
    | "POST", ["messages", "send"] => return gResp 200 (Lean.Json.mkObj [("id", Lean.Json.str "gsent1"), ("threadId", Lean.Json.str "t1")])
    | "POST", ["watch"] =>
      return gResp 200 (Lean.Json.mkObj [("historyId", Lean.Json.str "200"), ("expiration", Lean.Json.str (toString ((← Clock.nowMs) + 7 * 86400000)))])
    | "POST", ["stop"] => return gResp 204 .null
    | _, _ => return gResp 404 (Lean.Json.mkObj [])

def FakeG.count (g : FakeG) (p : String × String × String → Bool) : IO Nat := do
  return ((← g.calls.get).filter p).size

def gmailChecks (t : IO.Ref Tally) : IO Unit := do
  let db ← Db.openMemory
  discard <| Schema.migrate db
  db.exec "INSERT INTO account (id, email, created_at, updated_at) VALUES (1, 'alex@fralex.art', 0, 0)"
  let g ← FakeG.new
  let (wcalls, worker) ← recorder
  let cfg : Config := { devBypass := true, internalSecret := "isec", sessionSecret := "k-gmail" }
  let st : State := { config := cfg, db, store := ← Store.memory, worker, startedAt := 0, schemaVersion := 5, fts5 := true,
                      gmail := g.api }
  Gmail.OAuth.clearCache
  let mk (method path : String) (q : List (String × String) := []) (body : String := "") (extra : List (String × String) := []) : Router.Req :=
    { method, path := (path.splitOn "/").filter (· ≠ ""), query := q, headers := [("x-account-id", "dev")] ++ extra, body := body.toUTF8 }
  let go (r : Router.Req) : IO Router.Resp := Router.dispatch st (Routes.all cfg) r
  let loc (r : Router.Resp) : String := ((r.headers.find? (·.1 == "Location")).map (·.2)).getD ""
  let connRow ← (pure () : IO Unit)
  let _ := connRow
  let conn : IO (Option Db.Row) := db.queryOne "SELECT * FROM gmail_connection WHERE id = 1"

  check t "gmail: nothing connected means no X-Next-Wake" ((← Jobs.nextWake st) == none)
  -- OAuth: the client, the consent URL, the state
  let bad ← go (mk "POST" "/api/gmail/client" (body := "{\"client_id\":\"nope\",\"client_secret\":\"0123456789ab\"}"))
  let okc ← go (mk "POST" "/api/gmail/client" (body := "{\"client_id\":\"123.apps.googleusercontent.com\",\"client_secret\":\"0123456789ab\"}"))
  check t "gmail: a client id that is not Google's is refused; a good one is stored" <|
    bad.status == 400 && okc.status == 200 && ((← conn).map (·.text "client_id")) == some "123.apps.googleusercontent.com"
  let az ← go (mk "GET" "/api/gmail/authorize")
  let url := loc az
  check t "gmail: authorize is a 302 to Google with offline access, consent, the scopes and our redirect" <|
    az.status == 302 && url.startsWith Gmail.authUrl && has url "access_type=offline" && has url "prompt=consent" &&
      has url "gmail.modify" && has url "gmail.settings.basic" && has url "gmail.settings.sharing" &&
      has url (R2.uriEncode "http://127.0.0.1:8787/oauth/google/callback")
  let state := Pages.urlDecode (afterStr url "state=")
  let now ← Clock.nowMs
  let nonce := Gmail.OAuth.verifyState cfg now state
  check t "gmail: the state is v1.<acct>.goauth.<nonce>.<exp>.<mac> and round-trips" <|
    state.startsWith "v1.dev.goauth." && nonce.isSome && (← Gmail.setting? st Gmail.OAuth.nonceKey) == nonce
  check t "gmail: the state is not a magic link or a session, and fails tampered, expired or for another account" <|
    Auth.Tokens.verifyMagic cfg now state == none && Auth.Tokens.verifySession cfg now state == none &&
      Gmail.OAuth.verifyState cfg now (state ++ "x") == none &&
      Gmail.OAuth.verifyState cfg (now + 11 * 60000) state == none &&
      Gmail.OAuth.verifyState { cfg with accountId := "other" } now state == none
  let badCb ← go (mk "GET" "/oauth/google/callback" [("code", "c1"), ("state", "v1.dev.goauth.x.1.y")])
  check t "gmail: a callback with a forged state is sent back with error=state" <|
    badCb.status == 302 && loc badCb == "/settings/gmail?error=state" && ((← conn).bind (·.text? "refresh_token")).isNone
  let cb ← go (mk "GET" "/oauth/google/callback" [("code", "c1"), ("state", state)])
  let c1 ← conn
  check t "gmail: the callback exchanges the code, stores the refresh token and the mailbox, and redirects" <|
    cb.status == 302 && loc cb == "/settings/gmail?connected=1" &&
      (c1.map (·.text "refresh_token")) == some "rt1" && (c1.map (·.text "email")) == some "alex@gmail.com" &&
      (c1.map (·.text "history_id")) == some "100" &&
      (← g.count fun (m, u, b) => m == "POST" && u == Gmail.tokenUrl && has b "code=c1" && has b "grant_type=authorization_code") == 1
  let again ← go (mk "GET" "/oauth/google/callback" [("code", "c1"), ("state", state)])
  check t "gmail: a state is single use" (loc again == "/settings/gmail?error=state")
  check t "gmail: send-as identities are the mailbox and its verified aliases" <|
    (← Gmail.SendAs.identities st) == ["alex@gmail.com", "alias@fralex.art"]
  check t "gmail: a connection without a watch wakes every 300 s" ((← Jobs.nextWake st) == some 300)
  let status ← go (mk "GET" "/api/gmail")
  let sig := Crypto.base64Url (Crypto.hmacText "isec" "gmail-push:dev")
  check t "gmail: GET /api/gmail reports the connection and the Worker hook URL" <|
    status.status == 200 && jget status "email" == "alex@gmail.com" &&
      jget status "push_url" == s!"http://127.0.0.1:8787/hooks/gmail/dev/{sig}"

  -- poll
  g.addMsg "m1" 1 #["INBOX", "UNREAD"]
  g.addMsg "m2" 2 #["CATEGORY_UPDATES"]
  g.history.set #["m1", "m2"]
  let p1 ← Gmail.Poll.poll st
  let r1 ← db.queryOne "SELECT * FROM messages WHERE gm_msgid = 'm1'"
  let r2 ← db.queryOne "SELECT * FROM messages WHERE gm_msgid = 'm2'"
  check t "gmail: the poll ingests the two new messages from history.list and messages.get" <|
    p1.stored == 2 && (r1.map (·.text "source")) == some "gmail" && (r2.map (·.text "source")) == some "gmail" &&
      (r1.map (·.text "subject")) == some "Gmail message 1" && ((r1.map (·.text "gm_labels")).getD "").startsWith "[\"INBOX\""
  check t "gmail: a message without INBOX is stored done; one with it is not" <|
    ((r1.bind (·.nat? "archived_at")).isNone) && ((r2.bind (·.nat? "archived_at")).isSome)
  let rawKey := (r1.map (·.text "raw_key")).getD ""
  check t "gmail: the raw message is in the store under raw/<acct>/<id>" <|
    rawKey == s!"raw/dev/{(r1.map (·.text "id")).getD ""}" && (← st.store.get rawKey).isSome
  check t "gmail: the cursor moves to the mailbox's history id" (((← conn).map (·.text "history_id")) == some "105")
  let fetches0 ← g.count fun (_, u, _) => has u "format=raw"
  let p2 ← Gmail.Poll.poll st
  let n ← db.queryOne "SELECT count(*) AS n FROM messages WHERE source = 'gmail'"
  check t "gmail: a repeat is a duplicate by gm_msgid, without fetching it again" <|
    p2.duplicates == 2 && p2.stored == 0 && (n.map (·.nat "n")) == some 2 && (← g.count fun (_, u, _) => has u "format=raw") == fetches0
  -- the history gap
  g.history404.set true
  g.profileHid.set "300"
  g.addMsg "m3" 3 #["INBOX"]
  g.gapIds.set #["m1", "m3"]
  let p3 ← Gmail.Poll.poll st
  check t "gmail: a 404 history is a gap: messages.list since a day before, missing ones taken, cursor reset" <|
    p3.gap && p3.queued == 1 && (← Gmail.heldId? st "m3").isSome && ((← conn).map (·.text "history_id")) == some "300" &&
      (← g.count fun (_, u, _) => has u "/messages?" && has u "q=after%3A") == 1 &&
      ((← db.queryOne "SELECT count(*) AS n FROM history_failures").map (·.nat "n")) == some 0
  g.history404.set false
  g.history.set #[]
  -- a failure is written down and retried
  g.addMsg "m4" 4 #["INBOX"]
  g.history.set #["m4"]
  g.failOnce.set ["m4"]
  let p4 ← Gmail.Poll.poll st
  let f4 ← db.queryOne "SELECT * FROM history_failures WHERE gm_msgid = 'm4'"
  check t "gmail: a message the poll cannot fetch goes to history_failures and the cursor still moves" <|
    p4.failed == 1 && (f4.map (·.text "code")) == some "http_500" && (f4.map (·.text "job_id")) == some "poll"
  db.exec "UPDATE history_failures SET last_at = 0 WHERE gm_msgid = 'm4'"
  g.history.set #[]
  let p5 ← Gmail.Poll.poll st
  check t "gmail: its retry, once due, stores it and clears the record" <|
    p5.stored == 1 && (← Gmail.heldId? st "m4").isSome && (← db.queryOne "SELECT 1 AS x FROM history_failures").isNone

  -- the lane lease
  let inner ← IO.mkRef true
  let outer ← Jobs.runLane st "test.lane" 60000 (do inner.set (← Jobs.runLane st "test.lane" 60000 (pure ())))
  let third ← Jobs.runLane st "test.lane" 60000 (throw (IO.userError "google:http_503"))
  let jl ← db.queryOne "SELECT * FROM jobs WHERE name = 'test.lane'"
  check t "gmail: a leased lane does not run twice at once; the lease is freed and the error recorded" <|
    outer && !(← inner.get) && third && (jl.bind (·.nat? "lease_until")).isNone && (jl.map (·.text "last_error")) == some "http_503" &&
      (jl.bind (·.nat? "last_run_at")).isSome

  -- backfill
  g.addMsg "b1" 11 #["INBOX"]
  g.addMsg "b2" 12 #[]
  g.addMsg "b3" 13 #["SENT"]
  g.pages.set #[#["b1", "b2"], #["b3"]]
  g.failOnce.set ["b3"]
  let bf ← go (mk "POST" "/api/gmail/backfill" (body := "{}"))
  check t "gmail: POST /api/gmail/backfill starts a job with the mailbox's count; a second is 409" <|
    bf.status == 200 && has bf.bodyText "\"state\":\"running\"" && has bf.bodyText "\"planned_total\":3" &&
      (← go (mk "POST" "/api/gmail/backfill" (body := "{}"))).status == 409
  check t "gmail: a running backfill wakes every 60 s" ((← Jobs.nextWake st) == some 60)
  let w ← Jobs.withNextWake st (Router.Resp.okStatus)
  check t "gmail: responses carry X-Next-Wake" (w.headers.contains ("X-Next-Wake", "60"))
  let ran1 ← Jobs.backfillLane st
  let j1 ← Gmail.Backfill.active? st
  check t "gmail: a step commits the first page and stops on Google's 500 without committing the second" <|
    ran1 && (j1.map (·.text "state")) == some "running" && (j1.map (·.text "page_token")) == some "p1" &&
      (j1.map (·.nat "seen")) == some 2 && (j1.map (·.text "error_code")) == some "http_500" &&
      (← Gmail.heldId? st "b1").isSome && (← Gmail.heldId? st "b2").isSome && (← Gmail.heldId? st "b3").isNone
  let now2 ← Clock.nowMs
  db.exec "UPDATE jobs SET lease_until = ? WHERE name = 'gmail.backfill'" #[((now2 + 100000 : Nat) : Db.Value)]
  let ran2 ← Jobs.backfillLane st
  check t "gmail: while another run holds the lease the step is skipped" (!ran2)
  db.exec "UPDATE jobs SET lease_until = ? WHERE name = 'gmail.backfill'" #[((now2 - 1 : Nat) : Db.Value)]
  let b1Fetches ← g.count fun (_, u, _) => has u "/messages/b1?format=raw"
  let ran3 ← Jobs.backfillLane st
  let j3 ← Gmail.Backfill.latest? st
  let rb2 ← db.queryOne "SELECT * FROM messages WHERE gm_msgid = 'b2'"
  let rb3 ← db.queryOne "SELECT * FROM messages WHERE gm_msgid = 'b3'"
  check t "gmail: once the lease expires the step resumes at the page token and finishes" <|
    ran3 && (j3.map (·.text "state")) == some "done" && (j3.map (·.nat "seen")) == some 3 && (j3.map (·.nat "imported")) == some 3 &&
      (← g.count fun (_, u, _) => has u "/messages/b1?format=raw") == b1Fetches && (← Gmail.heldId? st "b3").isSome
  check t "gmail: backfilled mail is an import that expires from the backfill's start; Sent mail lands in Sent" <|
    ((rb2.bind (·.nat? "expires_at")).getD 0) > now2 && (rb3.map (·.text "folder")) == some "sent" &&
      (rb2.map (·.text "source")) == some "import" &&
      (rb2.bind (·.nat? "archived_at")).isSome
  -- invalid_grant pauses it
  let bf2 ← go (mk "POST" "/api/gmail/backfill" (body := "{\"keep_forever\":true}"))
  g.refreshFails.set true
  Gmail.OAuth.clearCache
  discard <| Jobs.backfillLane st
  let j4 ← Gmail.Backfill.active? st
  check t "gmail: a refused grant (invalid_grant) pauses the backfill and marks the connection" <|
    bf2.status == 200 && (j4.map (·.text "state")) == some "paused" && (j4.map (·.text "error_code")) == some "refresh_rejected" &&
      (((← conn).map (·.text "last_error")).getD "").startsWith "refresh_rejected:invalid_grant" &&
      (← Gmail.usable? st).isNone
  let gs ← go (mk "GET" "/api/gmail/backfill")
  check t "gmail: GET /api/gmail/backfill reports it" (gs.status == 200 && has gs.bodyText "\"error_code\":\"refresh_rejected\"")
  g.refreshFails.set false
  db.exec "UPDATE gmail_connection SET last_error = NULL"
  let rs ← go (mk "POST" "/api/gmail/backfill/resume")
  let cn ← go (mk "POST" "/api/gmail/backfill/cancel")
  check t "gmail: resume and cancel" <|
    rs.status == 200 && has rs.bodyText "\"state\":\"running\"" && cn.status == 200 && (← Gmail.Backfill.active? st).isNone

  -- the archive mirror
  let m1id := (r1.map (·.text "id")).getD ""
  let m3id := ((← db.queryOne "SELECT id FROM messages WHERE gm_msgid = 'm3'").map (·.text "id")).getD ""
  Gmail.kicked.set false
  discard <| Actions.apply st m1id "done"
  let q1 ← Gmail.Mirror.state? st m1id
  check t "gmail: marking a Gmail message done queues the mirror and kicks the job loop" (q1 == some "removing" && (← Gmail.kicked.get))
  let mods (id : String) := g.count fun (m, u, _) => m == "POST" && has u s!"/messages/{id}/modify"
  let mr ← Gmail.Mirror.step st
  check t "gmail: the mirror lane takes INBOX off it in Gmail" <|
    mr.archived == 1 && (← Gmail.Mirror.state? st m1id) == some "archived" && (← mods "m1") == 1 &&
      !(← g.labels "m1").contains "INBOX" &&
      (← g.count fun (_, u, b) => has u "/messages/m1/modify" && has b "\"removeLabelIds\":[\"INBOX\"]") == 1
  discard <| Actions.apply st m1id "inbox"
  check t "gmail: undo queues the restore" ((← Gmail.Mirror.state? st m1id) == some "restoring")
  let mr2 ← Gmail.Mirror.step st
  check t "gmail: the lane puts INBOX back" <|
    mr2.restored == 1 && (← Gmail.Mirror.state? st m1id) == some "restored" && (← g.labels "m1").contains "INBOX" &&
      (← g.count fun (_, u, b) => has u "/messages/m1/modify" && has b "\"addLabelIds\":[\"INBOX\"]") == 1
  -- not in Gmail's inbox: left alone
  g.msgs.modify fun ms => ms.map fun (i, raw, ls, a) => if i == "m3" then (i, raw, #["CATEGORY_SOCIAL"], a) else (i, raw, ls, a)
  discard <| Actions.apply st m3id "done"
  let mr3 ← Gmail.Mirror.step st
  check t "gmail: mail not in Gmail's inbox is skipped, never modified" <|
    mr3.skipped == 1 && (← mods "m3") == 0 &&
      ((← db.queryOne "SELECT reason FROM gmail_mirror WHERE message_id = ?" #[m3id]).map (·.text "reason")) == some "not_in_inbox"
  Gmail.setPref st Gmail.prefMirror false
  discard <| Actions.apply st m1id "done"
  check t "gmail: with the switch off nothing is queued" ((← Gmail.Mirror.state? st m1id) == some "restored")
  discard <| Actions.apply st m1id "inbox"
  Gmail.setPref st Gmail.prefMirror true

  -- spam
  discard <| Actions.apply st m1id "junk"
  let jg ← db.queryOne "SELECT junk_gmail FROM messages WHERE id = ?" #[m1id]
  check t "gmail: reporting junk queues a spam report and sets junk_gmail" <|
    (← Gmail.Spam.state? st m1id) == some "reporting" && (jg.map (·.bool "junk_gmail")) == some true
  let sr ← Gmail.Spam.step st
  check t "gmail: the spam lane marks it spam in Gmail and takes it out of the inbox" <|
    sr.reported == 1 && (← g.labels "m1").contains "SPAM" && !(← g.labels "m1").contains "INBOX" &&
      ((← db.queryOne "SELECT had_inbox FROM gmail_spam WHERE message_id = ?" #[m1id]).map (·.bool "had_inbox")) == some true
  discard <| Actions.apply st m1id "notjunk"
  let sr2 ← Gmail.Spam.step st
  check t "gmail: not junk takes SPAM off and puts INBOX back" <|
    sr2.restored == 1 && !(← g.labels "m1").contains "SPAM" && (← g.labels "m1").contains "INBOX" &&
      (← Gmail.Spam.state? st m1id) == some "restored"

  -- send-as
  let wBefore := (← wcalls.get).size
  let cid := Core.Ulid.encode (← Clock.nowMs) (ByteArray.mk #[1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  let sreq : Send.Request := {
    clientId := cid, from_ := "Alex@Gmail.com", subject := "Via Gmail", text := "hello", replyOf := some m1id
    to := [{ address := "bob@example.com" }], bcc := [{ address := "carol@example.com" }] }
  let so ← Send.send st .anon sreq
  let sendCall := (← g.calls.get).toList.find? fun (m, u, _) => m == "POST" && has u "/messages/send"
  let sentBody := (sendCall.map (·.2.2)).getD ""
  let sentJ := (Lean.Json.parse sentBody).toOption.getD .null
  let sentMime := (String.fromUTF8? (Core.Base64.decode ((sentJ.getObjValAs? String "raw").toOption.getD ""))).getD ""
  let srow ← match so with
    | .ok o => db.queryOne "SELECT * FROM messages WHERE id = ?" #[o.id]
    | .error _ => pure none
  check t "gmail: mail from the Gmail address is sent with users.messages.send, not the Worker" <|
    (match so with | .ok o => o.sendStatus == "sent" | .error _ => false) && sendCall.isSome && (← wcalls.get).size == wBefore
  check t "gmail: the Gmail send carries the Bcc and the original's Gmail thread" <|
    has sentMime "Bcc: carol@example.com" && has sentMime "Subject: Via Gmail" &&
      (sentJ.getObjValAs? String "threadId").toOption == some "t-m1"
  check t "gmail: the Sent copy gets the returned Gmail id" <|
    (srow.map (·.text "gm_msgid")) == some "gsent1" && (srow.map (·.text "folder")) == some "sent"
  check t "gmail: an alias Gmail verified is sendable, a pending one is not" <|
    (← Send.sendable st .anon "alias@fralex.art") && !(← Send.sendable st .anon "pending@fralex.art")

  -- watch and push
  db.exec "INSERT INTO settings(key, value, updated_at) VALUES ('gmail.pubsub_topic', 'projects/p/topics/t', 0)"
  let wr ← go (mk "POST" "/api/gmail/watch")
  check t "gmail: POST /api/gmail/watch calls users.watch with the topic and stores the expiry" <|
    wr.status == 200 && ((← conn).bind (·.nat? "watch_expiration")).isSome &&
      (← g.count fun (m, u, b) => m == "POST" && u == Gmail.apiBase ++ "/watch" && has b "projects/p/topics/t") == 1
  check t "gmail: an active watch wakes every 1800 s" ((← Jobs.nextWake st) == some 1800)
  check t "gmail: the daily renewal waits until two days before expiry" (!(← Gmail.Watch.renewIfDue st (← Clock.nowMs)))
  g.addMsg "m5" 5 #["INBOX"]
  g.history.set #["m5"]
  let sec := [("x-internal-secret", "isec")]
  let pushBad ← go (mk "POST" "/internal/gmail/push" (body := "{\"emailAddress\":\"alex@gmail.com\",\"historyId\":400}"))
  let pushOther ← go (mk "POST" "/internal/gmail/push" (body := "{\"emailAddress\":\"eve@gmail.com\",\"historyId\":400}") (extra := sec))
  let push ← go (mk "POST" "/internal/gmail/push" (body := "{\"emailAddress\":\"alex@gmail.com\",\"historyId\":400}") (extra := sec))
  check t "gmail: /internal/gmail/push needs the internal secret, ignores another mailbox, and runs the poll" <|
    pushBad.status == 401 && has pushOther.bodyText "\"polled\":false" && has push.bodyText "\"polled\":true" &&
      (← Gmail.heldId? st "m5").isSome && ((← conn).bind (·.nat? "last_push_at")).isSome
  let tk ← go (mk "POST" "/internal/tick" [("daily", "1")] (extra := sec))
  check t "gmail: POST /internal/tick runs the lanes (daily adds the renewal and pruning)" <|
    tk.status == 200 && has tk.bodyText "gmail.poll" && has tk.bodyText "gmail.mirror" && has tk.bodyText "gmail.watch" &&
      has tk.bodyText "prune"

  -- the page, and disconnecting
  let pg ← go (mk "GET" "/settings/gmail" (extra := [("accept", "text/html")]))
  check t "gmail: /settings/gmail shows the connection, the backfill and the switches" <|
    pg.status == 200 && has pg.bodyText "alex@gmail.com" && has pg.bodyText "/api/gmail/authorize" &&
      has pg.bodyText "mirror_archive" && has pg.bodyText "Backfill" && has pg.bodyText "Disconnect"
  let pf ← go (mk "POST" "/settings/gmail/prefs" (body := "report_spam=1&pubsub_topic=") (extra := [("accept", "text/html")]))
  check t "gmail: the switches form saves (an unticked box is off)" <|
    pf.status == 303 && !(← Gmail.prefOn st Gmail.prefMirror true) && (← Gmail.prefOn st Gmail.prefSpam false) &&
      (← Gmail.Watch.topic? st).isNone
  let del ← go (mk "DELETE" "/api/gmail")
  check t "gmail: DELETE /api/gmail stops the watch and forgets the connection" <|
    del.status == 200 && (← conn).isNone && (← Jobs.nextWake st) == none &&
      (← g.count fun (_, u, _) => u == Gmail.apiBase ++ "/stop") == 1

/-! ## M7: calendar, junk, blocks, routing, push, expiry -/

def crlf (lines : List String) : String := "\r\n".intercalate lines ++ "\r\n"

def m7Ics (method uid seq summary dtstart dtend : String) (extra : List String := []) : String :=
  crlf (["BEGIN:VCALENDAR", "VERSION:2.0", "PRODID:-//t//EN", s!"METHOD:{method}", "BEGIN:VEVENT", s!"UID:{uid}",
    s!"SEQUENCE:{seq}", "DTSTAMP:20261030T120000Z", s!"DTSTART:{dtstart}", s!"DTEND:{dtend}", s!"SUMMARY:{summary}",
    "LOCATION:14 Quay Street", "ORGANIZER;CN=Nadia:mailto:nadia@ferreira-arch.example",
    "ATTENDEE;PARTSTAT=NEEDS-ACTION;RSVP=TRUE:mailto:alex@fralex.art"] ++ extra ++ ["END:VEVENT", "END:VCALENDAR"])

def m7Invite (mid subject ical method : String) : String :=
  crlf ["From: Nadia <nadia@ferreira-arch.example>", "To: alex@fralex.art", s!"Subject: {subject}", s!"Message-ID: <{mid}>",
    "Date: Fri, 30 Oct 2026 12:00:00 +0000", "MIME-Version: 1.0", "Content-Type: multipart/alternative; boundary=\"b\"", "",
    "--b", "Content-Type: text/plain; charset=utf-8", "", subject, "",
    "--b", s!"Content-Type: text/calendar; method={method}; charset=utf-8", "", ical, "--b--"]

def m7Plain (fromHdr mid subject body : String) (extra : List String := []) : String :=
  crlf ([s!"From: {fromHdr}", "To: alex@fralex.art", s!"Subject: {subject}", s!"Message-ID: <{mid}>",
    "Date: Fri, 30 Oct 2026 12:00:00 +0000"] ++ extra ++ ["", body])

/-- A fake Cloudflare account for one zone (`fralex.art`, id `z1`): its catch-all, rules and
sending names change as the API is called, and every call is recorded. -/
structure FakeCf where
  calls : IO.Ref (Array (String × String × String))
  catchAll : IO.Ref String
  rules : IO.Ref (Array (String × String × String × String))
  preview : IO.Ref Bool

def FakeCf.new : IO FakeCf := do
  return { calls := ← IO.mkRef #[], catchAll := ← IO.mkRef "mailvault",
           rules := ← IO.mkRef #[("r1", "mv:old@fralex.art", "old@fralex.art", "x@gmail.com"),
                                 ("r2", "hand made", "keep@fralex.art", "y@gmail.com")],
           preview := ← IO.mkRef true }

def cfOk (j : Lean.Json) : Core.Http.Response :=
  { status := 200, headers := [], body := (Lean.Json.mkObj [("success", Lean.Json.bool true), ("result", j)]).compress.toUTF8 }

def FakeCf.api (f : FakeCf) : WebApi where
  request r := do
    let base := "https://cf.test/client/v4"
    let path := String.ofList (r.url.toList.drop base.length)
    let path0 := (path.splitOn "?").headD ""
    f.calls.modify (·.push (r.method, path, (String.fromUTF8? r.body).getD ""))
    let open_ := Lean.Json.mkObj
    let arr := Lean.Json.arr
    let str := Lean.Json.str
    match r.method, path0 with
    | "GET", "/zones" => return cfOk (arr #[open_ [("id", str "z1"), ("name", str "fralex.art")], open_ [("id", str "z9"), ("name", str "other.example")]])
    | "GET", "/accounts/acc/email/routing/addresses" =>
      return cfOk (arr #[open_ [("email", str "alex@gmail.com"), ("verified", str "2026-01-01T00:00:00Z")],
                         open_ [("email", str "nope@gmail.com"), ("verified", Lean.Json.null)]])
    | "GET", "/zones/z1/email/routing" => return cfOk (open_ [("enabled", Lean.Json.bool true), ("status", str "ready")])
    | "GET", "/zones/z1/email/routing/rules" =>
      let rs ← f.rules.get
      return cfOk (arr (rs.map fun (id, name, m, t) => open_ [("tag", str id), ("name", str name), ("enabled", Lean.Json.bool true),
        ("matchers", arr #[open_ [("type", str "literal"), ("field", str "to"), ("value", str m)]]),
        ("actions", arr #[open_ [("type", str "forward"), ("value", arr #[str t])]])]))
    | "GET", "/zones/z1/email/routing/rules/catch_all" =>
      return cfOk (open_ [("tag", str "ca"), ("name", str "catch-all"), ("enabled", Lean.Json.bool true),
        ("matchers", arr #[open_ [("type", str "all")]]), ("actions", arr #[open_ [("type", str "worker"), ("value", arr #[str (← f.catchAll.get)])]])])
    | "PUT", "/zones/z1/email/routing/rules/catch_all" =>
      let j := (Lean.Json.parse ((String.fromUTF8? r.body).getD "")).toOption.getD .null
      let w := ((j.getObjValAs? (Array Lean.Json) "actions").toOption.bind (·[0]?)).bind fun a =>
        ((a.getObjValAs? (Array String) "value").toOption.bind (·[0]?))
      f.catchAll.set (w.getD "?")
      return cfOk (open_ [])
    | "DELETE", p =>
      let id := (p.splitOn "/").getLast!
      f.rules.modify (·.filter (·.1 != id))
      return cfOk (open_ [])
    | "GET", "/zones/z1/email/sending/subdomains" =>
      return cfOk (arr #[open_ [("id", str "s1"), ("name", str "fralex.art"), ("enabled", Lean.Json.bool true),
        ("preview_enabled", Lean.Json.bool (← f.preview.get))]])
    | "PATCH", "/zones/z1/email/sending/subdomains/s1" => f.preview.set false; return cfOk (open_ [])
    | _, _ => return { status := 404, headers := [], body := .empty }

structure M7 where
  st : State
  clock : IO.Ref Nat
  calls : IO.Ref (Array (String × String × Lean.Json))
  webCalls : IO.Ref (Array (String × String × String))

def m7Req (m path : String) (hs : List (String × String) := []) (q : List (String × String) := []) (body : String := "") : Router.Req :=
  { method := m, path := (path.splitOn "/").filter (· ≠ ""), query := q, headers := hs, body := body.toUTF8 }
def m7Go (st' : State) (r : Router.Req) : IO Router.Resp := Router.dispatch st' (Routes.all st'.config) r
def m7Api (st : State) (m path : String) (body : String := "") (q : List (String × String) := []) : IO Router.Resp :=
  m7Go st (m7Req m path [("x-account-id", "dev"), ("content-type", "application/json")] q body)
def m7Form (st : State) (path body : String) : IO Router.Resp :=
  m7Go st (m7Req "POST" path [("x-account-id", "dev"), ("accept", "text/html"), ("content-type", "application/x-www-form-urlencoded")] [] body)
def m7Ing (st : State) (raw : String) (source := "route") : IO Ingest.IngestResult := do
  Ingest.ingest st { raw := raw.toUTF8, key := none, rcpt := some "alex@fralex.art", source, receivedAt := ← Clock.nowMs }
def m7Row (st : State) (id : String) : IO (Option Db.Row) := st.db.queryOne "SELECT * FROM messages WHERE id = ?" #[id]
def m7One (st : State) (sql : String) (args : Array Db.Value := #[]) : IO Nat := do
  return ((← st.db.queryOne sql args).map (·.nat "n")).getD 0
def m7Dead : String := String.ofList (List.replicate 64 'b')
def m7Live : String := String.ofList (List.replicate 64 'a')

def m7Setup : IO M7 := do
  let db ← Db.openMemory
  discard <| Schema.migrate db
  let clock ← IO.mkRef (← Clock.nowMs)
  let store ← Store.memoryWith clock.get
  -- the Worker: records calls; `/internal/push` says the second token is dead
  let calls ← IO.mkRef (#[] : Array (String × String × Lean.Json))
  let deadTok := m7Dead
  let worker : WorkerClient := { call := fun m p j => do
    calls.modify (·.push (m, p, j))
    if p == "/internal/push" then
      return some (Lean.Json.mkObj [("sent", Lean.Json.num 1), ("dead", Lean.Json.arr #[Lean.Json.str deadTok]), ("failed", Lean.Json.num 0)])
    return some (Lean.Json.mkObj [("ok", Lean.Json.bool true)]) }
  let webCalls ← IO.mkRef (#[] : Array (String × String × String))
  let web : WebApi := { request := fun r => do
    webCalls.modify (·.push (r.method, r.url, (String.fromUTF8? r.body).getD ""))
    return { status := 200, headers := [], body := .empty } }
  let st : State := { config := { devBypass := true }, db, store, worker, startedAt := 0, schemaVersion := 6, fts5 := true, web }
  return { st, clock, calls, webCalls }

def m7Dev (t : IO.Ref Tally) (m : M7) : IO Unit := do
  let st := m.st
  let db := st.db
  let store := st.store
  let clock := m.clock
  let calls := m.calls
  let webCalls := m.webCalls
  let dev := [("x-account-id", "dev")]
  let html := [("accept", "text/html")]
  let deadTok := m7Dead
  let liveTok := m7Live
  let _ := (db, store, clock, calls, webCalls, dev, html, deadTok, liveTok)
  -- the coordinator's dev additions, on the bare (unprovisioned) server
  let acct0 ← m7Api st "GET" "/api/account"
  check t "dev bypass: GET /api/account on a bare server is the synthetic dev account" <|
    acct0.status == 200 && jget acct0 "email" == "dev@localhost" && jget acct0 "default_from" == "dev@localhost" &&
      ((jsonOf acct0).getObjValAs? Bool "is_admin").toOption == some true
  let send0 ← m7Api st "POST" "/api/send" "{\"to\":[\"bob@example.com\"],\"subject\":\"hi\",\"text\":\"hello\",\"client_id\":\"01JDEVSEND000000000000000A\"}"
  let sentFrom ← db.queryOne "SELECT from_addr FROM messages WHERE folder = 'sent' ORDER BY received_at DESC LIMIT 1"
  check t "dev bypass: /api/send without from sends as dev@localhost" <|
    send0.status == 200 && (sentFrom.map (·.text "from_addr")) == some "dev@localhost"

  discard <| m7Api st "POST" "/internal/provision" "{\"email\":\"alex@example.test\",\"is_admin\":true,\"display_name\":\"Alex\"}"
  db.exec "UPDATE account SET default_from = 'alex@fralex.art' WHERE id = 1"
  let (_, tok) ← Auth.ApiTokens.mint st "app" ["read"]
  let stTok : State := { st with config := {} }
  let acctTok ← m7Go stTok (m7Req "GET" "/api/account" [("authorization", s!"Bearer {tok}"), ("x-account-id", "dev")])
  let acctNo ← m7Go stTok (m7Req "GET" "/api/account" [("x-account-id", "dev")])
  check t "GET /api/account accepts a read token" <|
    acctTok.status == 200 && jget acctTok "email" == "alex@example.test" && acctNo.status == 401

  -- Db.execCount
  let c1 ← db.execCount "UPDATE account SET display_name = 'Alex' WHERE id = 1"
  let c0 ← db.execCount "UPDATE account SET display_name = 'x' WHERE id = 2"
  check t "Db.execCount answers the rows its own statement changed" (c1 == 1 && c0 == 0)


def m7Calendar (t : IO.Ref Tally) (m : M7) : IO (String × String) := do
  let st := m.st
  let db := st.db
  let store := st.store
  let clock := m.clock
  let calls := m.calls
  let webCalls := m.webCalls
  let dev := [("x-account-id", "dev")]
  let html := [("accept", "text/html")]
  let deadTok := m7Dead
  let liveTok := m7Live
  let _ := (db, store, clock, calls, webCalls, dev, html, deadTok, liveTok)
  -- calendar: an invitation, its update, a stale copy, its cancellation
  let ev1 := m7Ics "REQUEST" "rev-1@ferreira-arch.example" "0" "Design review" "20261105T150000Z" "20261105T160000Z"
  let r1 ← m7Ing st (m7Invite "inv-1@x.test" "Invitation: Design review" ev1 "REQUEST")
  let rows1 ← db.query "SELECT * FROM events"
  let m1 ← m7Row st r1.id
  check t "calendar: an invite at ingest is one events row keyed by row_key, with an expiry, and marks has_invite" <|
    rows1.size == 1 && rows1[0]!.text "row_key" == Core.Ical.rowKey "rev-1@ferreira-arch.example" none &&
      rows1[0]!.text "source" == "mail" && rows1[0]!.text "message_id" == r1.id && (rows1[0]!.nat? "expires_at").isSome &&
      has (rows1[0]!.text "data") "\"summary\":\"Design review\"" && (m1.map (·.bool "has_invite")) == some true
  let evId := rows1[0]!.text "id"
  let ev2 := m7Ics "REQUEST" "rev-1@ferreira-arch.example" "1" "Design review (moved)" "20261106T150000Z" "20261106T160000Z"
  let raw2 := (m7Invite "inv-2@x.test" "Updated: Design review" ev2 "REQUEST").toUTF8
  store.put "raw/dev/INV2" raw2 []
  let r2 ← Ingest.ingest st { raw := raw2, key := some "raw/dev/INV2", rcpt := some "alex@fralex.art", source := "route", receivedAt := ← Clock.nowMs }
  let ev0 := m7Ics "REQUEST" "rev-1@ferreira-arch.example" "0" "Design review (stale)" "20261105T150000Z" "20261105T160000Z"
  discard <| m7Ing st (m7Invite "inv-0@x.test" "Old: Design review" ev0 "REQUEST")
  let rows2 ← db.query "SELECT * FROM events"
  check t "calendar: a higher SEQUENCE updates the same row; a stale one is ignored" <|
    rows2.size == 1 && rows2[0]!.text "id" == evId && rows2[0]!.nat "sequence" == 1 &&
      has (rows2[0]!.text "data") "Design review (moved)" && rows2[0]!.text "message_id" == r2.id
  let evc := m7Ics "CANCEL" "rev-1@ferreira-arch.example" "2" "Design review (moved)" "20261106T150000Z" "20261106T160000Z"
  discard <| m7Ing st (m7Invite "inv-c@x.test" "Cancelled: Design review" evc "CANCEL")
  let rows3 ← db.query "SELECT * FROM events"
  check t "calendar: a CANCEL marks the row cancelled and keeps its record" <|
    rows3.size == 1 && rows3[0]!.bool "cancelled" && rows3[0]!.nat "sequence" == 2 && has (rows3[0]!.text "data") "Design review (moved)"
  -- a REPLY is not an event
  discard <| m7Ing st (m7Invite "inv-r@x.test" "Accepted" (m7Ics "REPLY" "reply-1@x" "0" "Reply" "20261107T150000Z" "20261107T160000Z") "REPLY")
  check t "calendar: an iTIP REPLY makes no event" ((← m7One st "SELECT count(*) AS n FROM events") == 1)

  -- a weekly series with one moved instance
  let weekly := crlf ["BEGIN:VCALENDAR", "VERSION:2.0", "METHOD:REQUEST",
    "BEGIN:VEVENT", "UID:weekly-1@x.test", "SEQUENCE:0", "DTSTAMP:20261030T120000Z", "DTSTART:20261102T090000Z",
    "DTEND:20261102T100000Z", "RRULE:FREQ=WEEKLY;COUNT=4", "SUMMARY:Standup",
    "ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:alex@fralex.art", "END:VEVENT",
    "BEGIN:VEVENT", "UID:weekly-1@x.test", "SEQUENCE:0", "DTSTAMP:20261030T120000Z", "RECURRENCE-ID:20261109T090000Z",
    "DTSTART:20261109T100000Z", "DTEND:20261109T110000Z", "SUMMARY:Standup (late)", "END:VEVENT", "END:VCALENDAR"]
  let rw ← m7Ing st (m7Invite "weekly@x.test" "Standup" weekly "REQUEST")
  let evs ← m7Api st "GET" "/api/events" "" [("from", "2026-11-01"), ("to", "2026-12-01")]
  let occs := ((jsonOf evs).getObjValAs? (Array Lean.Json) "occurrences").toOption.getD #[]
  let ostr (o : Lean.Json) (k : String) := (o.getObjValAs? String k).toOption.getD ""
  let weeklyOccs := occs.filter (ostr · "uid" == "weekly-1@x.test")
  let moved := weeklyOccs.filter fun o => (o.getObjValAs? Bool "is_override").toOption == some true
  check t "GET /api/events expands a weekly COUNT=4 series in the window, with its moved instance" <|
    evs.status == 200 && weeklyOccs.size == 4 && moved.size == 1 &&
      (moved[0]!.getObjValAs? Int "start").toOption == some (Core.RRule.zonedToUtc "2026-11-09T10:00:00" (some "UTC")) &&
      ostr moved[0]! "summary" == "Standup (late)"
  let decOnly ← m7Api st "GET" "/api/events" "" [("from", "2026-12-01"), ("to", "2026-12-31")]
  check t "GET /api/events: nothing of a finished series outside the window" <|
    !has decOnly.bodyText "weekly-1@x.test"
  check t "GET /api/events: a cancelled event shows as cancelled" <|
    (occs.find? (ostr · "uid" == "rev-1@ferreira-arch.example")).any fun o => (o.getObjValAs? Bool "cancelled").toOption == some true
  -- manual events and the override
  let man ← m7Api st "POST" "/api/events" (Lean.Json.mkObj [("summary", Lean.Json.str "Dentist"),
    ("start", Lean.toJson (Core.RRule.zonedToUtc "2026-11-12T08:30:00" (some "UTC"))),
    ("end", Lean.toJson (Core.RRule.zonedToUtc "2026-11-12T09:00:00" (some "UTC")))]).compress
  let manId := jget man "id"
  let bad ← m7Api st "POST" "/api/events" "{\"start\":\"2026-11-12\"}"
  let put ← m7Api st "PUT" s!"/api/events/{manId}" "{\"summary\":\"Dentist (new time)\",\"start\":\"2026-11-13T08:30:00\",\"tz\":\"UTC\"}"
  let gotMan ← m7Api st "GET" s!"/api/events/{manId}"
  check t "POST /api/events makes a manual event; PUT replaces it with a higher SEQUENCE; a title is required" <|
    man.status == 201 && bad.status == 400 && put.status == 200 && has gotMan.bodyText "Dentist (new time)" &&
      has gotMan.bodyText "\"sequence\":1" && has gotMan.bodyText "\"source\":\"manual\""
  let wkMaster ← db.queryOne "SELECT id FROM events WHERE uid = 'weekly-1@x.test' AND row_key = ?"
    #[Core.Ical.rowKey "weekly-1@x.test" none]
  let wkId := (wkMaster.map (·.text "id")).getD ""
  let pat ← m7Api st "PATCH" s!"/api/events/{wkId}" "{\"partstat\":\"accepted\",\"notes\":\"bring laptop\"}"
  let evs2 ← m7Api st "GET" "/api/events" "" [("from", "2026-11-01"), ("to", "2026-12-01")]
  let occs2 := ((jsonOf evs2).getObjValAs? (Array Lean.Json) "occurrences").toOption.getD #[]
  let masterOccs := occs2.filter fun o => ostr o "event_id" == wkId
  check t "PATCH /api/events/:id keeps the RSVP and notes as an override, merged into the occurrences" <|
    pat.status == 200 && masterOccs.size == 3 && masterOccs.all (ostr · "partstat" == "ACCEPTED") &&
      masterOccs.all (ostr · "notes" == "bring laptop") &&
      ((← db.queryOne "SELECT partstat FROM event_overrides WHERE event_id = ?" #[wkId]).map (·.text "partstat")) == some "ACCEPTED"
  let rs ← m7Form st s!"/calendar/{wkId}/rsvp" "partstat=DECLINED&back=%2Fcalendar"
  check t "the RSVP form updates the override and goes back" <|
    rs.status == 303 && (rs.headers.lookup "Location") == some "/calendar?saved=rsvp" &&
      ((← db.queryOne "SELECT partstat FROM event_overrides WHERE event_id = ?" #[wkId]).map (·.text "partstat")) == some "DECLINED"
  let hide ← m7Api st "PATCH" s!"/api/events/{manId}" "{\"hidden\":true}"
  let evs3 ← m7Api st "GET" "/api/events" "" [("from", "2026-11-01"), ("to", "2026-12-01")]
  check t "a hidden event leaves the occurrences unless asked for" <|
    hide.status == 200 && !has evs3.bodyText manId &&
      has (← m7Api st "GET" "/api/events" "" [("from", "2026-11-01"), ("to", "2026-12-01"), ("hidden", "1")]).bodyText manId
  let batch ← m7Api st "POST" "/api/events/batch" (Lean.Json.mkObj [("events", Lean.Json.arr #[Lean.Json.mkObj [
    ("record", (Lean.Json.parse (rows3[0]!.text "data")).toOption.getD .null)]])]).compress
  check t "POST /api/events/batch keeps the sequence guard (an older copy is ignored)" <|
    batch.status == 200 && jnat batch "ignored" == 1
  let del ← m7Api st "DELETE" s!"/api/events/{manId}"
  check t "DELETE /api/events/:id soft-deletes" <|
    del.status == 200 && (← m7One st "SELECT count(*) AS n FROM events WHERE id = ? AND deleted = 1" #[manId]) == 1
  -- pages
  let mp ← m7Go st (m7Req "GET" s!"/m/{r2.id}" (dev ++ html))
  check t "the message page shows the invite block: summary, when, where, organizer, RSVP" <|
    mp.status == 200 && has mp.bodyText "class=\"invite card\"" && has mp.bodyText "Cancelled: Design review (moved)" &&
      has mp.bodyText "14 Quay Street" && has mp.bodyText "Nadia &lt;nadia@ferreira-arch.example&gt;" &&
      has mp.bodyText "Organizer"
  let mpw ← m7Go st (m7Req "GET" s!"/m/{rw.id}" (dev ++ html))
  check t "an invite with RSVP buttons posts to the event's rsvp form" <|
    has mpw.bodyText s!"action=\"/calendar/{wkId}/rsvp\"" && has mpw.bodyText "value=\"ACCEPTED\""
  let cal ← m7Go st (m7Req "GET" "/calendar" (dev ++ html) [("month", "2026-11")])
  check t "/calendar?month= shows the month grid with the occurrences, linking to each event" <|
    cal.status == 200 && pageHeadersOk cal && has cal.bodyText "class=\"month\"" && has cal.bodyText "Nov 2026" &&
      has cal.bodyText "Standup" && has cal.bodyText s!"href=\"/calendar/{wkId}\"" && has cal.bodyText "Next 14 days" &&
      has cal.bodyText "month=2026-12"
  let evp ← m7Go st (m7Req "GET" s!"/calendar/{wkId}" (dev ++ html))
  check t "/calendar/:id shows the event, its message and the answer" <|
    evp.status == 200 && has evp.bodyText "Standup" && has evp.bodyText s!"href=\"/m/{rw.id}\"" && has evp.bodyText "Declined" &&
      has evp.bodyText "bring laptop"

  return (evId, r1.id)

def m7Junk (t : IO.Ref Tally) (m : M7) (r1id : String) : IO Unit := do
  let st := m.st
  let db := st.db
  let store := st.store
  let clock := m.clock
  let calls := m.calls
  let webCalls := m.webCalls
  let dev := [("x-account-id", "dev")]
  let html := [("accept", "text/html")]
  let deadTok := m7Dead
  let liveTok := m7Live
  let _ := (db, store, clock, calls, webCalls, dev, html, deadTok, liveTok)
  -- junk: the verdict at ingest, a phishing message files itself
  let phish := crlf ["From: \"PayPal Service\" <service@paypa1-security.top>", "To: alex@fralex.art",
    "Subject: Verify your account now", "Message-ID: <phish-1@x.test>", "Date: Fri, 30 Oct 2026 12:00:00 +0000",
    "Authentication-Results: mx.example; dmarc=fail header.from=paypa1-security.top; spf=fail smtp.mailfrom=paypa1-security.top",
    "MIME-Version: 1.0", "Content-Type: text/html; charset=utf-8", "",
    "<p>Please verify your account within 24 hours.</p><p><a href=\"http://192.168.10.5/login\">https://www.paypal.com/signin</a></p>"]
  let rp ← m7Ing st phish
  let mph ← m7Row st rp.id
  check t "junk: a phishing message gets its verdict stored and lands in Junk as phishing" <|
    match mph with
    | some r => r.text "junk_kind" == "phishing" && (r.nat? "junk_at").isSome && has (r.text "junk_verdict") "\"verdict\":\"phishing\"" &&
        has (r.text "junk_verdict") "dmarc_fail"
    | none => false
  let inboxNow ← m7Api st "GET" "/api/msgs"
  let junkNow ← m7Api st "GET" "/api/msgs" "" [("filter", "junk")]
  check t "junk: the phishing message is out of the inbox and in the Junk list" <|
    !has inboxNow.bodyText rp.id && has junkNow.bodyText rp.id
  let clean ← m7Row st r1id
  check t "junk: a clean message keeps its verdict and stays in the inbox" <|
    (clean.map fun r => has (r.text "junk_verdict") "\"verdict\":\"clean\"" && (r.nat? "junk_at").isNone) == some true
  let imp ← Ingest.ingest st { raw := (phish.replace "phish-1" "phish-imp").toUTF8, key := none, rcpt := none, source := "import", receivedAt := 1 }
  check t "junk: an import is checked but not re-filed (the old vault already decided)" <|
    ((← m7Row st imp.id).map fun r => (r.nat? "junk_at").isNone && has (r.text "junk_verdict") "phishing") == some true
  let mpp ← m7Go st (m7Req "GET" s!"/m/{rp.id}" (dev ++ html))
  check t "the message page shows the verdict's reasons in words and the junk row" <|
    has mpp.bodyText "Looks like phishing" && has mpp.bodyText "DMARC failed" && has mpp.bodyText s!"action=\"/m/{rp.id}/notjunk\"" &&
      has mpp.bodyText "Block sender"

  -- blocks
  let blk ← m7Api st "POST" "/api/blocks" "{\"scope\":\"address\",\"value\":\"Spammer@Bulk.example\"}"
  let blkOwn ← m7Api st "POST" "/api/blocks" "{\"scope\":\"domain\",\"value\":\"fralex.art\"}"
  check t "POST /api/blocks keeps the one spelling; one of the vault's own domains is refused" <|
    blk.status == 201 && jget blk "value" == "spammer@bulk.example" && blkOwn.status == 400
  let rb ← m7Ing st (m7Plain "Spam Co <spammer@bulk.example>" "spam-2@x.test" "Deals" "Cheap")
  check t "a blocked sender's next mail is stored as junk_kind 'blocked' and counts a hit" <|
    (match rb with | .blocked _ => true | _ => false) &&
      ((← m7Row st rb.id).map (·.text "junk_kind")) == some "blocked" &&
      (← m7One st "SELECT hits AS n FROM sender_blocks WHERE value = 'spammer@bulk.example'") == 1
  let nj ← m7Api st "POST" s!"/api/msg/{rb.id}/not-junk"
  check t "not-junk takes it out of Junk, back to the inbox, and lifts the sender's block" <|
    nj.status == 200 && ((← m7Row st rb.id).map fun r => (r.nat? "junk_at").isNone && (r.nat? "archived_at").isNone) == some true &&
      (← m7One st "SELECT count(*) AS n FROM sender_blocks WHERE value = 'spammer@bulk.example'") == 0
  let rd ← m7Ing st (m7Plain "x@spamdomain.example" "sd-1@x.test" "Hello" "Hi")
  let rep ← m7Api st "POST" s!"/api/msg/{rd.id}/report" "{\"kind\":\"spam\",\"block\":\"domain\"}"
  let rd2 ← m7Ing st (m7Plain "y@mail.spamdomain.example" "sd-2@x.test" "Hello again" "Hi")
  check t "report {kind, block: domain} files it in Junk and blocks the domain, subdomains included" <|
    rep.status == 200 && jget rep "junk_kind" == "spam" && ((← m7Row st rd.id).map (·.nat? "archived_at" |>.isSome)) == some true &&
      (match rd2 with | .blocked _ => true | _ => false)
  let badKind ← m7Api st "POST" s!"/api/msg/{rd.id}/report" "{\"kind\":\"ham\"}"
  check t "report refuses an unknown kind" (badKind.status == 400)
  let bl ← m7Api st "GET" "/api/blocks"
  let blId := (((jsonOf bl).getObjValAs? (Array Lean.Json) "blocks").toOption.bind (·[0]?)).bind (fun b => (b.getObjValAs? String "id").toOption) |>.getD ""
  let blDel ← m7Api st "DELETE" s!"/api/blocks/{blId}"
  check t "GET /api/blocks lists them; DELETE /api/blocks/:id lifts one" <|
    bl.status == 200 && has bl.bodyText "spamdomain.example" && blDel.status == 200 &&
      (← m7One st "SELECT count(*) AS n FROM sender_blocks") == 0
  let bp ← m7Go st (m7Req "GET" "/settings/blocks" (dev ++ html))
  let bpost ← m7Form st "/settings/blocks" "scope=address&value=pest%40nuisance.example"
  let bp2 ← m7Go st (m7Req "GET" "/settings/blocks" (dev ++ html))
  check t "/settings/blocks lists the blocks and adds one from its form" <|
    bp.status == 200 && bpost.status == 303 && has bp2.bodyText "pest@nuisance.example" && has bp2.bodyText "Unblock"
  let pageBlock ← m7Form st s!"/m/{r1id}/block" ""
  check t "the message page's Block sender blocks the sender and files the message" <|
    pageBlock.status == 303 && (← m7One st "SELECT count(*) AS n FROM sender_blocks WHERE value = 'nadia@ferreira-arch.example'") == 1 &&
      ((← m7Row st r1id).map (·.text "junk_kind")) == some "spam"
  discard <| m7Api st "POST" s!"/api/msg/{r1id}/not-junk"

  -- List-Unsubscribe
  let listMsg (mid : String) (post : Bool) := m7Plain "News <news@news.example.com>" mid "Weekly news" "Read on"
    (["List-Id: <weekly.news.example.com>", "List-Unsubscribe: <mailto:unsub@news.example.com?subject=stop>, <https://news.example.com/u/123>"] ++
      (if post then ["List-Unsubscribe-Post: List-Unsubscribe=One-Click"] else []))
  let raw1 := (listMsg "news-1@x.test" true).toUTF8
  store.put "raw/dev/NEWS1" raw1 []
  let rn ← Ingest.ingest st { raw := raw1, key := some "raw/dev/NEWS1", rcpt := none, source := "route", receivedAt := ← Clock.nowMs }
  let un ← m7Api st "POST" s!"/api/msg/{rn.id}/unsubscribe"
  let posted ← webCalls.get
  check t "unsubscribe: a one-click list gets the RFC 8058 POST" <|
    un.status == 200 && jget un "status" == "unsubscribed" &&
      posted.any fun (m, u, b) => m == "POST" && u == "https://news.example.com/u/123" && b == "List-Unsubscribe=One-Click"
  let raw2 := (listMsg "news-2@x.test" false).toUTF8
  store.put "raw/dev/NEWS2" raw2 []
  let rn2 ← Ingest.ingest st { raw := raw2, key := some "raw/dev/NEWS2", rcpt := none, source := "route", receivedAt := ← Clock.nowMs }
  let un2 ← m7Api st "POST" s!"/api/msg/{rn2.id}/unsubscribe"
  check t "unsubscribe: without one-click the targets come back for the page to show" <|
    un2.status == 200 && jget un2 "status" == "manual" && jget un2 "url" == "https://news.example.com/u/123" &&
      jget un2 "mailto" == "mailto:unsub@news.example.com?subject=stop" && (← webCalls.get).size == posted.size
  check t "unsubscribe: a private address is never POSTed to" <|
    !Server.Junk.publicHost "127.0.0.1" && !Server.Junk.publicHost "localhost" && Server.Junk.publicHost "news.example.com"


def m7Routing (t : IO.Ref Tally) (m : M7) : IO Unit := do
  let st := m.st
  let db := st.db
  let store := st.store
  let clock := m.clock
  let calls := m.calls
  let webCalls := m.webCalls
  let dev := [("x-account-id", "dev")]
  let html := [("accept", "text/html")]
  let deadTok := m7Dead
  let liveTok := m7Live
  let _ := (db, store, clock, calls, webCalls, dev, html, deadTok, liveTok)
  -- routing: CRUD projects to the Worker with the *@domain row
  calls.set #[]
  let d1 ← m7Api st "POST" "/api/config/domains" "{\"name\":\"fralex.art\",\"default_mail\":\"vault\",\"sending\":[\"fralex.art\"]}"
  let projections := (← calls.get).filter (·.2.1 == "/internal/routes")
  let lastRoutes (cs : Array (String × String × Lean.Json)) : Array Lean.Json :=
    ((cs.filter (·.2.1 == "/internal/routes")).back?.bind fun (_, _, j) => (j.getObjValAs? (Array Lean.Json) "routes").toOption).getD #[]
  let rows0 := lastRoutes (← calls.get)
  let routeOf (rs : Array Lean.Json) (a : String) := rs.find? fun r => (r.getObjValAs? String "address").toOption == some a
  check t "routing: adding a domain projects PUT /internal/routes {account_id, routes, replace} with its *@domain row" <|
    d1.status == 201 && projections.size == 1 && projections[0]!.1 == "PUT" &&
      (projections[0]!.2.2.getObjValAs? String "account_id").toOption == some "dev" &&
      (projections[0]!.2.2.getObjValAs? Bool "replace").toOption == some true &&
      ((routeOf rows0 "*@fralex.art").map fun r => (r.getObjValAs? String "mail").toOption == some "vault" &&
        (r.getObjValAs? Bool "can_send").toOption == some true) == some true
  let a1 ← m7Api st "PUT" "/api/config/addresses/shop@fralex.art" "{\"mail\":\"forward\",\"forward_to\":[\"alex@gmail.com\"]}"
  let aBad ← m7Api st "PUT" "/api/config/addresses/bare@fralex.art" "{\"mail\":\"forward\"}"
  let aForeign ← m7Api st "PUT" "/api/config/addresses/x@notmine.example" "{\"mail\":\"vault\"}"
  let rows1r := lastRoutes (← calls.get)
  check t "routing: an address row is projected; a forward with nowhere to go, or an unknown domain, is refused" <|
    a1.status == 200 && aBad.status == 400 && aForeign.status == 400 &&
      ((routeOf rows1r "shop@fralex.art").map fun r => (r.getObjValAs? String "mail").toOption == some "forward" &&
        (r.getObjValAs? (Array String) "forward_to").toOption == some #["alex@gmail.com"] &&
        (r.getObjValAs? Bool "can_send").toOption == some false) == some true
  let pd ← m7Api st "PATCH" "/api/config/domains/fralex.art" "{\"default_mail\":\"drop\"}"
  check t "routing: changing a domain's default changes its *@domain row" <|
    pd.status == 200 && ((routeOf (lastRoutes (← calls.get)) "*@fralex.art").bind fun r => (r.getObjValAs? String "mail").toOption) == some "drop"
  discard <| m7Api st "PATCH" "/api/config/domains/fralex.art" "{\"default_mail\":\"vault\"}"
  let cfgGet ← m7Api st "GET" "/api/config"
  let sendableGet ← m7Api st "GET" "/api/sendable"
  check t "GET /api/config and /api/sendable read the routing" <|
    cfgGet.status == 200 && has cfgGet.bodyText "shop@fralex.art" && has cfgGet.bodyText "\"configured\":false" &&
      sendableGet.status == 200 && has sendableGet.bodyText "alex@fralex.art" && !has sendableGet.bodyText "shop@fralex.art"
  let planNoTok ← m7Api st "GET" "/api/config/plan"
  let applyNoTok ← m7Api st "POST" "/api/config/apply" "{}"
  check t "routing: without CF_API_TOKEN the plan is shown (assumed) and cannot be applied" <|
    planNoTok.status == 200 && has planNoTok.bodyText "\"assumed\":true" && has planNoTok.bodyText "\"can_apply\":false" &&
      has planNoTok.bodyText "catch_all.set" && applyNoTok.status == 503
  let rpNo ← m7Go st (m7Req "GET" "/settings/routing" (dev ++ html))
  check t "/settings/routing lists domains and addresses and says it cannot apply without the token" <|
    rpNo.status == 200 && has rpNo.bodyText "fralex.art" && has rpNo.bodyText "shop@fralex.art" &&
      has rpNo.bodyText "CF_API_TOKEN and CF_ACCOUNT_ID are not set" && !has rpNo.bodyText "action=\"/settings/routing/apply\""
  -- with a token and a fake Cloudflare
  let cf ← FakeCf.new
  let stCf : State := { st with config := { devBypass := true, cfApiToken := "tok", cfAccountId := "acc", cfApiBase := "https://cf.test/client/v4" }, cloudflare := cf.api }
  let apiCf (m path : String) (body : String := "") : IO Router.Resp := m7Go stCf (m7Req m path (dev ++ [("content-type", "application/json")]) [] body)
  discard <| apiCf "PATCH" "/api/config/domains/fralex.art" "{\"default_forward_to\":[\"nope@gmail.com\"],\"default_mail\":\"vault+forward\"}"
  let pl ← apiCf "GET" "/api/config/plan"
  let pj := jsonOf pl
  let ops := (pj.getObjValAs? (Array Lean.Json) "ops").toOption.getD #[]
  let kinds := ops.map fun o => (o.getObjValAs? String "kind").toOption.getD ""
  check t "routing: the plan diffs against Cloudflare: catch-all to this Worker, the vault's old rule deleted, preview off" <|
    pl.status == 200 && kinds == #["catch_all.set", "rule.delete", "preview.off"] &&
      has pl.bodyText "\"can_apply\":true" && has pl.bodyText "\"assumed\":false" &&
      has pl.bodyText "hand made" && has pl.bodyText "unverified_destination" && has pl.bodyText "nope@gmail.com"
  let stale ← apiCf "POST" "/api/config/apply" "{\"plan_id\":\"0000\"}"
  check t "routing: apply refuses a plan that is not the one Cloudflare would now get" (stale.status == 409)
  cf.calls.set #[]
  let ap ← apiCf "POST" "/api/config/apply" (Lean.Json.mkObj [("plan_id", (pj.getObjVal? "id").toOption.getD .null)]).compress
  let writes := (← cf.calls.get).filter (·.1 != "GET")
  check t "routing: apply makes the changes through the Cloudflare client, and the next plan is empty" <|
    ap.status == 200 && writes.size == 3 && (← cf.catchAll.get) == "leanmail" && !(← cf.preview.get) &&
      (← cf.rules.get).size == 1 && writes.any (fun (m, p, _) => m == "DELETE" && p == "/zones/z1/email/routing/rules/r1") &&
      ((((jsonOf ap).getObjVal? "plan").toOption.bind fun p => (p.getObjValAs? (Array Lean.Json) "ops").toOption).map (·.size)) == some 0
  let rpCf ← m7Go stCf (m7Req "GET" "/settings/routing" (dev ++ html) [("plan", "1")])
  check t "/settings/routing?plan=1 shows the plan and the Apply form when the token is set" <|
    rpCf.status == 200 && has rpCf.bodyText "action=\"/settings/routing/apply\"" && has rpCf.bodyText "keep@fralex.art: forwarded to y@gmail.com"
  let fd ← m7Form st "/settings/routing/domains" "name=second.example&default_mail=vault&default_forward_to=&sending=&manage=1"
  let fdel ← m7Form st "/settings/routing/addresses/shop@fralex.art/delete" ""
  check t "/settings/routing forms add a domain and delete an address, projecting each time" <|
    fd.status == 303 && fdel.status == 303 && (routeOf (lastRoutes (← calls.get)) "*@second.example").isSome &&
      (routeOf (lastRoutes (← calls.get)) "shop@fralex.art").isNone
  let dd ← m7Api st "DELETE" "/api/config/domains/second.example"
  check t "routing: deleting a domain drops its row from the projection" <|
    dd.status == 200 && (routeOf (lastRoutes (← calls.get)) "*@second.example").isNone


def m7Push (t : IO.Ref Tally) (m : M7) (evId : String) : IO Unit := do
  let st := m.st
  let db := st.db
  let store := st.store
  let clock := m.clock
  let calls := m.calls
  let webCalls := m.webCalls
  let dev := [("x-account-id", "dev")]
  let html := [("accept", "text/html")]
  let deadTok := m7Dead
  let liveTok := m7Live
  let _ := (db, store, clock, calls, webCalls, dev, html, deadTok, liveTok)
  -- push
  let pt1 ← m7Api st "PUT" "/api/push/token" (Lean.Json.mkObj [("token", Lean.Json.str liveTok), ("env", Lean.Json.str "prod")]).compress
  let pt2 ← m7Api st "PUT" "/api/push/token" (Lean.Json.mkObj [("token", Lean.Json.str deadTok), ("env", Lean.Json.str "sandbox")]).compress
  let ptBad ← m7Api st "PUT" "/api/push/token" "{\"token\":\"xyz\"}"
  check t "PUT /api/push/token registers device tokens; a malformed one is refused" <|
    pt1.status == 200 && pt2.status == 200 && ptBad.status == 400 && (← m7One st "SELECT count(*) AS n FROM push_tokens") == 2
  calls.set #[]
  let rpush ← m7Ing st (m7Plain "Bea <bea@friends.example>" "push-1@x.test" "Lunch?" "Tomorrow at one")
  let pushes := (← calls.get).filter (·.2.1 == "/internal/push")
  check t "new mail posts {tokens, payload:{id, from, subject}} to /internal/push; the dead token is pruned" <|
    pushes.size == 1 &&
      ((pushes[0]!.2.2.getObjValAs? (Array Lean.Json) "tokens").toOption.map (·.size)) == some 2 &&
      ((pushes[0]!.2.2.getObjVal? "payload").toOption.map fun p =>
        (p.getObjValAs? String "id").toOption == some rpush.id && (p.getObjValAs? String "from").toOption == some "Bea" &&
        (p.getObjValAs? String "subject").toOption == some "Lunch?") == some true &&
      (← m7One st "SELECT count(*) AS n FROM push_tokens") == 1 &&
      (← m7One st "SELECT count(*) AS n FROM push_tokens WHERE token = ?" #[liveTok]) == 1
  calls.set #[]
  discard <| Ingest.ingest st { raw := (m7Plain "Bea <bea@friends.example>" "push-2@x.test" "Old" "x").toUTF8, key := none, rcpt := none, source := "import", receivedAt := 1 }
  check t "an import, and junk, never push" <|
    ((← calls.get).filter (·.2.1 == "/internal/push")).isEmpty
  let ptDel ← m7Api st "DELETE" "/api/push/token" (Lean.Json.mkObj [("token", Lean.Json.str liveTok)]).compress
  check t "DELETE /api/push/token forgets it" (ptDel.status == 200 && (← m7One st "SELECT count(*) AS n FROM push_tokens") == 0)

  -- jobs: expire, purge, orphan and spool GC, stuck sends on a seeded database
  let now ← Clock.nowMs
  let rawE := (m7Plain "Old <old@archive.example>" "exp-1@x.test" "matterhorn expiring" "old words").toUTF8
  store.put "raw/dev/EXP1" rawE []
  let re ← Ingest.ingest st { raw := rawE, key := some "raw/dev/EXP1", rcpt := none, source := "import", receivedAt := now - 10 * 86400000, expiresAt := some (now - 1000) }
  let rsoon ← Ingest.ingest st { raw := (m7Plain "Soon <soon@x.example>" "soon-1@x.test" "Soon gone" "x").toUTF8, key := none, rcpt := none, source := "import", receivedAt := now, expiresAt := some (now + 3 * 86400000) }
  db.exec "UPDATE events SET expires_at = ?, kept = 0 WHERE id = ?" #[(now - 1000 : Nat), evId]
  let (xm, xe) ← Expiry.expireDue st now
  let rowE ← m7Row st re.id
  check t "expire: due mail is soft-deleted with a 7-day purge_after, due events too; the rest stay" <|
    xm ≥ 1 && xe == 1 && (rowE.map fun r => r.bool "deleted" && r.nat "purge_after" == now + 7 * 86400000 && (r.nat? "deleted_at").isSome) == some true &&
      ((← m7Row st rsoon.id).map (·.bool "deleted")) == some false &&
      (← m7One st "SELECT deleted AS n FROM events WHERE id = ?" #[evId]) == 1
  let exg ← m7Api st "GET" "/api/expiring"
  let expPage ← m7Go st (m7Req "GET" "/expiring" (dev ++ html))
  check t "GET /api/expiring and /expiring list what expires within 7 days, with Keep forever" <|
    exg.status == 200 && has exg.bodyText rsoon.id && !has exg.bodyText re.id &&
      expPage.status == 200 && has expPage.bodyText "Soon gone" && has expPage.bodyText "Keep forever"
  let keep ← m7Api st "POST" "/api/msg/expiry" (Lean.Json.mkObj [("ids", Lean.Json.arr #[Lean.Json.str rsoon.id]), ("expires_at", Lean.Json.str "never")]).compress
  check t "POST /api/msg/expiry {ids, never} keeps forever" <|
    keep.status == 200 && jnat keep "updated" == 1 && ((← m7Row st rsoon.id).map (·.nat? "expires_at" |>.isNone)) == some true
  check t "purge: nothing before the undo window" ((← Expiry.purgeDue st now) == 0 && (← store.get "raw/dev/EXP1").isSome)
  let pn ← Expiry.purgeDue st (now + 8 * 86400000)
  check t "purge: after 7 days the raw object, text and FTS entry go; the row keeps purged_at" <|
    pn ≥ 1 && (← store.get "raw/dev/EXP1").isNone && ((← m7Row st re.id).map (·.nat? "purged_at" |>.isSome)) == some true &&
      (← m7One st "SELECT count(*) AS n FROM message_text WHERE message_id = ?" #[re.id]) == 0 &&
      (← Search.run st "matterhorn").isEmpty
  -- orphan GC: old orphan goes; a held raw, a pending spool raw and a fresh orphan stay
  let t0 := now - 3 * 86400000
  clock.set t0
  let heldKey := s!"raw/dev/{rpush.id}"
  db.exec "UPDATE messages SET raw_key = ? WHERE id = ?" #[heldKey, rpush.id]
  store.put heldKey "x".toUTF8 []
  store.put "raw/dev/01ORPHANOLD" "x".toUTF8 []
  store.put "raw/dev/01PENDING" "x".toUTF8 []
  store.put "in/dev/01PENDING" .empty []
  store.put "in/dev/01STALEHELD" .empty []
  db.exec "UPDATE messages SET id = id WHERE 0"
  clock.set now
  store.put "raw/dev/01ORPHANNEW" "x".toUTF8 []
  let og ← Expiry.orphanGc st now
  check t "orphan_gc: deletes old objects no row holds, keeps held, pending and fresh ones, and resets its cursor at the end" <|
    og ≥ 1 && (← store.get "raw/dev/01ORPHANOLD").isNone && (← store.get heldKey).isSome &&
      (← store.get "raw/dev/01PENDING").isSome && (← store.get "raw/dev/01ORPHANNEW").isSome &&
      (← Expiry.cursorOf st Expiry.orphanLane) == ""
  let (sgDel, sgStuck) ← Expiry.spoolGc st now
  check t "spool_gc: an old marker whose raw is gone is dropped; one with its raw waiting stays" <|
    sgDel == 1 && sgStuck == 1 && (← store.get "in/dev/01STALEHELD").isNone && (← store.get "in/dev/01PENDING").isSome
  db.exec "UPDATE messages SET send_status = 'sending', updated_at = ? WHERE id = ?" #[(now - 2 * 86400000 : Nat), rsoon.id]
  check t "stuck_sends: a send still 'sending' after a day is 'unknown'" <|
    (← Expiry.stuckSends st now) == 1 && ((← m7Row st rsoon.id).map (·.text "send_status")) == some "unknown"
  let ran ← Jobs.tick st true
  check t "Jobs.tick runs expire, purge and stuck_sends; daily adds orphan_gc and spool_gc" <|
    ran.contains "expire" && ran.contains "purge" && ran.contains "stuck_sends" && ran.contains "orphan_gc" && ran.contains "spool_gc"

  -- M4 follow-ups: links and health
  let sp ← m7Go st (m7Req "GET" "/settings" (dev ++ html))
  check t "the settings page and the header menu link routing, blocks, Gmail, calendar and expiring" <|
    sp.status == 200 && ["/settings/routing", "/settings/blocks", "/settings/gmail", "/calendar", "/expiring"].all
      (fun l => count sp.bodyText s!"href=\"{l}\"" ≥ 2)
  store.put "in/dev/01WAITING" .empty []
  let hj ← Health.internalJson st
  check t "/internal/health reports gmail {connected, last_poll_at, last_push_at, watch_expiration} and spool_pending" <|
    ((hj.getObjVal? "gmail").toOption.map fun g => (g.getObjValAs? Bool "connected").toOption == some false &&
      (g.getObjVal? "watch_expiration").isOk) == some true &&
      (hj.getObjValAs? Nat "spool_pending").toOption == some 1


def m7Checks (t : IO.Ref Tally) : IO Unit := do
  let m ← m7Setup
  m7Dev t m
  let (evId, r1id) ← m7Calendar t m
  m7Junk t m r1id
  m7Routing t m
  m7Push t m evId

/-! ## M6b: MCP over HTTP, privileged mail, the optional AI -/

/-- A fake chat-completions model: answers by the system prompt, records every request. -/
structure FakeAi where
  calls : IO.Ref (Array Core.Http.Request)

def FakeAi.new : IO FakeAi := do return { calls := ← IO.mkRef #[] }

def FakeAi.api (f : FakeAi) : WebApi where
  request := fun r => do
    f.calls.modify (·.push r)
    let body := (String.fromUTF8? r.body).getD ""
    let content :=
      if has body "You sort" then "Here you go: {\"label\": \"needs_reply\", \"priority\": 4, \"reason\": \"Asks for a quote.\"} Hope that helps."
      else if has body "You summarise" then "Tomas asks for the unit price of 500 labels; Alex has to answer."
      else if has body "You draft" then "Hi Tomas, the unit price is 0.40. Best, Alex"
      else "?"
    let reply := Lean.Json.mkObj [("choices", Lean.Json.arr #[Lean.Json.mkObj [("message",
      Lean.Json.mkObj [("role", Lean.Json.str "assistant"), ("content", Lean.Json.str content)])]])]
    return { status := 200, headers := [("content-type", "application/json")], body := reply.compress.toUTF8 }

def FakeAi.count (f : FakeAi) : IO Nat := do return (← f.calls.get).size

/-- A JSON-RPC call to `/mcp` with a bearer token. -/
def mcpReq (tok : String) (body : String) (hs : List (String × String) := []) : Router.Req :=
  { method := "POST", path := ["mcp"], headers := [("x-account-id", "acct1"), ("authorization", s!"Bearer {tok}"),
      ("content-type", "application/json")] ++ hs, body := body.toUTF8 }

def mcpCall (name : String) (args : String) (id : Nat := 1) : String :=
  s!"\{\"jsonrpc\":\"2.0\",\"id\":{id},\"method\":\"tools/call\",\"params\":\{\"name\":\"{name}\",\"arguments\":{args}}}"

/-- A `tools/call` answer: `(isError, the text parsed as JSON)`. -/
def toolOut (r : Router.Resp) : Bool × Lean.Json :=
  let res := ((jsonOf r).getObjVal? "result").toOption.getD .null
  let isErr := ((res.getObjValAs? Bool "isError").toOption).getD false
  let text := ((res.getObjValAs? (Array Lean.Json) "content").toOption.bind (·[0]?) |>.bind
    fun c => (c.getObjValAs? String "text").toOption).getD ""
  (isErr, (Lean.Json.parse text).toOption.getD .null)

def jItems (j : Lean.Json) : Array Lean.Json := (j.getObjValAs? (Array Lean.Json) "items").toOption.getD #[]
def jS (j : Lean.Json) (k : String) : String := (j.getObjValAs? String k).toOption.getD ""
def jB (j : Lean.Json) (k : String) : Bool := (j.getObjValAs? Bool k).toOption.getD false

def m6bChecks (t : IO.Ref Tally) : IO Unit := do
  let db ← Db.openMemory
  discard <| Schema.migrate db
  let (_, worker) ← recorder
  let cfg : Config := { accountId := "acct1", sessionSecret := "k1", publicOrigin := "https://mail.test",
                        internalSecret := "s3cret", mailFrom := "no-reply@mail.test" }
  let st : State := { config := cfg, db, store := ← Store.memory, worker, startedAt := 0, schemaVersion := 7, fts5 := true }
  let go (st' : State) (r : Router.Req) : IO Router.Resp := Router.dispatch st' (Routes.all st'.config) r
  let ing (raw : String) : IO String := do
    let r ← Ingest.ingest st { raw := raw.toUTF8, key := none, rcpt := some "alex@fralex.art", source := "route", receivedAt := ← Clock.nowMs }
    return r.id
  discard <| go st { method := "POST", path := ["internal", "provision"], headers := [("x-account-id", "acct1"), ("x-internal-secret", "s3cret")], body := "{\"email\":\"alex@fralex.art\",\"display_name\":\"Alex\"}".toUTF8 }
  let q1 ← ing (m7Plain "Tomas Reyes <tomas@labelforge.io>" "q1@labelforge.io" "Quote for 500 labels" "Could you send the unit price for 500 labels?")
  let q2 ← ing (m7Plain "Tomas Reyes <tomas@labelforge.io>" "q2@labelforge.io" "Re: Quote for 500 labels" "Any news on the labels?"
    ["In-Reply-To: <q1@labelforge.io>", "References: <q1@labelforge.io>"])
  let pv ← ing (m7Plain "Dana Holt <dana@holt-partners.example>" "p1@holt-partners.example" "Settlement agreement, second draft"
    "Privileged and confidential. Please review the attached labels clause before Friday.")
  let row (id : String) := db.queryOne "SELECT * FROM messages WHERE id = ?" #[id]
  let r1 ← row q1
  let r2 ← row q2
  let rp ← row pv
  check t "m6b: ingest marks legal mail privileged and leaves the rest" <|
    (rp.map (·.bool "privileged")) == some true && (r1.map (·.bool "privileged")) == some false &&
      (r1.bind (·.text? "thread_id")) == (r2.bind (·.text? "thread_id")) && (r1.bind (·.nat? "junk_at")).isNone
  check t "m6b: the privilege rules port the old app's phrases and the sender list" <|
    let r : Core.Mcp.Privilege.Rules := {}
    Core.Mcp.Privilege.isPrivileged r "x@clinic.example" "" "Your lab result is ready" "" &&
      Core.Mcp.Privilege.isPrivileged { r with senders := ["law.example"] } "a@law.example" "" "hello" "" &&
      Core.Mcp.Privilege.isPrivileged r "j@x.example" "John Smith, Esq." "hi" "" &&
      !Core.Mcp.Privilege.isPrivileged r "tomas@labelforge.io" "Tomas" "Quote for 500 labels" "the unit price"
  check t "m6b: the AI's first-object reader skips prose and braces inside strings" <|
    Core.Prompts.firstObject "Sure! {\"a\": \"}{\", \"b\": {\"c\": 1}} trailing" == some "{\"a\": \"}{\", \"b\": {\"c\": 1}}" &&
      Core.Prompts.firstObject "no json" == none &&
      (Core.Prompts.parseTriage (Lean.Json.mkObj [("label", Lean.Json.str "Needs reply"), ("priority", Lean.Json.num 9)])).map (·.priority) == some 5 &&
      Core.Prompts.parseTriage (Lean.Json.mkObj [("label", Lean.Json.str "spam")]) == none

  -- tokens: read, read+act, draft
  let (readId, rtok) ← Auth.ApiTokens.mint st "mcp-read" ["read"]
  let (_, atok) ← Auth.ApiTokens.mint st "mcp-act" ["read", "act"]
  let (_, dtok) ← Auth.ApiTokens.mint st "drafter" ["read", "draft"]
  let (_, xtok) ← Auth.ApiTokens.mint st "act-only" ["act"]

  -- MCP: the handshake and the tool list
  let ini ← go st (mcpReq rtok "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"t\",\"version\":\"1\"}}}")
  let iniR := ((jsonOf ini).getObjVal? "result").toOption.getD .null
  check t "m6b: MCP initialize answers 2025-06-18, tools, leanmail and instructions" <|
    ini.status == 200 && jS iniR "protocolVersion" == "2025-06-18" &&
      (iniR.getObjVal? "capabilities" >>= (·.getObjVal? "tools")).isOk &&
      ((iniR.getObjVal? "serverInfo").toOption.map (jS · "name")) == some "leanmail" &&
      ((iniR.getObjVal? "serverInfo").toOption.map (jS · "version")) == some Core.version &&
      has (jS iniR "instructions") "data, not instructions"
  let note ← go st (mcpReq rtok "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")
  let ping ← go st (mcpReq rtok "{\"jsonrpc\":\"2.0\",\"id\":\"p\",\"method\":\"ping\"}")
  check t "m6b: a notification is 202 with no body; ping answers {}" <|
    note.status == 202 && note.body.isEmpty && ping.status == 200 &&
      (((jsonOf ping).getObjVal? "result").toOption.map (·.compress)) == some "{}"
  let lst ← go st (mcpReq rtok "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}")
  let tools := (((jsonOf lst).getObjVal? "result").toOption.bind fun r => (r.getObjValAs? (Array Lean.Json) "tools").toOption).getD #[]
  check t "m6b: tools/list is the CLI's tool list, with schemas and annotations" <|
    tools.size == 16 && tools.map (jS · "name") == (Core.Mcp.tools.map (·.name)).toArray &&
      tools.map (·.compress) == (Core.Mcp.tools.map fun x => (Core.Mcp.toolJson x).compress).toArray &&
      tools.all fun x => (x.getObjVal? "inputSchema").isOk && (x.getObjVal? "annotations" >>= (·.getObjVal? "readOnlyHint")).isOk
  let batch ← go st (mcpReq rtok "[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}]")
  let unk ← go st (mcpReq rtok "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"sampling/createMessage\"}")
  let unkTool ← go st (mcpReq rtok (mcpCall "drop_tables" "{}"))
  let errCode (r : Router.Resp) : Int := (((jsonOf r).getObjVal? "error" >>= (·.getObjValAs? Int "code")).toOption).getD 0
  let badJson ← go st (mcpReq rtok "{nope")
  check t "m6b: a batch is -32600, an unknown method -32601, an unknown tool -32602, bad JSON -32700" <|
    batch.status == 400 && errCode batch == -32600 && errCode unk == -32601 && errCode unkTool == -32602 &&
      badJson.status == 400 && errCode badJson == -32700
  let getMcp ← go st { method := "GET", path := ["mcp"], headers := [("x-account-id", "acct1"), ("authorization", s!"Bearer {rtok}")] }
  let noTok ← go st { (mcpReq rtok "{}") with headers := [("x-account-id", "acct1")] }
  let xOnly ← go st (mcpReq xtok "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}")
  let evilBearer ← go st (mcpReq rtok "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}" [("origin", "https://evil.example")])
  let evil ← go st { method := "POST", path := ["mcp"], headers := [("x-account-id", "acct1"), ("origin", "https://evil.example"), ("cookie", "lm_session=x"), ("x-requested-with", "LeanMail")], body := "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}".toUTF8 }
  let okOrigin ← go st (mcpReq rtok "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}" [("origin", "https://mail.test")])
  let badVer ← go st (mcpReq rtok "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}" [("mcp-protocol-version", "1999-01-01")])
  check t "m6b: GET /mcp is 405; no token 401; a token without read 403; a foreign Origin only matters without a bearer; a bad protocol header 400" <|
    getMcp.status == 405 && noTok.status == 401 && xOnly.status == 403 && (evil.status == 403 || evil.status == 401) && evilBearer.status == 200 && okOrigin.status == 200 &&
      badVer.status == 400

  -- MCP tools against the server functions
  let (e1, s1) := toolOut (← go st (mcpReq rtok (mcpCall "search_mail" "{\"query\":\"labels\"}")))
  let ids1 := (jItems s1).map (jS · "id")
  let heldItem := (jItems s1).find? (jS · "id" == pv)
  check t "m6b: search_mail round trip finds the thread and withholds the privileged hit" <|
    !e1 && ids1.contains q1 && ids1.contains q2 && ids1.contains pv &&
      (heldItem.map fun h => jB h "withheld" && !(h.getObjVal? "subject").isOk) == some true &&
      (s1.getObjValAs? Nat "withheld").toOption == some 1 &&
      ((jItems s1).find? (jS · "id" == q1)).map (jS · "subject") == some "Quote for 500 labels"
  let (eg, g1) := toolOut (← go st (mcpReq rtok (mcpCall "get_message" s!"\{\"id\":\"{q1}\"}")))
  let (_, gp) := toolOut (← go st (mcpReq rtok (mcpCall "get_message" s!"\{\"id\":\"{pv}\"}")))
  let (_, ap) := toolOut (← go st (mcpReq rtok (mcpCall "get_attachment" s!"\{\"id\":\"{pv}\",\"idx\":0}")))
  let (eMissing, gm) := toolOut (← go st (mcpReq rtok (mcpCall "get_message" "{\"id\":\"01JNOPE0000000000000000000\"}")))
  check t "m6b: get_message answers the message without raw_key; a privileged one is {id, withheld}" <|
    !eg && jS g1 "subject" == "Quote for 500 labels" && has (jS g1 "text") "unit price" && !(g1.getObjVal? "raw_key").isOk &&
      gp.compress == (Lean.Json.mkObj [("id", Lean.Json.str pv), ("withheld", Lean.Json.bool true)]).compress && jB ap "withheld" &&
      eMissing && jS gm "error" == "not found"
  let (eR, rec) := toolOut (← go st (mcpReq rtok (mcpCall "recent_mail" "{\"limit\":10}")))
  let (eBadF, _) := toolOut (← go st (mcpReq rtok (mcpCall "recent_mail" "{\"filter\":\"nope\"}")))
  check t "m6b: recent_mail lists the inbox with the privileged entry withheld; a bad filter is an error" <|
    !eR && (jItems rec).size == 3 && ((jItems rec).filter (jB · "withheld")).size == 1 && eBadF
  let (eA, accJ) := toolOut (← go st (mcpReq rtok (mcpCall "account_status" "{}")))
  let pw := (accJ.getObjVal? "privileged_withheld").toOption.getD .null
  check t "m6b: account_status reports health, the account and the privileged counts" <|
    !eA && (pw.getObjValAs? Nat "privileged").toOption == some 1 && (pw.getObjValAs? Nat "withheld").toOption == some 1 &&
      ((accJ.getObjVal? "account").toOption.map (jS · "email")) == some "alex@fralex.art" &&
      ((accJ.getObjVal? "health").toOption.map (jB · "ok")) == some true
  -- scopes per tool
  let (eDone, doneJ) := toolOut (← go st (mcpReq rtok (mcpCall "mark_done" s!"\{\"id\":\"{q1}\"}")))
  let still ← row q1
  let (eSave, _) := toolOut (← go st (mcpReq rtok (mcpCall "save_draft" "{\"id\":\"01JDRAFT000000000000000001\",\"mutation_id\":\"m1\"}")))
  let (eSend, _) := toolOut (← go st (mcpReq dtok (mcpCall "send_draft" "{\"id\":\"01JDRAFT000000000000000001\",\"revision\":1}")))
  check t "m6b: a read token cannot mark_done (isError, insufficient scope), save_draft or send_draft" <|
    eDone && jS doneJ "error" == "insufficient scope" && jS doneJ "scope" == "act" &&
      (still.bind (·.nat? "archived_at")).isNone && eSave && eSend
  let (eDone2, done2) := toolOut (← go st (mcpReq atok (mcpCall "mark_done" s!"\{\"id\":\"{q2}\"}")))
  let (eSn, sn) := toolOut (← go st (mcpReq atok (mcpCall "snooze" s!"\{\"id\":\"{q1}\",\"until\":\"1d\"}")))
  let (eSnBad, _) := toolOut (← go st (mcpReq atok (mcpCall "snooze" s!"\{\"id\":\"{q1}\",\"until\":\"someday\"}")))
  let r2b ← row q2
  check t "m6b: an act token marks done and snoozes through the tools" <|
    !eDone2 && jS done2 "id" == q2 && (r2b.bind (·.nat? "archived_at")).isSome && !eSn && (sn.getObjValAs? Nat "snoozed_until").isOk && eSnBad
  discard <| Actions.apply st q1 "inbox"
  discard <| Actions.apply st q2 "inbox"
  -- drafts through the tools
  let (_, ids) := toolOut (← go st (mcpReq dtok (mcpCall "new_draft_ids" "{\"count\":2}")))
  let did := (((ids.getObjValAs? (Array String) "ids").toOption.bind (·[0]?))).getD ""
  let (eSv, sv) := toolOut (← go st (mcpReq dtok (mcpCall "save_draft"
    s!"\{\"id\":\"{did}\",\"mutation_id\":\"mut1\",\"to\":[\"tomas@labelforge.io\"],\"subject\":\"Re: Quote\",\"text\":\"Soon.\"}")))
  let (eSv2, sv2) := toolOut (← go st (mcpReq dtok (mcpCall "save_draft"
    s!"\{\"id\":\"{did}\",\"mutation_id\":\"mut2\",\"revision\":7,\"text\":\"x\"}")))
  let (_, gd) := toolOut (← go st (mcpReq rtok (mcpCall "get_draft" s!"\{\"id\":\"{did}\"}")))
  let (_, ld) := toolOut (← go st (mcpReq rtok (mcpCall "list_drafts" "{}")))
  let (eDel, _) := toolOut (← go st (mcpReq dtok (mcpCall "delete_draft" s!"\{\"id\":\"{did}\",\"revision\":1}")))
  check t "m6b: new_draft_ids, save_draft (and its revision conflict), get_draft, list_drafts, delete_draft" <|
    Core.Ulid.isValid did && !eSv && (sv.getObjValAs? Nat "revision").toOption == some 1 &&
      eSv2 && jS sv2 "error" == "revision" && (sv2.getObjValAs? Nat "status").toOption == some 409 &&
      jS gd "subject" == "Re: Quote" && (jItems ld).size == 1 && !eDel
  -- the log
  let logs ← db.query "SELECT * FROM mcp_log ORDER BY rowid"
  let cols ← db.query "SELECT name FROM pragma_table_info('mcp_log')"
  check t "m6b: every tools/call is in mcp_log with token, tool, ok and ms, and no arguments" <|
    logs.size ≥ 15 && logs.any (fun r => r.text "tool" == "search_mail" && r.text "token_id" == readId && r.bool "ok") &&
      logs.any (fun r => r.text "tool" == "mark_done" && r.text "token_id" == readId && !r.bool "ok") &&
      cols.map (·.text "name") == #["at", "token_id", "tool", "ok", "ms"] &&
      !logs.any (fun r => r.text "tool" == "drop_tables")

  -- the routes the CLI needs with a bearer
  let bearer (tok : String) := [("x-account-id", "acct1"), ("authorization", s!"Bearer {tok}")]
  let tokR ← go st { method := "GET", path := ["api", "tokens"], headers := bearer rtok }
  let gmR ← go st { method := "GET", path := ["api", "gmail"], headers := bearer rtok }
  let acR ← go st { method := "GET", path := ["api", "account"], headers := bearer rtok }
  let bfR ← go st { method := "POST", path := ["api", "gmail", "backfill", "pause"], headers := bearer rtok }
  let bfA ← go st { method := "POST", path := ["api", "gmail", "backfill", "pause"], headers := bearer atok }
  check t "m6b: /api/tokens, /api/gmail and /api/account read with a read token; backfill controls need act" <|
    tokR.status == 200 && has tokR.bodyText "mcp-read" && !(has tokR.bodyText rtok) && gmR.status == 200 &&
      acR.status == 200 && bfR.status == 403 && bfA.status != 403 && bfA.status != 401

  -- AI off: the routes are 404, the lane does nothing, the settings page says so
  let tid := ((r1.bind (·.text? "thread_id"))).getD ""
  let offSum ← go st { method := "POST", path := ["api", "threads", tid, "summary"], headers := bearer rtok }
  let offDraft ← go st { method := "POST", path := ["api", "ai", "draft"], headers := bearer dtok, body := "{\"intent\":\"x\"}".toUTF8 }
  let offStatus ← go st { method := "GET", path := ["api", "ai"], headers := bearer rtok }
  let ranOff ← Jobs.tick st false
  let stDevOff : State := { st with config := { devBypass := true } }
  let dev := [("x-account-id", "dev"), ("accept", "text/html")]
  let pageOff ← go stDevOff { method := "GET", path := ["settings", "ai"], headers := dev }
  let msgOff ← go stDevOff { method := "GET", path := ["m", q1], headers := dev }
  check t "m6b: with the AI off its routes are 404, no ai lane runs, and the settings page says it is not configured" <|
    offSum.status == 404 && offDraft.status == 404 && offStatus.status == 404 && !ranOff.contains "ai" &&
      pageOff.status == 200 && has pageOff.bodyText "not configured" && has pageOff.bodyText "Privileged mail" &&
      !(has msgOff.bodyText "Summarize thread") && Config.aiEnabled cfg == false

  -- AI on, with a fake model
  let fake ← FakeAi.new
  let aiCfg : Config := { cfg with aiBaseUrl := "https://ai.test/v1/", aiApiKey := "sk-test", aiModel := "test-model" }
  let stAi : State := { st with config := aiCfg, ai := fake.api }
  let ran ← Jobs.tick stAi false
  let tri ← db.query "SELECT * FROM ai_triage"
  let calls0 ← fake.calls.get
  let firstBody := (calls0[0]?.bind fun r => String.fromUTF8? r.body).getD ""
  let firstJ := (Lean.Json.parse firstBody).toOption.getD .null
  check t "m6b: the ai lane triages new mail into ai_triage and skips the privileged message" <|
    ran.contains "ai" && tri.size == 2 && tri.all (fun r => r.text "label" == "needs_reply" && r.nat "priority" == 4 &&
      r.text "model" == "test-model") && !tri.any (·.text "message_id" == pv) && calls0.size == 2
  check t "m6b: the AI request is POST {base}/chat/completions, bearer key, temperature 0, system + user" <|
    (calls0[0]?.map fun r => r.method == "POST" && r.url == "https://ai.test/v1/chat/completions" &&
      r.headers.contains ("Authorization", "Bearer sk-test")) == some true &&
      jS firstJ "model" == "test-model" && (firstJ.getObjValAs? Nat "temperature").toOption == some 0 &&
      (firstJ.getObjValAs? Nat "max_tokens").isOk &&
      ((firstJ.getObjValAs? (Array Lean.Json) "messages").toOption.map (·.map (jS · "role"))) == some #["system", "user"]
  let again ← Jobs.tick stAi false
  check t "m6b: a second tick triages nothing new" (again.contains "ai" && (← fake.count) == 2)
  let lstAi ← go stAi { method := "GET", path := ["api", "msgs"], headers := bearer rtok }
  let item1 := (jItems (jsonOf lstAi)).find? (jS · "id" == q1)
  let itemP := (jItems (jsonOf lstAi)).find? (jS · "id" == pv)
  check t "m6b: the list item carries triage {label, priority}; untriaged mail has none" <|
    (item1.bind fun i => (i.getObjVal? "triage").toOption.map (·.compress)) ==
      some (Lean.Json.mkObj [("label", Lean.Json.str "needs_reply"), ("priority", Lean.Json.num 4)]).compress &&
      (itemP.map fun i => !(i.getObjVal? "triage").isOk) == some true
  let stDevAi : State := { stAi with config := { aiCfg with devBypass := true, accountId := "dev", internalSecret := "", publicOrigin := "http://127.0.0.1:8787" } }
  let inboxAi ← go stDevAi { method := "GET", path := ["inbox"], headers := dev }
  check t "m6b: the inbox renders the triage chip" (inboxAi.status == 200 && has inboxAi.bodyText "badge ai ai-needs_reply" && has inboxAi.bodyText ">needs reply<")
  -- the summary, cached
  let n0 ← fake.count
  let sum1 ← go stAi { method := "POST", path := ["api", "threads", tid, "summary"], headers := bearer rtok }
  let n1 ← fake.count
  let sum2 ← go stAi { method := "POST", path := ["api", "threads", tid, "summary"], headers := bearer rtok }
  let n2 ← fake.count
  let cachedRows ← db.query "SELECT * FROM ai_summaries"
  check t "m6b: a thread summary is {summary, model, at}, cached: the second call makes no AI request" <|
    sum1.status == 200 && has (jget sum1 "summary") "unit price" && jget sum1 "model" == "test-model" &&
      ((jsonOf sum1).getObjValAs? Nat "at").isOk && n1 == n0 + 1 && sum2.status == 200 && n2 == n1 &&
      jget sum2 "summary" == jget sum1 "summary" && cachedRows.size == 1
  let sumR ← go stAi { method := "POST", path := ["api", "threads", tid, "summary"], query := [("refresh", "1")], headers := bearer rtok }
  let ptid := ((rp.bind (·.text? "thread_id"))).getD ""
  let sumP ← go stAi { method := "POST", path := ["api", "threads", ptid, "summary"], headers := bearer rtok }
  check t "m6b: ?refresh=1 asks again; a privileged thread is refused 403 without an AI request" <|
    sumR.status == 200 && (← fake.count) == n2 + 1 && sumP.status == 403 && jget sumP "error" == "privileged" &&
      (← fake.count) == n2 + 1
  let msgAi ← go stDevAi { method := "GET", path := ["m", q1], headers := dev }
  let msgP ← go stDevAi { method := "GET", path := ["m", pv], headers := dev }
  check t "m6b: the message page shows the cached summary and the button; a privileged one says why not" <|
    has msgAi.bodyText "ai-summary" && has msgAi.bodyText "Summarize again" && has msgAi.bodyText "unit price of 500 labels" &&
      has msgP.bodyText "This thread is privileged"
  -- the draft suggestion
  let drafts0 ← db.query "SELECT count(*) AS n FROM drafts"
  let sug ← go stAi { method := "POST", path := ["api", "ai", "draft"], headers := bearer dtok, body := s!"\{\"message_id\":\"{q1}\",\"intent\":\"tell him 0.40\"}".toUTF8 }
  let sugRead ← go stAi { method := "POST", path := ["api", "ai", "draft"], headers := bearer rtok, body := "{\"intent\":\"x\"}".toUTF8 }
  let sugP ← go stAi { method := "POST", path := ["api", "ai", "draft"], headers := bearer dtok, body := s!"\{\"message_id\":\"{pv}\",\"intent\":\"x\"}".toUTF8 }
  let drafts1 ← db.query "SELECT count(*) AS n FROM drafts"
  let lastBody := ((← fake.calls.get).back?.bind fun r => String.fromUTF8? r.body).getD ""
  check t "m6b: a draft suggestion returns {text} only, saves nothing, needs draft, refuses privileged context" <|
    sug.status == 200 && jget sug "text" == "Hi Tomas, the unit price is 0.40. Best, Alex" &&
      !((jsonOf sug).getObjVal? "id").isOk && sugRead.status == 403 && sugP.status == 403 &&
      drafts0[0]!.nat "n" == drafts1[0]!.nat "n" && has lastBody "tell him 0.40" && has lastBody "unit price"
  let everything := String.join ((← fake.calls.get).toList.map fun r => (String.fromUTF8? r.body).getD "")
  check t "m6b: no request to the AI ever carried the privileged message" <|
    !(has everything "Settlement") && !(has everything "holt-partners") && !(has everything "Privileged and confidential")
  let stOn ← go stAi { method := "GET", path := ["api", "ai"], headers := bearer rtok }
  let pageOn ← go stDevAi { method := "GET", path := ["settings", "ai"], headers := dev }
  let compose ← go stDevAi { method := "GET", path := ["compose"], query := [("reply", q1)], headers := dev }
  check t "m6b: GET /api/ai says enabled; the settings page shows the model, never the key" <|
    stOn.status == 200 && jget stOn "model" == "test-model" && has pageOn.bodyText "test-model" &&
      !(has pageOn.bodyText "sk-test") && has pageOn.bodyText "ai.test" && compose.status == 200
  -- the owner's switch
  let offForm ← go stDevAi { method := "POST", path := ["settings", "ai"], headers := dev ++ [("content-type", "application/x-www-form-urlencoded"), ("origin", "http://127.0.0.1:8787")], body := "".toUTF8 }
  let offNow ← go stAi { method := "GET", path := ["api", "ai"], headers := bearer rtok }
  let nOff ← fake.count
  let ranOff2 ← Jobs.tick stAi false
  check t "m6b: switching the pref off makes the AI routes 404 and stops the lane" <|
    offForm.status == 303 && offNow.status == 404 && !ranOff2.contains "ai" && (← fake.count) == nOff
  discard <| go stDevAi { method := "POST", path := ["settings", "ai"], headers := dev ++ [("content-type", "application/x-www-form-urlencoded"), ("origin", "http://127.0.0.1:8787")], body := "enabled=1".toUTF8 }
  check t "m6b: and back on" ((← go stAi { method := "GET", path := ["api", "ai"], headers := bearer rtok }).status == 200)
  -- the privileged lists: a listed sender re-marks its mail, drops its triage and summary, and MCP withholds it
  let listsR ← go stDevAi { method := "POST", path := ["settings", "ai", "privileged"], headers := dev ++ [("content-type", "application/x-www-form-urlencoded"), ("origin", "http://127.0.0.1:8787")], body := "words=&senders=LabelForge.io%0D%0A".toUTF8 }
  let r1c ← row q1
  let triLeft ← db.query "SELECT * FROM ai_triage WHERE message_id = ?" #[q1]
  let sumLeft ← db.query "SELECT * FROM ai_summaries WHERE thread_id = ?" #[tid]
  let (_, g1c) := toolOut (← go st (mcpReq rtok (mcpCall "get_message" s!"\{\"id\":\"{q1}\"}")))
  let senders ← db.queryOne "SELECT value FROM prefs WHERE key = 'privileged_senders'"
  check t "m6b: listing a sender domain re-marks its mail privileged, drops its AI output, and MCP withholds it" <|
    listsR.status == 303 && has ((listsR.headers.lookup "Location").getD "") "changed=2" &&
      (r1c.map (·.bool "privileged")) == some true && triLeft.isEmpty && sumLeft.isEmpty && jB g1c "withheld" &&
      (senders.map (·.text "value")) == some "[\"labelforge.io\"]" &&
      (rp.map (·.bool "privileged")) == some true
  discard <| go stDevAi { method := "POST", path := ["settings", "ai", "privileged"], headers := dev ++ [("content-type", "application/x-www-form-urlencoded"), ("origin", "http://127.0.0.1:8787")], body := "words=&senders=".toUTF8 }
  let r1d ← row q1
  check t "m6b: clearing the lists un-marks it again (the built-in words still hold the legal mail)" <|
    (r1d.map (·.bool "privileged")) == some false && ((← row pv).map (·.bool "privileged")) == some true
  -- a transport failure keeps the cursor and records the error
  let failing : State := { stAi with ai := { request := fun _ => throw (IO.userError "connection refused") } }
  discard <| ing (m7Plain "Mia <mia@example.org>" "n1@example.org" "Dinner on Saturday?" "Are you free on Saturday?")
  let curBefore ← db.queryOne "SELECT cursor FROM jobs WHERE name = 'ai'"
  discard <| Jobs.tick failing false
  let laneRow ← db.queryOne "SELECT cursor, last_error FROM jobs WHERE name = 'ai'"
  discard <| Jobs.tick stAi false
  let mia ← db.queryOne "SELECT count(*) AS n FROM ai_triage t JOIN messages m ON m.id = t.message_id WHERE m.subject = 'Dinner on Saturday?'"
  check t "m6b: a failing AI leaves the cursor and records the lane error; the next tick catches up" <|
    (laneRow.bind (·.text? "last_error")) == some "ai_transport" &&
      (laneRow.bind (·.text? "cursor")) == (curBefore.bind (·.text? "cursor")) && (mia.map (·.nat "n")) == some 1

/-! ## M9b: passkeys

The vectors below come from `gen-passkey-vectors.mjs` (node's `crypto`; kept with the M9b notes):
the RFC 6979 A.2.5 P-256/SHA-256 signature of "sample", DER-encoded, and one generated credential
key with WebAuthn assertions over `authenticatorData || sha256(clientDataJSON)` for
`https://mail.test`. Each assertion's challenge is a fixed `v1.<rand>.<far future>.<mac>` token,
which the container takes on the Worker's word (`x-passkey-challenge-ok`). -/

namespace PK
def rfcX : String := "60fed4ba255a9d31c961eb74c6356d68c049b8923b61fa6ce669622e60f29fb6"
def rfcY : String := "7903fe1008b8bc99a41ae9e95628bc64f2f1b20c2d7e9f5177a3c294d4462299"
def rfcSig : String := "3046022100efd48b2aacb6a8fd1140dd9cd45e81d69d2c877b56aaf991c34d0ea84eaf3716022100f7cb1c942d657c41d436c7a1b6e29f65f3e900dbb9aff4064dc4ab2f843acda8"
def pkX : String := "51ee4ccf7ed09ec2d09eac31069259349be5716a8617716458b4fc4230a8b53a"
def pkY : String := "ebacb9bd042b7fccae2db41b099f904282c549cb2391b00529d182e5911c5b09"
def credA : String := "a65e41b2b7ab0e8aa75dfc58a68dc4cc"
def credB : String := "7c643c83192a8cee8e972a9c5be74ba9df0596fb"
-- a1: signCount 5
def a1Tok : String := "v1.duIp7EZ1HUL_IAc8NJXi4p-9.9999999999999.0wC8QqNi3z5fb2e5xJNQnxKk89Tac7B3MV-h7wVINXw"
def a1Cdj : String := "7b2274797065223a22776562617574686e2e676574222c226368616c6c656e6765223a22646a45755a48564a63446446576a4649565578665355466a4f45354b57476b30634330354c6a6b354f546b354f546b354f546b354f546b754d4864444f464678546d6b7a656a566d596a4a6c4e58684b546c4675654574724f446c5559574d33516a4e4e5669316f4e336457535535596477222c226f726967696e223a2268747470733a2f2f6d61696c2e74657374222c2263726f73734f726967696e223a66616c73657d"
def a1Ad : String := "8eab005901b0d55f8c8c764c06361c4e5335c16af6842e3833142c26bdde9f770500000005"
def a1Sig : String := "3045022100edb7d11476066181d66df66853aed831b7705521977ca9a83c13c9afc6ea5af3022075eadccaf84eeab3bc9c9ca30a59a364a8552fcf8f5ae82ab72791a8db82228e"
-- a2: signCount 3
def a2Tok : String := "v1.ktI8nXRtp9tk1nJqfH9KDcy9.9999999999999.E4X-BhkzFDi60yBVoPIz1T7u1CtISUOQSCc6MeLt7t4"
def a2Cdj : String := "7b2274797065223a22776562617574686e2e676574222c226368616c6c656e6765223a22646a45756133524a4f473559556e52774f5852724d57354b63575a494f55744559336b354c6a6b354f546b354f546b354f546b354f546b75525452594c554a6f6133704752476b324d486c43566d395153586f78564464314d554e3053564e565431465451324d325457564d644464304e41222c226f726967696e223a2268747470733a2f2f6d61696c2e74657374222c2263726f73734f726967696e223a66616c73657d"
def a2Ad : String := "8eab005901b0d55f8c8c764c06361c4e5335c16af6842e3833142c26bdde9f770500000003"
def a2Sig : String := "304402203d5a3fde5fa1bcff966b1acdd6a58c849c0913d831a6c1ef856193dea4989c6e02201aaffabacd63b993ea70fa090604d1e46a188f6b66be3442fd7585fa78f8c754"
-- a3: signCount 9, origin https://evil.test
def a3Tok : String := "v1.uGPDEVdR-wUDEe6w7xBp_ZpQ.9999999999999.mYFxgsL25C_Bcn5MbvVp0RdN4LfAT7i1ufrSLiuZKbg"
def a3Cdj : String := "7b2274797065223a22776562617574686e2e676574222c226368616c6c656e6765223a22646a457564556451524556575a4649746431564552575532647a6434516e4266576e42524c6a6b354f546b354f546b354f546b354f546b7562566c476547647a54444931513139435932343154574a32566e4177556d524f4e45786d51565133615446315a6e4a5454476c31576b74695a77222c226f726967696e223a2268747470733a2f2f6576696c2e74657374222c2263726f73734f726967696e223a66616c73657d"
def a3Ad : String := "8eab005901b0d55f8c8c764c06361c4e5335c16af6842e3833142c26bdde9f770500000009"
def a3Sig : String := "3045022035c800ac2e97ccbfcf8dc8d99c88f944b77dbed5ce13ed98718405d850b1c3d3022100fe45999a42d988e596cc55822d9004344e375146d34f02d4d93b5dd1f750a1d2"
-- a4: signCount 7
def a4Tok : String := "v1.I9Qp9NW6Ip9ANzz9dYj04P8F.9999999999999.hCtArArpqYsx3MsiDvxfe2XNn0Vsm1XJykm2-nfkEr0"
def a4Cdj : String := "7b2274797065223a22776562617574686e2e676574222c226368616c6c656e6765223a22646a457553546c5263446c4f567a5a4a63446c42546e70364f57525a616a4130554468474c6a6b354f546b354f546b354f546b354f546b7561454e3051584a42636e427857584e344d30317a6155523265475a6c4d6c684f626a42576332307857457035613230794c57356d613056794d41222c226f726967696e223a2268747470733a2f2f6d61696c2e74657374222c2263726f73734f726967696e223a66616c73657d"
def a4Ad : String := "8eab005901b0d55f8c8c764c06361c4e5335c16af6842e3833142c26bdde9f770500000007"
def a4Sig : String := "3045022100c2f139bd9150ca251fafdce98314385dddee27d455b674954cc1b55dedeff81502203dca355e1ae2b5b0687880ecd6d3dde35cd2deaaa7f101288b0059d61f3aae09"
-- a5: signCount 8, rpId evil.test
def a5Tok : String := "v1.fLpoSYkRpGhDL4XjWoOWg7x5.9999999999999.pvZ0HGEtb9QE6L0Q1I8lvsCFC_Am6H6FeWLJbdyS4Sg"
def a5Cdj : String := "7b2274797065223a22776562617574686e2e676574222c226368616c6c656e6765223a22646a45755a6b787762314e5a61314a775232684554445259616c64765431646e4e3367314c6a6b354f546b354f546b354f546b354f546b7563485a614d456848525852694f5646464e6b77775554464a4f47783263304e475131394262545a494e6b5a6c5630784b596d5235557a52545a77222c226f726967696e223a2268747470733a2f2f6d61696c2e74657374222c2263726f73734f726967696e223a66616c73657d"
def a5Ad : String := "9834f30f8b807769d8d40e882f51d93a6db8b3f785357b3b7086326fbd575a530500000008"
def a5Sig : String := "3045022010fc40ab7885981c9a0ea984922cfae48062e5b26050c13e803fd7e11d959200022100b66ca78f252d83281a97c86b6af7fc95d75b342e6d9f884ac73db0e3cb2761cc"
-- a6: signCount 10, challenge exp 1
def a6Tok : String := "v1.EGFicIivFiJUF9VvNBpidru_.1.oiqp8q_uXVfEPbwphBS8UeEDtYu6eVFGuGil723GVuU"
def a6Cdj : String := "7b2274797065223a22776562617574686e2e676574222c226368616c6c656e6765223a22646a45755255644761574e4a61585a4761557056526a6c57646b354363476c6b636e56664c6a457562326c786344687858335659566d5a4655474a3363476843557a68565a55564564466c314e6d5657526b643152326c734e7a497a52315a315651222c226f726967696e223a2268747470733a2f2f6d61696c2e74657374222c2263726f73734f726967696e223a66616c73657d"
def a6Ad : String := "8eab005901b0d55f8c8c764c06361c4e5335c16af6842e3833142c26bdde9f77050000000a"
def a6Sig : String := "3046022100af2d02b6cadb3d2ba07502a29d72ff34727f973eb5948d01fa77b1ace964fe85022100f7923311fcb442871008e8e42633f7d9787e7f2dd52b2a1f931d5aaa31b463e2"
-- b1: signCount 0
def b1Tok : String := "v1.xTEB8mAjqPylFpVBXP4dzQqm.9999999999999.h5aR8OClcc19AEiLSFQ_h0zT2qIk6GlnPTitQeuW0D4"
def b1Cdj : String := "7b2274797065223a22776562617574686e2e676574222c226368616c6c656e6765223a22646a457565465246516a68745157707855486c73526e4257516c68514e475236555846744c6a6b354f546b354f546b354f546b354f546b7561445668556a68505132786a597a45355155567054464e475556396f4d4870554d6e464a617a5a486247355156476c3055575631567a42454e41222c226f726967696e223a2268747470733a2f2f6d61696c2e74657374222c2263726f73734f726967696e223a66616c73657d"
def b1Ad : String := "8eab005901b0d55f8c8c764c06361c4e5335c16af6842e3833142c26bdde9f770500000000"
def b1Sig : String := "30440220292a5a0994d85080ed8d4f8963c9c869a60a483b9d1ba8e2d81dc7550b26373b022053ce743fedb645061404fac9984d02bd7ff8dc622440146dd02028731df95600"

end PK

def unhex (s : String) : ByteArray := Id.run do
  let v (c : Char) : Nat := if c ≤ '9' then c.toNat - 48 else c.toNat - 87
  let cs := s.toList.toArray
  let mut out := ByteArray.empty
  for i in [:cs.size / 2] do
    out := out.push (v cs[2*i]! * 16 + v cs[2*i+1]!).toUInt8
  return out

def bytesOf (xs : List Nat) : ByteArray := ⟨(xs.map (·.toUInt8)).toArray⟩
def be4 (n : Nat) : ByteArray := bytesOf [n / 16777216 % 256, n / 65536 % 256, n / 256 % 256, n % 256]
def b64u (b : ByteArray) : String := Core.Base64.encodeUrl b
def jat (j : Lean.Json) (ks : List String) : Lean.Json :=
  ks.foldl (fun acc k => (acc.getObjValAs? Lean.Json k).toOption.getD .null) j
def cborInt? (e : Except String Core.Cbor.Value) (k : Int) : Option Int :=
  match e with
  | .ok v => (v.atInt k).bind (·.int?)
  | .error _ => none
def utf8Nats (s : String) : List Nat := s.toUTF8.toList.map UInt8.toNat
def isErr (e : Except String α) : Bool := match e with | .error _ => true | .ok _ => false

/-- An EC2 / ES256 / P-256 COSE key, with labels and values overridable for the refusals. -/
def coseKey (x y : ByteArray) (alg : Int := -7) (crv : Nat := 1) : ByteArray :=
  Core.Cbor.encode (.map #[(.uint 1, .uint 2), (.uint 3, Core.Cbor.int alg), (Core.Cbor.int (-1), .uint crv),
    (Core.Cbor.int (-2), .bytes x), (Core.Cbor.int (-3), .bytes y)])

/-- `authenticatorData` with an attested credential. -/
def attestedAd (rp : String) (flags : Nat) (count : Nat) (cred key : ByteArray) : ByteArray :=
  Crypto.sha256Text rp ++ bytesOf [flags] ++ be4 count ++ ByteArray.mk (Array.replicate 16 7) ++
    bytesOf [cred.size / 256, cred.size % 256] ++ cred ++ key

def m9bPasskeys (t : IO.Ref Tally) : IO Unit := do
  -- CBOR on hand-built bytes
  let small := Core.Cbor.decode (bytesOf [0xa2, 0x01, 0x02, 0x03, 0x26])
  check t "m9b: cbor decodes a map of small unsigned and negative ints" <|
    cborInt? small 1 == some 2 && cborInt? small 3 == some (-7) && cborInt? small 2 == none
  let mixed := Core.Cbor.decode (bytesOf [0x84, 0x63, 0x61, 0x62, 0x63, 0x42, 0x01, 0x02, 0x19, 0x01, 0x00, 0x38, 0x63])
  check t "m9b: cbor decodes an array of text, bytes, a two-byte uint and a one-byte negative int" <|
    match mixed with
    | .ok v => match v.array? with
      | some #[a, b, c, d] => a.text? == some "abc" && (b.bytes?.map ByteArray.toList) == some [1, 2] && c.int? == some 256 && d.int? == some (-100)
      | _ => false
    | .error _ => false
  let att := Core.Cbor.decode (bytesOf ([0xa3, 0x63] ++ utf8Nats "fmt" ++ [0x64] ++ utf8Nats "none" ++
    [0x67] ++ utf8Nats "attStmt" ++ [0xa0, 0x68] ++ utf8Nats "authData" ++ [0x43, 9, 8, 7]))
  check t "m9b: cbor reads text keys, an empty map and a byte string (an attestation object's shape)" <|
    match att with
    | .ok v => (v.atText "fmt").bind (·.text?) == some "none" && ((v.atText "attStmt").bind (·.map?)).map Array.size == some 0 &&
        ((v.atText "authData").bind (·.bytes?)).map ByteArray.toList == some [9, 8, 7]
    | .error _ => false
  check t "m9b: cbor refuses indefinite lengths, tags, floats, truncation, trailing bytes, lying counts and deep nesting" <|
    isErr (Core.Cbor.decode (bytesOf [0x5f, 0x41, 0x00, 0xff])) && isErr (Core.Cbor.decode (bytesOf [0xc0, 0x01])) &&
      isErr (Core.Cbor.decode (bytesOf [0xf9, 0x3c, 0x00])) && isErr (Core.Cbor.decode (bytesOf [0x43, 0x01])) &&
      isErr (Core.Cbor.decode (bytesOf [0x01, 0x02])) && isErr (Core.Cbor.decode (bytesOf [0x9b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff])) &&
      isErr (Core.Cbor.decode (bytesOf (List.replicate 20 0x81 ++ [0x01]))) && isErr (Core.Cbor.decode (bytesOf [0x62, 0xff, 0xfe])) &&
      isErr (Core.Cbor.decode ByteArray.empty)
  check t "m9b: cbor decodeAt reports where an item ends, so what follows it can be read" <|
    match Core.Cbor.decodeAt (bytesOf [0xa1, 0x01, 0x02, 0xde, 0xad]) 0 with
    | .ok (_, j) => j == 3
    | .error _ => false
  let x := unhex PK.pkX
  let y := unhex PK.pkY
  let cose := coseKey x y
  check t "m9b: cbor encode and decode round-trip a COSE key byte for byte" <|
    match Core.Cbor.decode cose with
    | .ok v => Core.Cbor.encode v == cose && cose[0]! == 0xa5
    | .error _ => false
  -- authenticator data on a synthetic record
  let credA := unhex PK.credA
  let credB := unhex PK.credB
  let ad := attestedAd "mail.test" 0x4d 3 credA cose
  check t "m9b: authData parses the rp id hash, flags, count, AAGUID, credential id and COSE key" <|
    match Auth.Passkey.parseAuthData ad with
    | .ok a => a.rpIdHash == Crypto.sha256Text "mail.test" && a.up && a.uv && a.backupEligible && a.hasAttested &&
        a.signCount == 3 && (a.attested.map (·.credentialId)) == some credA && (a.attested.map (·.coseKey)) == some cose &&
        (a.attested.map (·.aaguid)) == some (ByteArray.mk (Array.replicate 16 7))
    | .error _ => false
  let withExt := attestedAd "mail.test" 0xc5 3 credA cose ++ Core.Cbor.encode (.map #[(.text "credProps", .bool true)])
  check t "m9b: authData reads an extensions map after the key when the ED flag is set, and refuses stray bytes" <|
    !isErr (Auth.Passkey.parseAuthData withExt) && isErr (Auth.Passkey.parseAuthData (ad ++ bytesOf [0]))
  check t "m9b: authData refuses a short record and a credential id past the end" <|
    isErr (Auth.Passkey.parseAuthData (ad.extract 0 36)) && isErr (Auth.Passkey.parseAuthData (ad.extract 0 60))
  let assertAd := unhex PK.a1Ad
  check t "m9b: an assertion's authData (no attested credential) parses with its count" <|
    match Auth.Passkey.parseAuthData assertAd with
    | .ok a => a.signCount == 5 && a.up && a.attested.isNone
    | .error _ => false
  check t "m9b: a COSE key must be EC2, ES256 and P-256 with 32-byte coordinates" <|
    (match Auth.Passkey.parseCose cose with | .ok k => k.x == x && k.y == y | .error _ => false) &&
      isErr (Auth.Passkey.parseCose (coseKey x y (alg := -257))) && isErr (Auth.Passkey.parseCose (coseKey x y (crv := 2))) &&
      isErr (Auth.Passkey.parseCose (coseKey (x.extract 0 31) y))
  check t "m9b: the sign count must grow unless it stays 0" <|
    Auth.Passkey.countOk 0 0 && Auth.Passkey.countOk 0 1 && Auth.Passkey.countOk 5 6 &&
      !Auth.Passkey.countOk 5 5 && !Auth.Passkey.countOk 5 3 && !Auth.Passkey.countOk 5 0

  -- an account container behind the Worker: register, then log in
  let cfg : Config := { accountId := "acct1", sessionSecret := "k1", publicOrigin := "https://mail.test",
                        internalSecret := "s3cret", mailFrom := "no-reply@mail.test" }
  let db ← Db.openMemory
  discard <| Schema.migrate db
  let (_, worker) ← recorder
  let st : State := { config := cfg, db, store := ← Store.memory, worker, startedAt := 0, schemaVersion := 8, fts5 := true }
  let routes := Routes.all cfg
  let go (m path : String) (hs : List (String × String) := []) (body : String := "") (q : List (String × String) := []) :=
    Router.dispatch st routes { method := m, path := (path.splitOn "/").filter (· ≠ ""), query := q,
                                headers := [("x-account-id", "acct1")] ++ hs, body := body.toUTF8 }
  discard <| go "POST" "/internal/provision" [("x-internal-secret", "s3cret")] "{\"email\":\"alex@example.test\",\"display_name\":\"Alex\"}"
  let cookie := (← Auth.Session.create st "test").getD ""
  let sess := [("cookie", s!"lm_session={cookie}"), ("origin", "https://mail.test"), ("x-requested-with", "LeanMail")]
  let unauth ← go "POST" "/api/passkeys/options" [("origin", "https://mail.test"), ("x-requested-with", "LeanMail")]
  let opt ← go "POST" "/api/passkeys/options" sess
  let oj := jsonOf opt
  let ch1 := jS oj "challenge"
  let params := (jat oj ["pubKeyCredParams"]).getArr?.toOption.getD #[]
  let stored ← db.queryOne "SELECT kind, expires_at FROM auth_challenges WHERE challenge = ?" #[Core.Base64.decode ch1]
  check t "m9b: registration options carry a stored 32-byte challenge, the rp and user, ES256, no attestation, a resident key" <|
    unauth.status == 401 && opt.status == 200 && (Core.Base64.decode ch1).size == 32 &&
      (stored.map (·.text "kind")) == some "register" &&
      jS (jat oj ["rp"]) "id" == "mail.test" && jS (jat oj ["rp"]) "name" == "LeanMail" &&
      jS (jat oj ["user"]) "id" == b64u "acct1".toUTF8 && jS (jat oj ["user"]) "name" == "alex@example.test" &&
      jS (jat oj ["user"]) "displayName" == "Alex" && params.size == 1 &&
      (params[0]!.getObjValAs? Int "alg").toOption == some (-7) && jS oj "attestation" == "none" &&
      jS (jat oj ["authenticatorSelection"]) "residentKey" == "required" &&
      jS (jat oj ["authenticatorSelection"]) "userVerification" == "preferred" &&
      (jat oj ["excludeCredentials"]).getArr?.toOption.map (·.size) == some 0
  let cdjOf (ch origin : String) (ty : String := "webauthn.create") : ByteArray :=
    (Lean.Json.mkObj [("type", .str ty), ("challenge", .str ch), ("origin", .str origin), ("crossOrigin", .bool false)]).compress.toUTF8
  let attOf (adB : ByteArray) (fmt : String := "none") (stmt : Array (Core.Cbor.Value × Core.Cbor.Value) := #[]) : ByteArray :=
    Core.Cbor.encode (.map #[(.text "fmt", .text fmt), (.text "attStmt", .map stmt), (.text "authData", .bytes adB)])
  let regBody (rawId cdj attB : ByteArray) (name : String := "") : String :=
    (Lean.Json.mkObj [("id", .str (b64u rawId)), ("rawId", .str (b64u rawId)), ("type", .str "public-key"),
      ("response", Lean.Json.mkObj [("clientDataJSON", .str (b64u cdj)), ("attestationObject", .str (b64u attB))]),
      ("transports", Lean.Json.arr #[.str "internal", .str "hybrid", .str "bogus"]), ("name", .str name)]).compress
  let reg (body : String) := go "POST" "/api/passkeys" sess body
  let adA := attestedAd "mail.test" 0x4d 0 credA cose
  let wrongOrigin ← reg (regBody credA (cdjOf ch1 "https://evil.test") (attOf adA))
  let wrongType ← reg (regBody credA (cdjOf ch1 "https://mail.test" "webauthn.get") (attOf adA))
  check t "m9b: registration refuses another origin and a get assertion's client data" <|
    wrongOrigin.status == 400 && wrongType.status == 400
  let r1 ← reg (regBody credA (cdjOf ch1 "https://mail.test") (attOf adA) "  Laptop  ")
  let rowA ← db.queryOne "SELECT * FROM passkeys WHERE credential_id = ?" #[credA]
  check t "m9b: a none-attestation registration is stored with its COSE key, name, transports and backup flag" <|
    r1.status == 201 && jget r1 "name" == "Laptop" &&
      (rowA.map (·.blob "public_key_cose")) == some cose && (rowA.bind fun r => (r.get "alg").toInt?.map Int64.toInt) == some (-7) &&
      (rowA.map (·.text "transports")) == some "[\"internal\",\"hybrid\"]" && (rowA.map (·.bool "backup_eligible")) == some true &&
      (rowA.map (·.nat "sign_count")) == some 0
  let again ← reg (regBody credA (cdjOf ch1 "https://mail.test") (attOf adA))
  check t "m9b: a registration challenge works once" (again.status == 400)
  let opt2 ← go "POST" "/api/passkeys/options" sess
  let ch2 := jS (jsonOf opt2) "challenge"
  let ok2 := cdjOf ch2 "https://mail.test"
  let adB := attestedAd "mail.test" 0x45 0 credB cose
  let packed ← reg (regBody credB ok2 (attOf adB "packed"))
  let stmt ← reg (regBody credB ok2 (attOf adB "none" #[(.text "sig", .bytes (bytesOf [1]))]))
  let rawDiff ← reg (regBody credA ok2 (attOf adB))
  let otherRp ← reg (regBody credB ok2 (attOf (attestedAd "evil.test" 0x45 0 credB cose)))
  let noUp ← reg (regBody credB ok2 (attOf (attestedAd "mail.test" 0x44 0 credB cose)))
  let rs256 ← reg (regBody credB ok2 (attOf (attestedAd "mail.test" 0x45 0 credB (coseKey x y (alg := -257)))))
  let unknownCh ← reg (regBody credB (cdjOf (b64u (bytesOf (List.replicate 32 1))) "https://mail.test") (attOf adB))
  check t "m9b: registration refuses other formats, an attStmt, rawId ≠ credential id, another rp id, no UP, RS256 and an unknown challenge" <|
    packed.status == 400 && stmt.status == 400 && rawDiff.status == 400 && otherRp.status == 400 && noUp.status == 400 &&
      rs256.status == 400 && unknownCh.status == 400
  check t "m9b: the options exclude the credentials already registered" <|
    ((jat (jsonOf opt2) ["excludeCredentials"]).getArr?.toOption.getD #[]).map (jS · "id") == #[b64u credA]
  let r2 ← reg (regBody credB ok2 (attOf adB) "Phone")
  let opt3 ← go "POST" "/api/passkeys/options" sess
  let dup ← reg (regBody credA (cdjOf (jS (jsonOf opt3) "challenge") "https://mail.test") (attOf adA))
  check t "m9b: a second credential registers; the same credential twice is a 409" (r2.status == 201 && dup.status == 409)

  -- login, with the Worker's voucher for each fixed challenge
  let loginBody (cred : ByteArray) (cdj ad sig : ByteArray) (uh : String := b64u "acct1".toUTF8) : String :=
    (Lean.Json.mkObj [("id", .str (b64u cred)), ("rawId", .str (b64u cred)), ("type", .str "public-key"),
      ("response", Lean.Json.mkObj [("clientDataJSON", .str (b64u cdj)), ("authenticatorData", .str (b64u ad)),
        ("signature", .str (b64u sig)), ("userHandle", .str uh)])]).compress
  let vec (cdj ad sig : String) : ByteArray × ByteArray × ByteArray := (unhex cdj, unhex ad, unhex sig)
  let login (cred : ByteArray) (v : ByteArray × ByteArray × ByteArray) (tok : String) (uh : String := b64u "acct1".toUTF8)
      (voucher : Option String := none) :=
    go "POST" "/auth/passkey/login" [("x-passkey-challenge-ok", voucher.getD (Auth.Passkey.vouch cfg tok)), ("user-agent", "passkey-test")]
      (loginBody cred v.1 v.2.1 v.2.2 uh)
  let a1 := vec PK.a1Cdj PK.a1Ad PK.a1Sig
  let noVoucher ← go "POST" "/auth/passkey/login" [] (loginBody credA a1.1 a1.2.1 a1.2.2)
  let otherVoucher ← login credA a1 PK.a1Tok (voucher := some (Auth.Passkey.vouch cfg PK.a2Tok))
  let otherKey ← login credA a1 PK.a1Tok (voucher := some (Auth.Passkey.vouch { cfg with sessionSecret := "k2" } PK.a1Tok))
  let otherHandle ← login credA a1 PK.a1Tok (uh := b64u "acct2".toUTF8)
  check t "m9b: a login needs the Worker's voucher for this very challenge under this account's key, and this account's user handle" <|
    noVoucher.status == 401 && otherVoucher.status == 401 && otherKey.status == 401 && otherHandle.status == 401
  check t "m9b: the voucher is base64url HMAC-SHA256 of \"passkey:\" + token under the account's secret" <|
    Auth.Passkey.vouch cfg PK.a1Tok == Crypto.base64Url (Crypto.hmacText "k1" ("passkey:" ++ PK.a1Tok))
  let ok1 ← login credA a1 PK.a1Tok
  let rowA1 ← db.queryOne "SELECT sign_count, last_used_at FROM passkeys WHERE credential_id = ?" #[credA]
  let newCookie := (setCookie? ok1).getD ""
  let me ← go "GET" "/api/passkeys" [("cookie", s!"lm_session={newCookie}")]
  let sessRow ← db.queryOne "SELECT ua FROM sessions WHERE token_hash = ?" #[Auth.Tokens.hash newCookie]
  check t "m9b: a valid assertion starts a session (cookie set), records the count and the time" <|
    ok1.status == 200 && jB (jsonOf ok1) "ok" && !newCookie.isEmpty && me.status == 200 &&
      (sessRow.map (·.text "ua")) == some "passkey-test" &&
      (rowA1.map (·.nat "sign_count")) == some 5 && (rowA1.bind (·.nat? "last_used_at")).isSome
  let a2 ← login credA (vec PK.a2Cdj PK.a2Ad PK.a2Sig) PK.a2Tok
  check t "m9b: a sign count that went backwards is refused" (a2.status == 401)
  let a3 ← login credA (vec PK.a3Cdj PK.a3Ad PK.a3Sig) PK.a3Tok
  check t "m9b: a correctly signed assertion for another origin is refused" (a3.status == 401)
  let a5 ← login credA (vec PK.a5Cdj PK.a5Ad PK.a5Sig) PK.a5Tok
  check t "m9b: a correctly signed assertion for another rp id is refused" (a5.status == 401)
  let a6 ← login credA (vec PK.a6Cdj PK.a6Ad PK.a6Sig) PK.a6Tok
  check t "m9b: an expired challenge is refused even with the Worker's voucher" (a6.status == 401)
  let a4v := vec PK.a4Cdj PK.a4Ad PK.a4Sig
  let flipped ← login credA (a4v.1, a4v.2.1, (a4v.2.2).set! 20 ((a4v.2.2)[20]! ^^^ 1)) PK.a4Tok
  let a4 ← login credA a4v PK.a4Tok
  let a4again ← login credA a4v PK.a4Tok
  check t "m9b: a tampered signature is refused without spending the challenge; the genuine one then works, once" <|
    flipped.status == 401 && a4.status == 200 && a4again.status == 401
  let b1v := vec PK.b1Cdj PK.b1Ad PK.b1Sig
  let b1 ← login credB b1v PK.b1Tok
  let b1again ← login credB b1v PK.b1Tok
  check t "m9b: a credential that never counts (0 and 0) logs in, and its assertion cannot be replayed" <|
    b1.status == 200 && b1again.status == 401
  let unknownCred ← login (credA.set! 0 (credA[0]! ^^^ 1)) b1v PK.b1Tok
  check t "m9b: an unknown credential is refused" (unknownCred.status == 401)
  let lst ← go "GET" "/api/passkeys" sess
  let items := jItems (jsonOf lst)
  check t "m9b: GET /api/passkeys lists both with names, transports and last use" <|
    lst.status == 200 && items.size == 2 && (items.map (jS · "name")).toList.mergeSort == ["Laptop", "Phone"] &&
      items.all (fun i => !(jat i ["last_used_at"]).isNull)

  -- the pages
  let signin ← go "GET" "/signin" [("accept", "text/html")]
  let closedSt := { st with config := { cfg with signupMode := "closed" } }
  let signinClosed ← Router.dispatch closedSt (Routes.all closedSt.config) { method := "GET", path := ["signin"], headers := [("x-account-id", "acct1")] }
  let sbody := signin.bodyText
  let before (sub : String) : Nat := ((sbody.splitOn sub).headD "").length
  check t "m9b: /signin has the passkey button above the email form and links to /signup unless sign-ups are closed" <|
    signin.status == 200 && has sbody "passkey-signin-btn" && before "passkey-signin-btn" < before "email-form" &&
      has sbody "href=\"/signup\"" && !has signinClosed.bodyText "href=\"/signup\""
  let settingsR ← go "GET" "/settings" [("cookie", s!"lm_session={cookie}"), ("accept", "text/html")]
  let some rowB ← db.queryOne "SELECT id FROM passkeys WHERE credential_id = ?" #[credB] | check t "m9b: credB stored" false
  let bid := rowB.text "id"
  check t "m9b: /settings lists the passkeys with a remove form each and an add button" <|
    settingsR.status == 200 && has settingsR.bodyText "id=\"passkeys\"" && has settingsR.bodyText "Add a passkey" &&
      has settingsR.bodyText s!"/settings/passkeys/{bid}/remove" && has settingsR.bodyText "Laptop" && has settingsR.bodyText "Synced"
  let rm ← go "POST" s!"/settings/passkeys/{bid}/remove" [("cookie", s!"lm_session={cookie}"), ("origin", "https://mail.test"),
    ("content-type", "application/x-www-form-urlencoded")]
  let rmCsrf ← go "POST" s!"/settings/passkeys/{bid}/remove" [("cookie", s!"lm_session={cookie}"), ("origin", "https://evil.test")]
  let goneB ← db.queryOne "SELECT id FROM passkeys WHERE id = ?" #[bid]
  let b1gone ← login credB b1v PK.b1Tok
  check t "m9b: removing a passkey from Settings (CSRF-checked) deletes it, and it no longer signs in" <|
    rmCsrf.status == 403 && rm.status == 303 && (rm.headers.lookup "Location") == some "/settings?saved=passkey" && goneB.isNone &&
      b1gone.status == 401
  let some rowA2 ← db.queryOne "SELECT id FROM passkeys WHERE credential_id = ?" #[credA] | check t "m9b: credA stored" false
  let del ← go "DELETE" s!"/api/passkeys/{rowA2.text "id"}" sess
  let del2 ← go "DELETE" s!"/api/passkeys/{rowA2.text "id"}" sess
  check t "m9b: DELETE /api/passkeys/:id removes one, then 404s" (del.status == 200 && del2.status == 404)

def m9bSignup (t : IO.Ref Tally) : IO Unit := do
  let cfg : Config := { accountId := "_operator", role := "operator", sessionSecret := "k1", publicOrigin := "https://mail.test",
                        internalSecret := "s3cret", mailFrom := "no-reply@mail.test", signupMode := "approve" }
  let db ← Db.openMemory
  discard <| Schema.migrate db
  let (calls, worker) ← recorder
  let st : State := { config := cfg, db, store := ← Store.memory, worker, startedAt := 0, schemaVersion := 8, fts5 := true }
  let goWith (s : State) (m path : String) (hs : List (String × String) := []) (body : String := "") (q : List (String × String) := []) :=
    let req : Router.Req := { method := m, path := (path.splitOn "/").filter (· ≠ ""), query := q,
                              headers := [("x-account-id", s.config.accountId)] ++ hs, body := body.toUTF8 }
    Router.dispatch s (Routes.all s.config) req
  let go (m path : String) (hs : List (String × String) := []) (body : String := "") (q : List (String × String) := []) :=
    goWith st m path hs body q
  let page ← go "GET" "/signup" [("accept", "text/html")]
  let sent ← go "GET" "/signup" [("accept", "text/html")] (q := [("sent", "1")])
  let bad ← go "GET" "/signup" [("accept", "text/html")] (q := [("error", "email")])
  check t "m9b: /signup asks for an email and a note, posting to the Worker's /auth/signup; ?sent=1 thanks, ?error explains" <|
    page.status == 200 && has page.bodyText "id=\"signup-form\"" && has page.bodyText "action=\"/auth/signup\"" &&
      has page.bodyText "name=\"note\"" && has page.bodyText "href=\"/signin\"" &&
      sent.status == 200 && has sent.bodyText "Thanks" && has sent.bodyText "approves each sign-up" && !has sent.bodyText "signup-form" &&
      has bad.bodyText "does not look like an email"
  let closed := { st with config := { cfg with signupMode := "closed" } }
  let cp ← goWith closed "GET" "/signup" [("accept", "text/html")]
  check t "m9b: with SIGNUP_MODE=closed the page says so and has no form" <|
    cp.status == 200 && has cp.bodyText "Sign-ups are closed" && !has cp.bodyText "signup-form"
  let form := [("content-type", "application/x-www-form-urlencoded"), ("origin", "https://mail.test"), ("cf-connecting-ip", "198.51.100.4")]
  let p1 ← go "POST" "/signup" form "email=New%40Example.test&note=hi+there"
  let row ← db.queryOne "SELECT email, note FROM signups"
  let p2 ← go "POST" "/signup" form "email=nope"
  let p3 ← go "POST" "/signup" [("content-type", "application/x-www-form-urlencoded"), ("origin", "https://evil.test")] "email=a%40b.test"
  check t "m9b: the sign-up form posted to the operator queues the sign-up and redirects to the thanks page" <|
    p1.status == 303 && (p1.headers.lookup "Location") == some "/signup?sent=1" &&
      (row.map (·.text "email")) == some "new@example.test" && (row.map (·.text "note")) == some "hi there" &&
      p2.status == 303 && (p2.headers.lookup "Location") == some "/signup?error=email" && p3.status == 403 &&
      (← calls.get).isEmpty
  let acctSt := { st with config := { cfg with role := "account", accountId := "acct1" } }
  let pa ← goWith acctSt "POST" "/signup" form "email=x%40example.test"
  check t "m9b: an account container takes no sign-ups" (pa.status == 404)

def m9bChecks (t : IO.Ref Tally) : IO Unit := do
  let flip (b : ByteArray) (i : Nat) : ByteArray := b.set! i (b[i]! ^^^ 1)
  check t "m9b: p256Verify accepts the RFC 6979 A.2.5 P-256/SHA-256 signature of 'sample'" <|
    Crypto.p256Verify (unhex PK.rfcX) (unhex PK.rfcY) "sample".toUTF8 (unhex PK.rfcSig)
  check t "m9b: p256Verify refuses another message, a flipped signature byte, another key and malformed input" <|
    !Crypto.p256Verify (unhex PK.rfcX) (unhex PK.rfcY) "test".toUTF8 (unhex PK.rfcSig) &&
    !Crypto.p256Verify (unhex PK.rfcX) (unhex PK.rfcY) "sample".toUTF8 (flip (unhex PK.rfcSig) 10) &&
    !Crypto.p256Verify (unhex PK.pkX) (unhex PK.pkY) "sample".toUTF8 (unhex PK.rfcSig) &&
    !Crypto.p256Verify (unhex PK.rfcY) (unhex PK.rfcX) "sample".toUTF8 (unhex PK.rfcSig) &&
    !Crypto.p256Verify ((unhex PK.rfcX).extract 0 31) (unhex PK.rfcY) "sample".toUTF8 (unhex PK.rfcSig) &&
    !Crypto.p256Verify (unhex PK.rfcX) (unhex PK.rfcY) "sample".toUTF8 ByteArray.empty
  check t "m9b: p256Verify accepts a node-made WebAuthn assertion signature" <|
    Crypto.p256Verify (unhex PK.pkX) (unhex PK.pkY) (unhex PK.a1Ad ++ Crypto.sha256 (unhex PK.a1Cdj)) (unhex PK.a1Sig)
  m9bPasskeys t
  m9bSignup t

def main (_ : List String) : IO UInt32 := do
  let t ← IO.mkRef ({} : Tally)
  -- crypto vectors (FIPS 180-2 and RFC 4231)
  check t "sha256 of 'abc' matches the FIPS vector" <|
    Crypto.sha256Hex "abc".toUTF8 == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
  check t "hmac-sha256 matches RFC 4231 test case 2" <|
    Crypto.toHex (Crypto.hmacSha256 "Jefe".toUTF8 "what do ya want for nothing?".toUTF8)
      == "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
  check t "base64url encodes without padding" <|
    Crypto.base64Url "hello".toUTF8 == "aGVsbG8" && Crypto.base64Url "hi".toUTF8 == "aGk"
  let r1 ← Crypto.randomBytes 32
  let r2 ← Crypto.randomBytes 32
  check t "random bytes are 32 long and differ between calls" (r1.size == 32 && r1 != r2)
  check t "constant-time equality agrees with structural equality" <|
    Crypto.constantTimeEq "abc".toUTF8 "abc".toUTF8 && !Crypto.constantTimeEq "abc".toUTF8 "abd".toUTF8

  -- sqlite
  let db ← Db.openMemory
  check t "sqlite reports its version" (Db.version.startsWith "3.")
  let v ← Schema.migrate db
  check t "the schema migrates to the current version" (v == Schema.migrations.back!.1)
  check t "migrating again is a no-op" ((← Schema.migrate db) == v)
  check t "fts5 is compiled in" (← Schema.fts5Works db)
  db.exec "INSERT INTO settings(key, value, updated_at) VALUES (?, ?, ?)" #["k", "v", (5 : Nat)]
  let rows ← db.query "SELECT key, value, updated_at FROM settings WHERE key = ?" #["k"]
  check t "a bound insert reads back by name" <|
    rows.size == 1 && rows[0]!.text "value" == "v" && rows[0]!.nat "updated_at" == 5
  db.exec "INSERT INTO fts_src(rowid, from_, subject, body) VALUES (1, 'alex@example.com alex example.com', 'Lean mail', 'the body text')"
  db.exec "INSERT INTO fts(rowid, from_, to_, cc, subject, body, list_id, attachments) SELECT rowid, from_, to_, cc, subject, body, list_id, attachments FROM fts_src WHERE rowid = 1"
  let hits ← db.query "SELECT rowid FROM fts WHERE fts MATCH ?" #["from_:\"example.com\""]
  check t "fts5 matches a quoted address domain with the custom tokenizer" (hits.size == 1)
  let unquoted ← (do discard <| db.query "SELECT rowid FROM fts WHERE fts MATCH ?" #["from_:example.com"]; pure true) <|> pure false
  check t "fts5 rejects an unquoted term containing a dot, so the query builder must quote" (!unquoted)
  let hits2 ← db.query "SELECT rowid FROM fts WHERE fts MATCH ?" #["subject:lean"]
  check t "fts5 matches a subject word case-insensitively" (hits2.size == 1)
  let blob := ByteArray.mk #[0, 1, 2, 255]
  db.exec "INSERT INTO duplicates(dedupe_key, source, received_at) VALUES (?, 'route', 1)" #[blob]
  let d ← db.query "SELECT dedupe_key FROM duplicates"
  check t "a blob round-trips byte for byte" (d[0]!.blob "dedupe_key" == blob)
  let failed ← (do
    db.transaction do
      db.execUnlocked "INSERT INTO settings(key, value, updated_at) VALUES ('t', '1', 1)"
      throw (IO.userError "boom")
    pure false) <|> pure true
  let after ← db.query "SELECT key FROM settings WHERE key = 't'"
  check t "a failed transaction rolls back" (failed && after.size == 0)

  -- the rest of the server: store, SigV4, query grammar, ingest, spool, search, API
  serverChecks t
  authChecks t
  pageChecks t
  sendChecks t
  spoolChecks t
  gmailChecks t
  m7Checks t
  m6bChecks t
  m9bChecks t
  let r ← t.get
  IO.println s!"{r.passed} passed, {r.failed} failed"
  return if r.failed == 0 then 0 else 1
