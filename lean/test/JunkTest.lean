import Core.Junk
import Core.Mime
import Lean.Data.Json

/-! Junk check tests: every case of `junkcheck-vectors.json` (MailVault's `scripts/junkcheck-vectors.ts`
over `shared/junkcheck.js`) must give the same verdict, weights and reasons in the same order, with
the same words; then each reason code once more over the eml fixtures. One line per check, exit 1
on any failure. -/

open Core.Junk
open Lean (Json)

structure Tally where
  passed : Nat := 0
  failed : Nat := 0

def ok (t : IO.Ref Tally) (name : String) : IO Unit := do
  IO.println s!"ok   {name}"
  t.modify fun x => { x with passed := x.passed + 1 }

def fail (t : IO.Ref Tally) (name why : String) : IO Unit := do
  IO.println s!"FAIL {name}: {why}"
  t.modify fun x => { x with failed := x.failed + 1 }

def checkB (t : IO.Ref Tally) (name : String) (b : Bool) (why : String := "") : IO Unit :=
  if b then ok t name else fail t name why

/-! ## Reading a vector -/

def str (j : Json) (k : String) : String := ((j.getObjValAs? String k).toOption).getD ""
def arr (j : Json) (k : String) : Array Json := ((j.getObjValAs? (Array Json) k).toOption).getD #[]
def val (j : Json) (k : String) : Json := ((j.getObjVal? k).toOption).getD .null

def addrOfJson (j : Json) : Addr := { name := str j "name", address := str j "address" }

def inputOfJson (j : Json) (ctx : Json) : Input :=
  let headers := match val j "headers" with
    | .null => none
    | h => some ((h.getArr?.toOption.getD #[]).map fun x => (str x "key", str x "value"))
  let auth := match val j "auth" with
    | .null => none
    | a => some { spf := str a "spf", dkim := str a "dkim", dmarc := str a "dmarc" }
  let from? := match val j "from" with
    | .null => none
    | f => some (addrOfJson f)
  let html := match val j "html" with
    | .str s => some s
    | _ => none
  { headers, auth, from?
    replyTo := (arr j "reply_to").toList.map addrOfJson
    subject := str j "subject", text := str j "text", html
    attachments := (arr j "attachments").toList.map fun a => { name := str a "name", mime := str a "mime" }
    listId := str j "list_id", listUnsubscribe := str j "list_unsubscribe"
    labels := (arr j "labels").toList.filterMap (·.getStr?.toOption)
    ownDomains := (arr ctx "own_domains").toList.filterMap (·.getStr?.toOption) }

def showDetail (d : List (String × String)) : String :=
  "{" ++ ", ".intercalate (d.map fun (k, v) => s!"{k}: {v.quote}") ++ "}"

def showReason (r : Reason) : String := s!"{r.code}/{r.family.toString}/{r.weight} {showDetail r.detail}"

/-- The detail object as (key, value) pairs; the JSON reader does not keep key order, so details are
compared as maps. -/
def detailOfJson (j : Json) : List (String × String) :=
  match j with
  | .obj kvs => kvs.toArray.toList.map fun ⟨k, v⟩ => (k, v.getStr?.toOption.getD "")
  | _ => []

def sameDetail (a b : List (String × String)) : Bool :=
  a.length == b.length && a.all fun (k, v) => b.lookup k == some v

/-- The first difference between the expected result and ours. -/
def resultDiff (exp : Json) (expWords : Array Json) (got : Verdict) : Option String := Id.run do
  let n (k : String) : Nat := ((exp.getObjValAs? Nat k).toOption).getD 999
  if n "v" != got.v then return some s!"v expected {n "v"} got {got.v}"
  if str exp "verdict" != got.verdict.toString then
    return some s!"verdict expected {str exp "verdict"} got {got.verdict.toString} ({", ".intercalate (got.reasons.map showReason)})"
  if n "phish" != got.phish || n "spam" != got.spam then
    return some s!"weights expected phish {n "phish"} spam {n "spam"} got phish {got.phish} spam {got.spam}"
  let rs := arr exp "reasons"
  let gotCodes := got.reasons.map (·.code)
  let expCodes := rs.toList.map (str · "code")
  if expCodes != gotCodes then return some s!"reasons expected {expCodes} got {gotCodes}"
  for (e, g) in rs.toList.zip got.reasons do
    let ew := ((e.getObjValAs? Nat "weight").toOption).getD 999
    if str e "family" != g.family.toString || ew != g.weight then
      return some s!"{g.code} expected {str e "family"}/{ew} got {g.family.toString}/{g.weight}"
    let ed := detailOfJson (val e "detail")
    if !sameDetail ed g.detail then
      return some s!"{g.code} detail expected {showDetail ed} got {showDetail g.detail}"
  for (e, g) in expWords.toList.zip got.reasons do
    let w := e.getStr?.toOption.getD ""
    if w != reasonText g then return some s!"{g.code} words expected {w.quote} got {(reasonText g).quote}"
  if expWords.size != got.reasons.length then return some s!"words expected {expWords.size} got {got.reasons.length}"
  return none

def fixture (rel : String) : IO String := do
  if ← System.FilePath.pathExists rel then IO.FS.readFile rel else IO.FS.readFile ("lean/" ++ rel)

def runCases (t : IO.Ref Tally) (cases : Array Json) : IO Unit := do
  for c in cases do
    let name := str c "name"
    let got := check (inputOfJson (val c "input") (val c "ctx"))
    match resultDiff (val c "result") (arr c "words") got with
    | none => ok t s!"vector {name}: {got.verdict.toString} {got.reasons.map (·.code)}"
    | some d => fail t name d

def vectors (t : IO.Ref Tally) : IO Unit := do
  match Json.parse (← fixture "test/fixtures/junkcheck-vectors.json") with
  | .error e => fail t "junkcheck-vectors.json parses" e
  | .ok j =>
    let nv := ((j.getObjValAs? Nat "v").toOption).getD 0
    checkB t "the vectors are for this check's version" (nv == checkVersion) s!"vectors v{nv}, check v{checkVersion}"
    checkB t "phishing reports go where the JavaScript sends them" (str j "report_to" == phishReportTo)
      s!"expected {str j "report_to"}"
    -- the words: every reason text and verdict text, the same
    let rt := detailOfJson (val j "reason_text")
    checkB t "every reason's text is the JavaScript's" (sameDetail rt reasonTexts)
      s!"{(rt.filter fun (k, v) => reasonTexts.lookup k != some v).map (·.1)} differ, {rt.length} vs {reasonTexts.length}"
    let vt := detailOfJson (val j "verdict_text")
    let kinds := [Kind.clean, .suspicious, .spam, .phishing]
    checkB t "every verdict's text is the JavaScript's"
      (sameDetail vt (kinds.map fun k => (k.toString, verdictText k)))
    for s in arr j "sites" do
      let h := str s "host"
      checkB t s!"site of {h} is {str s "site"}" (siteOf h == str s "site") s!"got {siteOf h}"
    let cases := arr j "cases"
    checkB t "junkcheck-vectors.json has cases" (!cases.isEmpty)
    runCases t cases

/-! ## Each reason over the eml fixtures -/

/-- A fixture with its CRLF line ends made LF, so edits can name lines plainly. -/
def eml (n : String) : IO String := return (← fixture s!"test/fixtures/eml/{n}.eml").replace "\r\n" "\n"

def bytes (s : String) : ByteArray := s.toUTF8

/-- The message with `extra` header lines on top. -/
def withHeaders (raw : String) (extra : List String) : String := String.join (extra.map (· ++ "\n")) ++ raw

/-- The message with every `from` replaced by `to`. -/
def edit (raw : String) (from_ to_ : String) : String := raw.replace from_ to_

def own : List String := ["wuli.nu"]

def run (raw : String) (labels : List String := []) : Verdict :=
  check (inputOfMessage (Core.Mime.parse (bytes raw)) own labels)

def expect (t : IO.Ref Tally) (name : String) (raw : String) (kind : Kind) (codes : List String)
    (labels : List String := []) (detail : List (String × String) := []) : IO Unit := do
  let got := run raw labels
  let gotCodes := got.reasons.map (·.code)
  let firstDetail := (got.reasons.head?.map (·.detail)).getD []
  if got.verdict == kind && gotCodes == codes && detail.all (fun (k, v) => firstDetail.lookup k == some v) then
    ok t name
  else
    fail t name s!"expected {kind.toString} {codes} {showDetail detail} got {got.verdict.toString} {", ".intercalate (got.reasons.map showReason)}"

/-- An HTML body in place of 10.eml's plain one. -/
def html10 (raw body : String) : String :=
  let head := (raw.splitOn "\n\n").head!
  edit head "Content-Type: text/plain; charset=utf-8" "Content-Type: text/html; charset=utf-8" ++ "\n\n" ++ body ++ "\n"

def emlChecks (t : IO.Ref Tally) : IO Unit := do
  let e01 ← eml "01"; let e02 ← eml "02"; let e04 ← eml "04"; let e09 ← eml "09"
  let e10 ← eml "10"; let e11 ← eml "11"; let e13 ← eml "13"
  -- the fixtures as they are
  for n in ["01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "14b"] do
    expect t s!"eml {n} as it came (CRLF) is clean" (← fixture s!"test/fixtures/eml/{n}.eml") .clean []
  expect t "eml 11 (a list with no List-Unsubscribe) is bulk mail with no way out" e11 .clean ["bulk_no_unsubscribe"]
  let input02 := inputOfMessage (Core.Mime.parse (bytes e02)) own
  checkB t "inputOfMessage reads 02's sender, subject, unsubscribe and header block"
    (input02.from? == some { name := "Capital One", address := "no-reply@notification.capitalone.example" } &&
      input02.subject == "Your statement is ready" && input02.listUnsubscribe == "<mailto:unsub@example.test>" &&
      ((input02.headers.getD #[]).find? (·.1 == "precedence")).map (·.2) == some "bulk")
  let input13 := inputOfMessage (Core.Mime.parse (bytes e13)) own
  checkB t "inputOfMessage reads 13's attachment name and type"
    (input13.attachments == [{ name := "invoice-2026-0912.pdf", mime := "application/pdf" }])
  -- authentication: the topmost Authentication-Results only
  expect t "02 with DMARC, SPF and DKIM failed at the receiver is phishing"
    (withHeaders e02 ["Authentication-Results: mx.cloudflare.net; dkim=fail; spf=fail smtp.mailfrom=x; dmarc=fail"])
    .phishing ["dmarc_fail", "spf_fail", "dkim_fail"] (detail := [("domain", "notification.capitalone.example")])
  expect t "02 with a forged pass below the receiver's fail still fails DMARC"
    (withHeaders e02 ["Authentication-Results: mx.cloudflare.net; dmarc=fail; spf=none",
      "Authentication-Results: evil; dmarc=pass; spf=pass; dkim=pass"]) .suspicious ["dmarc_fail"]
  expect t "02 passing at the receiver is clean"
    (withHeaders e02 ["ARC-Authentication-Results: i=1; mx.cloudflare.net; dkim=pass; spf=pass; dmarc=pass"]) .clean []
  -- own domain
  expect t "09 from our own domain with nothing passing is phishing"
    (withHeaders (edit e09 "From: Ben Okafor <ben@okafor.example>" "From: Alex <alex@wuli.nu>")
      ["Authentication-Results: mx.cloudflare.net; dkim=none; spf=softfail; dmarc=none"])
    .phishing ["own_domain_unverified"] (detail := [("domain", "wuli.nu")])
  expect t "09 from our own domain with no Authentication-Results is clean"
    (edit e09 "From: Ben Okafor <ben@okafor.example>" "From: Alex <alex@wuli.nu>") .clean []
  -- the sender's name
  expect t "04 whose name shows another address (and a brand) is phishing"
    (edit e04 "From: Hyperliquid <alerts@hyperliquid.example>" "From: \"support@paypal.com\" <x7@mailer.evil.example>")
    .phishing ["name_has_other_address", "brand_mismatch"] (detail := [("shown", "support@paypal.com"), ("actual", "x7@mailer.evil.example")])
  expect t "04 whose name is a brand it is not from is suspicious"
    (edit e04 "From: Hyperliquid <alerts@hyperliquid.example>" "From: PayPal Security <security@pp-alerts.example>")
    .suspicious ["brand_mismatch"] (detail := [("brand", "PayPal Security"), ("domain", "pp-alerts.example")])
  expect t "10 asking to verify a Coinbase account from kraken.example names a brand in the subject"
    (edit e10 "Subject: Sign-in from a new device" "Subject: Coinbase: verify your account now")
    .suspicious ["brand_in_subject"] (detail := [("brand", "Coinbase"), ("domain", "kraken.example")])
  expect t "10 forwarded with that subject is not caught by the brand rule"
    (edit e10 "Subject: Sign-in from a new device" "Subject: Fwd: Coinbase: verify your account now") .clean []
  -- the sender's domain
  expect t "04 from paypa1.com is a look-alike (swapped characters)"
    (edit e04 "alerts@hyperliquid.example" "alerts@paypa1.com")
    .phishing ["lookalike_domain"] (detail := [("domain", "paypa1.com"), ("like", "paypal.com")])
  expect t "04 from wu1i.nu looks like our own domain"
    (edit e04 "alerts@hyperliquid.example" "alerts@wu1i.nu") .phishing ["lookalike_domain"]
  expect t "04 from amazoon.com is one letter from a brand"
    (edit e04 "alerts@hyperliquid.example" "alerts@amazoon.com") .suspicious ["lookalike_domain"]
  expect t "04 from a punycode domain is written in look-alike characters"
    (edit e04 "alerts@hyperliquid.example" "alerts@xn--pple-43d.com") .suspicious ["punycode_sender"]
  -- where replies go
  expect t "01 with Reply-To elsewhere says so"
    (withHeaders e01 ["Reply-To: Claims <claims@other.example>"]) .clean ["reply_to_elsewhere"]
    (detail := [("reply", "claims@other.example"), ("domain", "nordic-press.example")])
  expect t "01 with Reply-To on its own site is clean"
    (withHeaders e01 ["Reply-To: maya@mail.nordic-press.example"]) .clean []
  -- links
  expect t "10 with a link that shows paypal.com and goes elsewhere is a mismatch"
    (html10 e10 "<p>Sign in at <a href=\"https://login.evil.example/p\">https://www.paypal.com/signin</a></p>")
    .suspicious ["link_text_mismatch"] (detail := [("shown", "www.paypal.com"), ("actual", "login.evil.example")])
  expect t "10 with a link hiding behind userinfo to a bare address"
    (edit e10 "secure your account." "secure it at https://paypal.com@203.0.113.9/login now.")
    .phishing ["link_userinfo", "link_to_ip"] (detail := [("shown", "paypal.com"), ("actual", "203.0.113.9")])
  expect t "10 with a link to an IPv6 literal goes to a bare address"
    (html10 e10 "<A HREF='http://[2001:DB8::1]/x'>Click here</A>") .suspicious ["link_to_ip"]
    (detail := [("actual", "[2001:db8::1]")])
  expect t "10 with a punycode link"
    (html10 e10 "<a href=\"https://xn--80ak6aa92e.com/\">Open</a>") .suspicious ["link_punycode"]
  expect t "10 with a tracked newsletter link is clean"
    (html10 e10 "<a href=\"https://x.us1.list-manage.com/track\">kraken.example/blog</a> and <a href=\"https://f.example/a\">report.pdf</a>") .clean []
  -- attachments
  expect t "13 with its PDF renamed to an .exe runs a program"
    (edit e13 "filename=\"invoice-2026-0912.pdf\"" "filename=\"Invoice.pdf.exe\"") .suspicious ["risky_attachment"]
    (detail := [("name", "Invoice.pdf.exe")])
  expect t "13 with an .htm attachment and a mailbox-full subject is phishing"
    (edit (edit e13 "filename=\"invoice-2026-0912.pdf\"" "filename=\"Restore.HTM\"") "Subject: Invoice 2026-0912" "Subject: Your mailbox is full")
    .phishing ["page_attachment", "credential_lure"] (detail := [("name", "Restore.HTM")])
  -- words
  expect t "10 with unusual sign-in activity and a link is a credential lure"
    (html10 (edit e10 "Subject: Sign-in from a new device" "Subject: Unusual sign-in activity")
      "<p>We noticed unusual sign-in activity. <a href=\"https://secure-check.example/v\">Review</a></p>")
    .suspicious ["credential_lure"] (detail := [("phrase", "unusual sign-in")])
  expect t "10 with unusual sign-in activity and no link is clean"
    (edit e10 "Subject: Sign-in from a new device" "Subject: Unusual sign-in activity") .clean []
  -- spam
  expect t "09 in Gmail's Spam is spam" e09 .spam ["gmail_spam"] (labels := ["SPAM", "UNREAD"])
  expect t "09 flagged by a filter on the way is spam" (withHeaders e09 ["X-Spam-Flag: YES"]) .spam ["upstream_spam"]
  expect t "09 with X-Spam-Status yes is spam" (withHeaders e09 ["X-Spam-Status: Yes, score=9.1"]) .spam ["upstream_spam"]
  expect t "09 shouting about a lottery is spam"
    (edit (edit e09 "Subject: Dinner Saturday?" "Subject: YOU HAVE WON A PRIZE!!!") "We're doing" "Dear friend, claim your prize. We're doing")
    .spam ["spam_phrases", "shouting_subject"] (detail := [("phrase", "you have won"), ("more", " and 2 more")])
  expect t "02 without its List-Unsubscribe (Precedence: bulk) has no way out"
    (edit e02 "List-Unsubscribe: <mailto:unsub@example.test>\n" "") .clean ["bulk_no_unsubscribe"]
  expect t "09 with a shouting subject alone"
    (edit e09 "Subject: Dinner Saturday?" "Subject: DINNER SATURDAY AT EIGHT") .clean ["shouting_subject"]

/-! ## The JavaScript string quirks -/

def quirks (t : IO.Ref Tally) : IO Unit := do
  checkB t "lower-casing İ lengthens the string, as in JavaScript"
    (Js.lower "İX" == "i̇x" && Js.utf16Len (Js.lower "İX") == 3)
  checkB t "the Kelvin sign lower-cases to k" (Js.lower "K" == "k")
  checkB t "&#NNNNN; wraps at 65536 like String.fromCharCode" (Js.decodeEntities "&#65601;" == "A")
  checkB t "&amp;lt; decodes once, to &lt;" (Js.decodeEntities "&amp;lt;" == "&lt;")
  checkB t "a tag never closed stays as text" (Js.stripTags "a <b c" == "a <b c")
  checkB t "data-href counts as href (\\b before href)"
    ((anchorsOf "<a data-href=\"https://x.example/\">t</a>" #[]).map (·.host) == #["x.example"])
  checkB t "a quoted href runs to the next quote, even in the next anchor (as the JavaScript regex does)"
    ((anchorsOf "<a href=\"https://y.example/>t</a> <a href=\"https://z.example/\">u</a>" #[]).map (fun l => (l.host, l.text)) == #[("y.example", "u")])
  checkB t "an unclosed quote falls back to the unquoted value, which is no link"
    ((anchorsOf "<a href=\"https://y.example/>t</a> <a href=https://z.example/>u</a>" #[]).map (·.host) == #["z.example"])
  checkB t "pick reads spf=softfail as softfail" (pick "mx; spf=softfail; dmarc=pass" "spf" == "softfail")
  checkB t "header.d= is not dkim=" (pick "mx; header.d=x; dkim=pass" "dkim" == "pass")

/-- `junk-test` runs everything; `junk-test FILE` runs only the cases of another vector file of the
same shape (`{cases: [{name, input, ctx, result, words}]}`), for checking against the JavaScript. -/
def main (args : List String) : IO UInt32 := do
  let t ← IO.mkRef ({} : Tally)
  if let some path := args.head? then
    match Json.parse (← IO.FS.readFile path) with
    | .error e => fail t s!"{path} parses" e
    | .ok j => runCases t (arr j "cases")
    let r ← t.get
    IO.println s!"junk-test {path}: {r.passed} passed, {r.failed} failed"
    return if r.failed == 0 then 0 else 1
  vectors t
  emlChecks t
  quirks t
  let r ← t.get
  IO.println s!"junk-test: {r.passed} passed, {r.failed} failed"
  return if r.failed == 0 then 0 else 1
