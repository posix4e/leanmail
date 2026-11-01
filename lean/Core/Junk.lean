import Core.Text.Str
import Core.Junk.Js
import Core.Mime

/-! # The junk and phishing check

A port of MailVault's `shared/junkcheck.js` (`CHECK_VERSION` 2), rule for rule and word for word:
a handful of rules, each a reason with a weight, run in a fixed order; the weights give the verdict.
Nothing here is learned or fetched; the rules read only what the message itself carries, so the
same message gets the same answer here, in the PWA and in the native app.
`test/fixtures/junkcheck-vectors.json` is what it must reproduce (`junk-test`).

The input is the JavaScript `CheckInput` plus the `CheckContext`'s `own_domains`;
`inputOfMessage` builds it from a parsed message the way MailVault's `public/junk.js` did. -/

namespace Core.Junk

open Core.Text.Str (isDigit isAlpha isJsWhitespace)

def checkVersion : Nat := 2
/-- Phishing from this much phishing weight, spam from this much spam weight; "suspicious" from this
much of either. -/
def phishAt : Nat := 4
def spamAt : Nat := 3
def suspiciousAt : Nat := 2
/-- Past these the rest is not read (UTF-16 units, as JavaScript counts). -/
def htmlMax : Nat := 512 * 1024
def textMax : Nat := 64 * 1024
def linksMax : Nat := 200

/-! ## Input and output -/

structure Addr where
  name : String := ""
  address : String := ""
  deriving Repr, BEq, Inhabited

/-- The envelope's verdicts, used only when the header block is absent. -/
structure Auth where
  spf : String := ""
  dkim : String := ""
  dmarc : String := ""
  deriving Repr, BEq, Inhabited

structure AttachmentInfo where
  name : String := ""
  mime : String := ""
  deriving Repr, BEq, Inhabited

/-- `CheckInput` + `CheckContext`. `headers` is the message's own header block, top first, keys
lower-case; `none` when only the envelope is known, and then `auth` stands in for it. An empty
block (`some #[]`) is not `none`: as in JavaScript (`[]` is truthy) it means "no verdicts". -/
structure Input where
  headers : Option (Array (String × String)) := none
  auth : Option Auth := none
  from? : Option Addr := none
  replyTo : List Addr := []
  subject : String := ""
  text : String := ""
  /-- `none` and `some ""` are the same to every rule (JavaScript's `""` is falsy). -/
  html : Option String := none
  attachments : List AttachmentInfo := []
  listId : String := ""
  listUnsubscribe : String := ""
  /-- Gmail's labels for imported mail (`SPAM`). -/
  labels : List String := []
  /-- The vault's own domains (the context's `own_domains`). -/
  ownDomains : List String := []
  deriving Repr, Inhabited

inductive Family | phish | spam
  deriving Repr, BEq, Inhabited

def Family.toString : Family → String
  | .phish => "phish"
  | .spam => "spam"

inductive Kind | clean | suspicious | spam | phishing
  deriving Repr, BEq, Inhabited

def Kind.toString : Kind → String
  | .clean => "clean"
  | .suspicious => "suspicious"
  | .spam => "spam"
  | .phishing => "phishing"

/-- One reason: its code, its family, its weight and the words that fill its text, in the order the
JavaScript object literal lists them. -/
structure Reason where
  code : String
  family : Family
  weight : Nat
  detail : List (String × String) := []
  deriving Repr, BEq, Inhabited

/-- `CheckResult`. -/
structure Verdict where
  v : Nat := checkVersion
  verdict : Kind
  phish : Nat
  spam : Nat
  reasons : List Reason
  deriving Repr, BEq, Inhabited

/-! ## Tables -/

/-- Names a phishing message puts in the sender's name, and the domains each one's mail really comes
from. Lower-case; matched as whole words. -/
def brands : List (String × List String) := [
  ("paypal", ["paypal.com", "paypal.co.uk", "paypal-communication.com"]),
  ("apple", ["apple.com", "icloud.com", "me.com"]),
  ("icloud", ["apple.com", "icloud.com"]),
  ("microsoft", ["microsoft.com", "microsoftonline.com", "office.com", "outlook.com", "live.com", "office365.com"]),
  ("outlook", ["microsoft.com", "outlook.com", "live.com", "office.com"]),
  ("office 365", ["microsoft.com", "office.com", "office365.com", "microsoftonline.com"]),
  ("google", ["google.com", "gmail.com", "youtube.com", "googlemail.com"]),
  ("gmail", ["google.com", "gmail.com", "googlemail.com"]),
  ("amazon", ["amazon.com", "amazon.co.uk", "amazon.de", "amazon.ca", "amazon.co.jp", "amazonses.com", "amazonaws.com"]),
  ("netflix", ["netflix.com"]),
  ("dhl", ["dhl.com", "dhl.de"]),
  ("fedex", ["fedex.com"]),
  ("ups", ["ups.com"]),
  ("usps", ["usps.com", "usps.gov"]),
  ("irs", ["irs.gov"]),
  ("docusign", ["docusign.com", "docusign.net"]),
  ("dropbox", ["dropbox.com", "dropboxmail.com"]),
  ("coinbase", ["coinbase.com"]),
  ("chase bank", ["chase.com", "jpmorgan.com"]),
  ("jpmorgan chase", ["chase.com", "jpmorgan.com"]),
  ("wells fargo", ["wellsfargo.com"]),
  ("bank of america", ["bankofamerica.com", "bofa.com", "ealerts.bankofamerica.com"]),
  ("facebook", ["facebook.com", "facebookmail.com", "meta.com"]),
  ("instagram", ["instagram.com", "facebookmail.com", "meta.com"]),
  ("linkedin", ["linkedin.com"]),
  ("cloudflare", ["cloudflare.com"]),
  ("trust wallet", ["trustwallet.com"]),
  ("metamask", ["metamask.io", "consensys.io", "consensys.net"]),
  ("binance", ["binance.com", "binance.us"]),
  ("ledger live", ["ledger.com"]),
  ("phantom wallet", ["phantom.app", "phantom.com"]),
  ("exodus wallet", ["exodus.com", "exodus.io"]),
  ("siteground", ["siteground.com"])]

/-- Files that run when opened (`RUNS`), and files that open a page (`PAGE`). -/
def runs : List String := ["exe", "scr", "com", "pif", "bat", "cmd", "js", "jse", "vbs", "vbe", "wsf", "wsh",
  "hta", "msi", "msp", "lnk", "iso", "img", "vhd", "jar", "ps1", "reg", "cpl", "appx", "dmg", "pkg", "app"]
def pages : List String := ["html", "htm", "shtml", "xhtml", "svg"]

/-- Click counters and link scanners: sites, as `siteOf` gives them. -/
def redirectors : List String := ["list-manage.com", "mailchimp.com", "mcsv.net", "sendgrid.net",
  "mandrillapp.com", "mailgun.org", "mailgun.net", "hubspotlinks.com", "hubspotemail.net", "rs6.net",
  "exacttarget.com", "sailthru.com", "klaviyomail.com", "klclick.com", "mlsend.com", "convertkit-mail.com",
  "substack.com", "beehiiv.com", "postmarkapp.com", "awstrack.me", "outlook.com", "urldefense.com", "mimecast.com"]

/-- What a link's words end in when they name a file rather than a place. -/
def fileEndings : List String := ["pdf", "doc", "docx", "xls", "xlsx", "ppt", "pptx", "txt", "csv", "jpg",
  "jpeg", "png", "gif", "zip", "html", "htm", "php", "aspx", "exe", "ics", "mp3", "mp4", "mov"]

/-- What a page that wants a password says. Lower-case, matched as is. -/
def lures : List String := ["verify your account", "verify your identity", "confirm your account",
  "confirm your identity", "confirm your password", "update your payment", "update your billing",
  "account has been suspended", "account will be suspended", "accounts will be suspended",
  "verify your wallet", "account has been locked", "account has been limited", "account is locked",
  "unusual sign-in", "unusual activity", "suspicious activity", "password expires",
  "password will expire", "password has expired", "your mailbox is full", "mailbox quota",
  "validate your mailbox", "failed delivery attempt"]

def replyMarks : List String := ["re:", "fwd:", "fw:"]

/-- What mail nobody asked for says. -/
def spamPhrases : List String := ["you have won", "you've won", "claim your prize", "lottery", "act now",
  "limited time offer", "100% free", "risk-free", "risk free", "no credit check", "earn money fast",
  "make money fast", "work from home", "crypto giveaway", "double your bitcoin", "inheritance",
  "beneficiary", "dear friend", "viagra", "cialis", "casino", "miracle cure", "lose weight fast",
  "unclaimed funds"]

/-- What each reason says to the person, `{field}` filled from its detail. -/
def reasonTexts : List (String × String) := [
  ("dmarc_fail", "The sender's domain ({domain}) says it did not send this (DMARC failed)."),
  ("spf_fail", "It came from a server {domain} does not send from (SPF failed)."),
  ("dkim_fail", "Its signature does not check out (DKIM failed)."),
  ("own_domain_unverified", "It says it is from your own domain ({domain}), but nothing proves it."),
  ("name_has_other_address", "The sender's name shows {shown}, but it came from {actual}."),
  ("brand_mismatch", "The sender's name says {brand}, but it came from {domain}."),
  ("brand_in_subject", "It says it is from {brand} and asks you to act, but it came from {domain}."),
  ("lookalike_domain", "{domain} looks like {like}, but is not it."),
  ("punycode_sender", "The sender's domain ({domain}) is written in look-alike characters."),
  ("reply_to_elsewhere", "Replies go to {reply}, not to {domain}."),
  ("link_text_mismatch", "A link shows {shown} but goes to {actual}."),
  ("link_userinfo", "A link hides where it goes behind {shown} (it goes to {actual})."),
  ("link_to_ip", "A link goes to a bare address ({actual}), not a named site."),
  ("link_punycode", "A link goes to {actual}, written in look-alike characters."),
  ("risky_attachment", "{name} runs a program when opened."),
  ("page_attachment", "{name} opens a web page, which is how fake sign-in pages travel."),
  ("credential_lure", "It asks you to act on your account (\"{phrase}\") and has a link."),
  ("gmail_spam", "Gmail had it in Spam."),
  ("upstream_spam", "A mail server on the way marked it as spam."),
  ("spam_phrases", "It reads like spam (\"{phrase}\"{more})."),
  ("bulk_no_unsubscribe", "It is bulk mail with no way to unsubscribe."),
  ("shouting_subject", "The subject shouts.")]

def verdictText : Kind → String
  | .clean => "Nothing looks wrong"
  | .suspicious => "Some things look off"
  | .spam => "Looks like spam"
  | .phishing => "Looks like phishing"

/-- Where "Report phishing" sends a copy: the Anti-Phishing Working Group. -/
def phishReportTo : String := "reportphishing@apwg.org"

/-- The words for one reason; a code this build does not know gets its code. -/
def reasonText (r : Reason) : String := Id.run do
  let some t := reasonTexts.lookup r.code | return r.code
  let a := t.toList.toArray
  let mut out := ""
  let mut i := 0
  while i < a.size do
    if a[i]! == '{' then
      let mut j := i + 1
      while j < a.size && 'a' ≤ a[j]! && a[j]! ≤ 'z' do j := j + 1
      if j > i + 1 && j < a.size && a[j]! == '}' then
        out := out ++ (r.detail.lookup (Core.Text.Str.slice a (i + 1) j)).getD ""
        i := j + 1
        continue
    out := out.push a[i]!
    i := i + 1
  return out

/-! ## Domains -/

/-- Second-level labels under a two-letter country code that are not a name of their own. -/
def secondLevel : List String := ["co", "com", "net", "org", "ac", "gov", "edu", "ne", "or", "go", "gv", "ltd", "plc", "me"]

def cleanHost (host : String) : String :=
  String.ofList ((Js.lower (Js.trim host)).toList.reverse.dropWhile (· == '.')).reverse

def isIpv4 (h : String) : Bool :=
  let ps := h.splitOn "."
  ps.length == 4 && ps.all fun p =>
    let cs := p.toList
    1 ≤ cs.length && cs.length ≤ 3 && cs.all isDigit && p.toNat! ≤ 255

/-- The name a domain is registered under, near enough: the last two labels, or three under a
country's second level. No public suffix list. -/
def siteOf (host : String) : String :=
  let h := cleanHost host
  if isIpv4 h then h else
  let labels := (h.splitOn ".").filter (· ≠ "")
  if labels.length ≤ 2 then ".".intercalate labels else
  let tld := labels.getLast!
  let second := labels[labels.length - 2]!
  let n := if Js.utf16Len tld == 2 && secondLevel.contains second then 3 else 2
  ".".intercalate (labels.drop (labels.length - n))

/-- `host` is `domain` or under it. -/
def within (host domain : String) : Bool :=
  let h := cleanHost host
  let d := cleanHost domain
  h == d || h.endsWith ("." ++ d)

def domainOf (address : String) : String :=
  let a := address.toList.toArray
  match Core.Text.Str.lastIndexOfChar a '@' with
  | some at_ => cleanHost (Core.Text.Str.slice a (at_ + 1) a.size)
  | none => ""

/-- The look-alikes a phishing domain swaps in, swapped back. -/
def unconfuse (label : String) : String :=
  let s := Js.replaceAll label "rn" "m"
  let s := Js.replaceAll s "vv" "w"
  let s := Js.replaceAll s "0" "o"
  let s := String.ofList (s.toList.map fun c => if c == '1' || c == 'i' then 'l' else c)
  let s := Js.replaceAll s "3" "e"
  let s := Js.replaceAll s "5" "s"
  Js.replaceAll s "-" ""

/-- Edit distance, stopping once it is past `max`. -/
def distance (a b : Array Char) (max : Nat) : Nat := Id.run do
  let gap := if a.size ≥ b.size then a.size - b.size else b.size - a.size
  if gap > max then return max + 1
  let mut prev : Array Nat := (List.range (b.size + 1)).toArray
  for i in [1 : a.size + 1] do
    let mut cur : Array Nat := #[i]
    let mut best := i
    for j in [1 : b.size + 1] do
      let cost := if a[i - 1]! == b[j - 1]! then 0 else 1
      let v := min (min (prev[j]! + 1) (cur[j - 1]! + 1)) (prev[j - 1]! + cost)
      cur := cur.push v
      if v < best then best := v
    if best > max then return max + 1
    prev := cur
  return prev[b.size]!

private def firstLabel (s : String) : String := ((s.splitOn ".").head?).getD ""

/-- The real site a sender's site is made to look like, and whether it took swapping look-alike
characters back to see it. One edit apart counts only when both names are six letters or more. -/
def lookalikeOf (site : String) (targets : List String) : Option (String × Bool) :=
  let label := firstLabel site
  if label.isEmpty || targets.contains site then none else
  match targets.find? (fun t => let real := firstLabel t
      real != "" && real != label && unconfuse label == unconfuse real) with
  | some t => some (t, true)
  | none =>
    (targets.find? fun t => let real := firstLabel t
      real != "" && real != label && min (Js.utf16Len label) (Js.utf16Len real) ≥ 6 &&
        distance label.toList.toArray real.toList.toArray 1 ≤ 1).map (·, false)

/-! ## Text -/

/-- Where `word` first stands as a word of its own in `hay` (nothing in `[a-z0-9]` either side). -/
def wordAt (hay word : Array Char) : Option Nat := Id.run do
  if word.size > hay.size then return none
  for i in [0 : hay.size - word.size + 1] do
    if Js.at? hay i word then
      let beforeOk := i == 0 || !Js.isLowerAlnum hay[i - 1]!
      let afterOk := i + word.size ≥ hay.size || !Js.isLowerAlnum hay[i + word.size]!
      if beforeOk && afterOk then return some i
  return none

def hasWord (hay word : String) : Bool := (wordAt hay.toList.toArray word.toList.toArray).isSome

/-! ## Links -/

structure Link where
  href : String
  host : String
  userinfo : String
  text : String
  deriving Repr, Inhabited

/-- `/^\s*https?:\/\//i`: where the authority starts, if `a` begins like that from `i`. -/
private def schemeEnd (a : Array Char) (i : Nat) : Option Nat :=
  if !Js.at? a i "http".toList.toArray true then none
  else if i + 4 < a.size && Js.asciiLower a[i + 4]! == 's' then
    if Js.at? a (i + 5) "://".toList.toArray then some (i + 8) else none
  else if Js.at? a (i + 4) "://".toList.toArray then some (i + 7) else none

/-- Where a link goes: its host and anything before an `@` in its authority. `none` for anything
that is not http or https. -/
def linkHost (href : String) : Option (String × String) := do
  let a := href.toList.toArray
  let mut i := 0
  while i < a.size && isJsWhitespace a[i]! do i := i + 1
  let start ← schemeEnd a i
  let mut j := start
  while j < a.size && !(a[j]! == '/' || a[j]! == '?' || a[j]! == '#' || isJsWhitespace a[j]!) do j := j + 1
  let auth := a.extract start j
  let (userinfo, host) := match Core.Text.Str.lastIndexOfChar auth '@' with
    | some at_ => (Core.Text.Str.slice auth 0 at_, Core.Text.Str.slice auth (at_ + 1) auth.size)
    | none => ("", String.ofList auth.toList)
  if host.startsWith "[" then return (Js.lower host, userinfo)
  let hc := host.toList.toArray
  let host := match Core.Text.Str.indexOfChar hc ':' with
    | some c => Core.Text.Str.slice hc 0 c
    | none => host
  return (cleanHost host, userinfo)

/-- `SHOWS_HOST` on a link's (lower-cased) words: the site they name, if they are a host, optionally
with a scheme before and a path, port, query or fragment after. -/
def showsHost (t : String) : Option String :=
  let a := t.toList
  let a := if a.take 8 == "https://".toList then a.drop 8 else if a.take 7 == "http://".toList then a.drop 7 else a
  let isDom (c : Char) := Js.isLowerAlnum c || c == '-' || c == '.'
  let p := a.takeWhile isDom
  let rest := a.dropWhile isDom
  let restOk := match rest with
    | [] => true
    | c :: cs => (c == '/' || c == ':' || c == '?' || c == '#') && cs.all (!isJsWhitespace ·)
  let pieces := (String.ofList p).splitOn "."
  let labelOk (l : String) : Bool :=
    let cs := l.toList
    match cs.head?, cs.getLast? with
    | some x, some y => Js.isLowerAlnum x && Js.isLowerAlnum y
    | _, _ => false
  let tld := pieces.getLast!
  if restOk && pieces.length ≥ 2 && (pieces.dropLast.all labelOk) && tld.length ≥ 2 &&
      tld.toList.all (fun c => 'a' ≤ c && c ≤ 'z') then some (String.ofList p) else none

/-- Find the earliest `</a\s*>` (case folded) at or after `c`: (where it starts, where it ends). -/
private def closeAnchor (h : Array Char) (c : Nat) : Option (Nat × Nat) := Id.run do
  let pat := "</a".toList.toArray
  let mut j := c
  while j + 3 ≤ h.size do
    if Js.at? h j pat true then
      let mut k := j + 3
      while k < h.size && isJsWhitespace h[k]! do k := k + 1
      if k < h.size && h[k]! == '>' then return some (j, k + 1)
    j := j + 1
  return none

/-- After the `href` value ends at `after`: `[^>]*>([\s\S]*?)<\/a\s*>`. -/
private def anchorRest (h : Array Char) (after : Nat) : Option (String × Nat) := do
  let g ← Core.Text.Str.indexOfChar h '>' after
  let (j, e) ← closeAnchor h (g + 1)
  return (Core.Text.Str.slice h (g + 1) j, e)

/-- The anchor regex of `anchorsOf` tried at `i`:
`/<a\b[^>]*?\bhref\s*=\s*(?:"([^"]*)"|'([^']*)'|([^\s>]+))[^>]*>([\s\S]*?)<\/a\s*>/i`,
backtracking as JavaScript does: (raw href, inner HTML, end). -/
private def anchorAt (h : Array Char) (i : Nat) : Option (String × String × Nat) := Id.run do
  let n := h.size
  if !(i + 1 < n && h[i]! == '<' && Js.asciiLower h[i + 1]! == 'a') then return none
  let p0 := i + 2
  if p0 < n && Js.isWordC h[p0]! then return none
  let href := "href".toList.toArray
  let mut p := p0
  while p ≤ n do
    if !Js.isWordC h[p - 1]! && Js.at? h p href true then
      let mut q := p + 4
      while q < n && isJsWhitespace h[q]! do q := q + 1
      if q < n && h[q]! == '=' then
        let mut r := q + 1
        while r < n && isJsWhitespace h[r]! do r := r + 1
        if r < n then
          -- the alternatives in order; the unquoted one only at its longest (a shorter run reaches
          -- the same `>` and the same content, so it cannot succeed where the longest failed)
          let mut alts : Array (String × Nat) := #[]
          for qc in ['"', '\''] do
            if h[r]! == qc then
              if let some e := Core.Text.Str.indexOfChar h qc (r + 1) then
                alts := alts.push (Core.Text.Str.slice h (r + 1) e, e + 1)
          let mut e := r
          while e < n && !(isJsWhitespace h[e]! || h[e]! == '>') do e := e + 1
          if e > r then alts := alts.push (Core.Text.Str.slice h r e, e)
          for (v, after) in alts do
            if let some (inner, fin) := anchorRest h after then return some (v, inner, fin)
    if p < n && h[p]! != '>' then p := p + 1 else break
  return none

/-- The HTML's anchors, with their words. -/
def anchorsOf (html : String) (out : Array Link) : Array Link := Id.run do
  let h := html.toList.toArray
  let mut out := out
  let mut i := 0
  while i < h.size do
    if h[i]! == '<' then
      if let some (raw, inner, fin) := anchorAt h i then
        if out.size ≥ linksMax then return out
        let href := Js.trim (Js.decodeEntities raw)
        if let some (host, userinfo) := linkHost href then
          out := out.push { href, host, userinfo, text := Js.stripTags inner }
        i := fin
        continue
    i := i + 1
  return out

/-- A URL written out in the text: `/https?:\/\/[^\s<>"')\]]+/gi`. -/
def bareLinksOf (text : String) (out : Array Link) : Array Link := Id.run do
  let a := text.toList.toArray
  let mut out := out
  let mut i := 0
  let stop (c : Char) : Bool := isJsWhitespace c || c == '<' || c == '>' || c == '"' || c == '\'' || c == ')' || c == ']'
  while i < a.size do
    if let some s := schemeEnd a i then
      let mut e := s
      while e < a.size && !stop a[e]! do e := e + 1
      if e > s then
        if out.size ≥ linksMax then return out
        let m := Core.Text.Str.slice a i e
        if let some (host, userinfo) := linkHost m then
          out := out.push { href := m, host, userinfo, text := "" }
        i := e
        continue
    i := i + 1
  return out

private def nonEmpty? : Option String → Option String
  | some s => if s.isEmpty then none else some s
  | none => none

/-- Every http(s) link in the HTML and in the text. -/
def linksOf (html : Option String) (text : String) : Array Link :=
  let out := match nonEmpty? html with
    | some h => anchorsOf (Js.takeUtf16 h htmlMax) #[]
    | none => #[]
  bareLinksOf (Js.takeUtf16 text textMax) out

/-! ## Authentication -/

/-- `(?:^|[;\s])k=([a-z]+)`, case folded: the verdict for `k`, lower-cased, or `""`. -/
def pick (value k : String) : String := Id.run do
  let a := value.toList.toArray
  let kk := (k ++ "=").toList.toArray
  for s in [0 : a.size + 1] do
    let starts : List Nat :=
      (if s == 0 then [0] else []) ++ (if s < a.size && (a[s]! == ';' || isJsWhitespace a[s]!) then [s + 1] else [])
    for st in starts do
      if Js.at? a st kk true then
        let mut e := st + kk.size
        while e < a.size && isAlpha a[e]! do e := e + 1
        if e > st + kk.size then return Js.lower (Core.Text.Str.slice a (st + kk.size) e)
  return ""

/-- The verdicts of the server that took the message in: the topmost `Authentication-Results` or
`ARC-Authentication-Results` only. -/
def receiverAuth (headers : Array (String × String)) : Option Auth :=
  (headers.find? fun (k, _) => k == "authentication-results" || k == "arc-authentication-results").map
    fun (_, v) => { spf := pick v "spf", dkim := pick v "dkim", dmarc := pick v "dmarc" }

/-! ## The check -/

/-- What every rule reads, worked out once. -/
structure Seen where
  input : Input
  auth : Option Auth
  own : List String
  fromAddr : String
  fromName : String
  fromDomain : String
  fromSite : String
  links : Array Link
  words : String

/-- The subject and the text, or the HTML's words when there is no text, lower-case. -/
def wordsOf (i : Input) : String :=
  let body := if !(Js.trim i.text).isEmpty then Js.takeUtf16 i.text textMax
    else match nonEmpty? i.html with
      | some h => Js.stripTags (Js.takeUtf16 h htmlMax)
      | none => ""
  Js.lower (i.subject ++ "\n" ++ body)

def see (i : Input) : Seen :=
  let given := match i.headers with
    | some hs => receiverAuth hs
    | none => i.auth
  let auth := given.filter fun a => !a.spf.isEmpty || !a.dkim.isEmpty || !a.dmarc.isEmpty
  let fromAddr := Js.lower (Js.trim ((i.from?.map (·.address)).getD ""))
  let fromDomain := domainOf fromAddr
  { input := i, auth, own := (i.ownDomains.map cleanHost).filter (· ≠ "")
    fromAddr, fromName := Js.trim ((i.from?.map (·.name)).getD ""), fromDomain
    fromSite := if fromDomain.isEmpty then "" else siteOf fromDomain
    links := linksOf i.html i.text, words := wordsOf i }

/-- Reasons so far; `add` records a reason once, the first time its rule fires. -/
abbrev Rs := Array Reason

def add (rs : Rs) (code : String) (family : Family) (weight : Nat) (detail : List (String × String) := []) : Rs :=
  if rs.any (·.code == code) then rs else rs.push { code, family, weight, detail }

def authRules (s : Seen) (rs : Rs) : Rs := Id.run do
  let some a := s.auth | return rs
  let who := if s.fromDomain.isEmpty then "the sender" else s.fromDomain
  let mut rs := rs
  if a.dmarc == "fail" then rs := add rs "dmarc_fail" .phish 3 [("domain", who)]
  if a.spf == "fail" then rs := add rs "spf_fail" .phish 1 [("domain", who)]
  if a.dkim == "fail" then rs := add rs "dkim_fail" .phish 1
  return rs

def ownDomainRule (s : Seen) (rs : Rs) : Rs := Id.run do
  let some a := s.auth | return rs
  if s.fromDomain.isEmpty || a.dmarc == "pass" || a.dkim == "pass" || a.spf == "pass" then return rs
  if s.own.any (within s.fromDomain ·) then return add rs "own_domain_unverified" .phish 4 [("domain", s.fromDomain)]
  return rs

/-- `/[a-z0-9._%+-]+@([a-z0-9-]+(?:\.[a-z0-9-]+)+)/` on the lower-cased name: (whole match, domain). -/
def shownAddress (name : String) : Option (String × String) := Id.run do
  let a := name.toList.toArray
  let isLocal (c : Char) := Js.isLowerAlnum c || c == '.' || c == '_' || c == '%' || c == '+' || c == '-'
  let isLab (c : Char) := Js.isLowerAlnum c || c == '-'
  for p in [0 : a.size] do
    if !isLocal a[p]! then continue
    let mut q := p
    while q < a.size && isLocal a[q]! do q := q + 1
    if !(q < a.size && a[q]! == '@') then continue
    let d0 := q + 1
    let mut e := d0
    while e < a.size && isLab a[e]! do e := e + 1
    if e == d0 then continue
    let mut reps := 0
    while e + 1 < a.size && a[e]! == '.' && isLab a[e + 1]! do
      e := e + 1
      while e < a.size && isLab a[e]! do e := e + 1
      reps := reps + 1
    if reps == 0 then continue
    return some (Core.Text.Str.slice a p e, Core.Text.Str.slice a d0 e)
  return none

def nameRules (s : Seen) (rs : Rs) : Rs := Id.run do
  if s.fromDomain.isEmpty then return rs
  let name := Js.lower s.fromName
  let mut rs := rs
  if let some (whole, dom) := shownAddress name then
    if s.input.listId.isEmpty && siteOf dom != s.fromSite then
      rs := add rs "name_has_other_address" .phish 3 [("shown", whole), ("actual", s.fromAddr)]
  if (brands.find? fun (b, ds) => hasWord name b && !ds.any (within s.fromDomain ·)).isSome then
    rs := add rs "brand_mismatch" .phish 2 [("brand", s.fromName), ("domain", s.fromDomain)]
  return rs

def subjectBrandRule (s : Seen) (rs : Rs) : Rs := Id.run do
  if s.fromDomain.isEmpty || rs.any (·.code == "brand_mismatch") then return rs
  if !lures.any (Js.includes s.words ·) then return rs
  let said := s.input.subject
  let subject := Js.lower said
  if replyMarks.any ((Js.trim subject).startsWith ·) then return rs
  let some (b, _) := brands.find? (fun (b, ds) => hasWord subject b && !ds.any (within s.fromDomain ·)) | return rs
  -- the brand as the subject writes it, where lower-casing kept every position; else as the list does
  let at_ := (wordAt subject.toList.toArray b.toList.toArray).getD 0
  let shown := if Js.utf16Len said == Js.utf16Len subject && said.length == subject.length then
      Core.Text.Str.slice said.toList.toArray at_ (at_ + b.length) else b
  return add rs "brand_in_subject" .phish 2 [("brand", shown), ("domain", s.fromDomain)]

def domainRules (s : Seen) (rs : Rs) : Rs := Id.run do
  if s.fromSite.isEmpty then return rs
  let mut rs := rs
  if (s.fromDomain.splitOn ".").any (·.startsWith "xn--") then
    rs := add rs "punycode_sender" .phish 3 [("domain", s.fromDomain)]
  let all := s.own.map siteOf ++ brands.flatMap (fun (_, ds) => ds.map siteOf)
  let targets := all.foldl (fun acc d => if acc.contains d then acc else acc ++ [d]) []
  if let some (like, swapped) := lookalikeOf s.fromSite targets then
    rs := add rs "lookalike_domain" .phish (if swapped then 4 else 3) [("domain", s.fromDomain), ("like", like)]
  return rs

def replyRules (s : Seen) (rs : Rs) : Rs :=
  if s.fromSite.isEmpty || !s.input.listId.isEmpty then rs else
  match (s.input.replyTo.map (Js.lower ·.address)).find? (fun a => let d := domainOf a
      !d.isEmpty && siteOf d != s.fromSite) with
  | some r => add rs "reply_to_elsewhere" .phish 1 [("reply", r), ("domain", s.fromDomain)]
  | none => rs

/-- The site a link's own words name, when it is not where the link goes. -/
def namesOtherSite (l : Link) (fromSite : String) : String :=
  let named := (showsHost (Js.lower l.text)).getD ""
  let ending := match (named.splitOn ".").getLast? with | some e => e | none => named
  if named.isEmpty || fileEndings.contains ending then "" else
  let goesTo := siteOf l.host
  if redirectors.contains goesTo || goesTo == fromSite then "" else
  if siteOf named != goesTo then named else ""

/-- `/[a-z0-9-]+\.[a-z]{2,}/i`. -/
private def looksLikeHost (s : String) : Bool := Id.run do
  let a := s.toList.toArray
  let lab (c : Char) := isAlpha c || isDigit c || c == '-'
  for i in [1 : a.size] do
    if a[i]! == '.' && lab a[i - 1]! && i + 2 < a.size && isAlpha a[i + 1]! && isAlpha a[i + 2]! then
      return true
  return false

def linkRules (s : Seen) (rs : Rs) : Rs := Id.run do
  let mut rs := rs
  for l in s.links do
    if !l.userinfo.isEmpty && looksLikeHost l.userinfo then
      rs := add rs "link_userinfo" .phish 3 [("shown", Js.lower l.userinfo), ("actual", l.host)]
    let named := namesOtherSite l s.fromSite
    if !named.isEmpty then rs := add rs "link_text_mismatch" .phish 3 [("shown", named), ("actual", l.host)]
    if isIpv4 l.host || l.host.startsWith "[" then rs := add rs "link_to_ip" .phish 2 [("actual", l.host)]
    if (l.host.splitOn ".").any (·.startsWith "xn--") then rs := add rs "link_punycode" .phish 2 [("actual", l.host)]
  return rs

private def endsWithExt (name : String) (exts : List String) : Bool := exts.any fun e => name.endsWith ("." ++ e)

def attachmentRules (s : Seen) (rs : Rs) : Rs := Id.run do
  let mut rs := rs
  for a in s.input.attachments do
    let name := Js.lower (Js.trim a.name)
    if name.isEmpty then continue
    if endsWithExt name runs then rs := add rs "risky_attachment" .phish 3 [("name", Js.trim a.name)]
    else if endsWithExt name pages || Js.lower a.mime == "text/html" then
      rs := add rs "page_attachment" .phish 2 [("name", Js.trim a.name)]
  return rs

def lureRule (s : Seen) (rs : Rs) : Rs :=
  match lures.find? (Js.includes s.words ·) with
  | some lure => if s.links.size > 0 || rs.any (·.code == "page_attachment")
      then add rs "credential_lure" .phish 2 [("phrase", lure)] else rs
  | none => rs

/-- The topmost header named `key`, trimmed and lower-cased, or `""` (also with no header block). -/
def top (s : Seen) (key : String) : String :=
  match s.input.headers with
  | some hs => Js.lower (Js.trim (((hs.find? (·.1 == key)).map (·.2)).getD ""))
  | none => ""

def spamFlagRules (s : Seen) (rs : Rs) : Rs := Id.run do
  let mut rs := rs
  if s.input.labels.contains "SPAM" then rs := add rs "gmail_spam" .spam 4
  if top s "x-spam-flag" == "yes" || (top s "x-spam-status").startsWith "yes" then rs := add rs "upstream_spam" .spam 3
  return rs

def spamWordRules (s : Seen) (rs : Rs) : Rs :=
  let phrases := spamPhrases.filter (Js.includes s.words ·)
  match phrases with
  | [] => rs
  | p :: more => add rs "spam_phrases" .spam (if more.isEmpty then 1 else 2)
      [("phrase", p), ("more", if more.isEmpty then "" else s!" and {more.length} more")]

def spamShapeRules (s : Seen) (rs : Rs) : Rs := Id.run do
  let mut rs := rs
  if (!s.input.listId.isEmpty || top s "precedence" == "bulk") && s.input.listUnsubscribe.isEmpty then
    rs := add rs "bulk_no_unsubscribe" .spam 1
  let subject := s.input.subject
  let letters := subject.toList.filter isAlpha
  if (letters.length ≥ 10 && letters.all (fun c => 'A' ≤ c && c ≤ 'Z')) || Js.includes subject "!!!" then
    rs := add rs "shouting_subject" .spam 1
  return rs

/-- The verdict from the weights: phishing first, then spam, then "suspicious". -/
def verdictOf (rs : List Reason) : Verdict :=
  let weigh (f : Family) := (rs.filter (·.family == f)).foldl (· + ·.weight) 0
  let phish := weigh .phish
  let spam := weigh .spam
  let verdict := if phish ≥ phishAt then Kind.phishing else if spam ≥ spamAt then .spam
    else if phish ≥ suspiciousAt || spam ≥ suspiciousAt then .suspicious else .clean
  { verdict, phish, spam, reasons := rs }

/-- The rules, in the order they run: each reason is listed where its rule first fired. -/
def rules : List (Seen → Rs → Rs) := [authRules, ownDomainRule, nameRules, subjectBrandRule, domainRules,
  replyRules, linkRules, attachmentRules, lureRule, spamFlagRules, spamWordRules, spamShapeRules]

/-- Check one message. -/
def check (input : Input) : Verdict :=
  let s := see input
  verdictOf (rules.foldl (fun rs r => r s rs) #[]).toList

/-! ## From a parsed message -/

private def addrOf : Core.Mime.Addr → Addr
  | .mailbox n a => { name := n, address := a }
  | .group n _ => { name := n, address := "" }

/-- The check's input from a parsed message, as MailVault's `public/junk.js` built it from the opened
message: the raw header block (top first, keys lower-case), postal-mime's `from`, `Reply-To`, subject,
text, HTML and attachment names, and the first `List-Id` / `List-Unsubscribe` values (cut to 200 / 500
characters, as `shared/envelope.js` stores them). `labels` are Gmail's for imported mail; `ownDomains`
the vault's sending domains (`ownDomainsOf`: its domains and its own send-as zones). The JavaScript
reads no own *addresses*. A group in `From` / `Reply-To` gives its name and an empty address, as
postal-mime's group objects (no `address`) read to the check. -/
def inputOfMessage (m : Core.Mime.Message) (ownDomains : List String) (labels : List String := []) : Input :=
  { headers := some (m.headers.map fun f => (f.key, f.value))
    from? := m.from?.map addrOf
    replyTo := m.replyTo.map addrOf
    subject := m.subject?.getD ""
    text := m.text.getD ""
    html := nonEmpty? m.html
    attachments := m.attachments.toList.map fun a => { name := a.filename.getD "", mime := a.mimeType }
    listId := Js.takeUtf16 ((m.rawHeader? "list-id").getD "") 200
    listUnsubscribe := Js.takeUtf16 ((m.rawHeader? "list-unsubscribe").getD "") 500
    labels, ownDomains }

end Core.Junk
