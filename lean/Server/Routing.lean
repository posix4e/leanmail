import Lean.Data.Json
import Server.State
import Server.Router
import Server.Worker
import Server.Send
import Server.Blocks
import Server.Clock
import Server.Crypto
import Server.Log

/-! # Routing settings: domains, addresses, the Worker's routes, and Cloudflare

The owner's routing lives in `domains` and `addresses` (migration 4). After every change it is
projected to the Worker (`PUT /internal/routes {account_id, routes, replace: true}`), which decides
vault / forward / drop for each delivery from D1 without waking anything: one row per address plus
a `*@<domain>` row carrying each domain's default.

What Cloudflare must change to deliver to that Worker is a plan, a port of MailVault's
`src/plan.ts` and `src/reconcile.ts` reduced to what this schema has (no `edge` addresses kept at
Cloudflare, no `worker` mail mode): for each managed domain, its catch-all hands mail to the
Worker named by `CF_WORKER_NAME`; address rules this vault made (named `lm:` or MailVault's `mv:`)
are deleted once the catch-all reaches the Worker; Email preview is turned off on the names mail is
sent from; rules someone else made are listed, never touched unless named in `remove` at apply.
A zone that could not be read plans nothing. Without `CF_API_TOKEN`/`CF_ACCOUNT_ID` the plan is
made against zones assumed empty (`assumed`), and it cannot be applied (`can_apply: false`).

The Cloudflare API goes through `State.cloudflare`, so tests answer with a fake.

| route | auth | |
|---|---|---|
| `GET /api/config` | token `read` | domains, addresses, whether Cloudflare is configured |
| `POST /api/config/domains {name, manage?, default_mail?, default_forward_to?, sending?}` | session | |
| `PATCH /api/config/domains/:name` | session | the same fields |
| `DELETE /api/config/domains/:name` | session | the domain and its addresses |
| `PUT /api/config/addresses/:address {mail?, forward_to?, can_send?, note?}` | session | |
| `DELETE /api/config/addresses/:address` | session | |
| `GET /api/config/plan` | session | the diff against Cloudflare |
| `POST /api/config/apply {plan_id?, remove?}` | session | 503 `no_cf_token` without the token |
| `GET /api/sendable` | token `read` | the From choices |

**Grants.** An account may add, plan and apply only the domains the operator granted it: the
Worker answers `GET /internal/grants` → `{domains}` from D1 for the calling account
(`Worker.grants`). Adding an ungranted domain is 403 `domain_not_granted`; a domain held from
before that is not granted is left out of the plan (a `domain_not_granted` problem) and so out of
apply. The operator container and the dev bypass may use any domain. A Worker that does not answer
grants nothing. The plan never lists the Cloudflare account's other zones, and destination
addresses are only used to check this account's own forward targets.

**Who talks to Cloudflare.** `CF_API_TOKEN` is given only to the operator container. An account
container (`needsOperator`) sends its granted domains and their addresses to the Worker's
`POST /internal/routing/plan` / `apply` (`{domains, addresses[, plan_id, remove]}`); the Worker
forwards the body to the operator's own `POST /internal/routing/plan|apply` with
`x-routing-account: <account>` and hands back the operator's status and body. The operator
(`opPlan`, `opApply`, internal auth, operator only) refuses 403 `domain_not_granted` unless every
domain in the body is in its `domain_grants` for that account, then plans and applies with its
token. When the operator cannot be reached, the account's plan is the one made against zones
assumed empty and `GET /api/config/plan` answers it with 503 `routing_needs_operator`; apply is
refused the same way. The account keeps `routing.last_apply` itself.
-/

namespace Server.Routing

open Lean Server.Db Server.Router

def mails : List String := ["vault", "vault+forward", "forward", "drop"]
def vaultModes : List String := ["vault", "vault+forward"]
def forwardModes : List String := ["forward", "vault+forward"]
def forwardMax : Nat := 10
def ownRulePrefixes : List String := ["lm:", "mv:"]

/-! ## The owner's routing -/

structure Domain where
  name : String
  manage : Bool := true
  defaultMail : String := "vault"
  defaultForwardTo : List String := []
  sending : List String := []
  deriving Repr, BEq, Inhabited

structure Address where
  address : String
  domain : String
  mail : String := "vault"
  forwardTo : List String := []
  canSend : Bool := true
  note : String := ""
  deriving Repr, BEq, Inhabited

def strList (s : String) : List String :=
  match Json.parse s with
  | .ok (.arr xs) => xs.toList.filterMap fun | .str v => some v | _ => none
  | _ => []

def strListJson (l : List String) : String := (Json.arr (l.toArray.map Json.str)).compress

def loadDomains (st : State) : IO (List Domain) := do
  let rows ← st.db.query "SELECT * FROM domains ORDER BY name"
  return rows.toList.map fun r => { name := r.text "name", manage := r.bool "manage", defaultMail := r.text "default_mail"
                                    defaultForwardTo := strList (r.text "default_forward_to"), sending := strList (r.text "sending") }

def loadAddresses (st : State) : IO (List Address) := do
  let rows ← st.db.query "SELECT * FROM addresses ORDER BY domain, address"
  return rows.toList.map fun r => { address := r.text "address", domain := r.text "domain", mail := r.text "mail"
                                    forwardTo := strList (r.text "forward_to"), canSend := r.bool "can_send", note := r.text "note" }

def Domain.toJson (d : Domain) : Json := Json.mkObj [
  ("name", Json.str d.name), ("manage", Json.bool d.manage),
  ("default", Json.mkObj [("mail", Json.str d.defaultMail), ("forward_to", Json.arr (d.defaultForwardTo.toArray.map Json.str))]),
  ("sending", Json.arr (d.sending.toArray.map Json.str))]

def Address.toJson (a : Address) : Json := Json.mkObj [
  ("address", Json.str a.address), ("domain", Json.str a.domain), ("mail", Json.str a.mail),
  ("forward_to", Json.arr (a.forwardTo.toArray.map Json.str)), ("can_send", Json.bool a.canSend), ("note", Json.str a.note)]

def jStrs (j : Json) (k : String) : List String :=
  ((j.getObjValAs? (Array Json) k).toOption.getD #[]).toList.filterMap fun | .str v => some v | _ => none

/-- A domain as `Domain.toJson` wrote it (the operator reads an account's routing so). -/
def Domain.ofJson? (j : Json) : Option Domain := do
  let name := ((jstr? j "name").getD "").toLower
  guard (Blocks.isDomain name)
  let dflt := (j.getObjVal? "default").toOption.getD Json.null
  let mail := (jstr? dflt "mail").getD "vault"
  guard (mails.contains mail)
  return { name, manage := (jbool? j "manage").getD true, defaultMail := mail,
           defaultForwardTo := (jStrs dflt "forward_to").take forwardMax, sending := jStrs j "sending" }

/-- An address as `Address.toJson` wrote it. -/
def Address.ofJson? (j : Json) : Option Address := do
  let address ← (jstr? j "address").bind normEmail?
  let mail := (jstr? j "mail").getD "vault"
  guard (mails.contains mail)
  return { address, domain := Blocks.domainOf address, mail, forwardTo := (jStrs j "forward_to").take forwardMax,
           canSend := (jbool? j "can_send").getD true }

/-! ## The Worker's routes -/

/-- Whether mail may be sent as anyone on a domain without an address row (`Send.sendable`). -/
def domainSendable (ds : List Domain) (d : Domain) : Bool :=
  vaultModes.contains d.defaultMail && ds.any (·.sending.contains d.name)

/-- What `PUT /internal/routes` carries: each address, then each domain's `*@domain` default. -/
def routesOf (ds : List Domain) (as : List Address) : List Json :=
  let row (addr mail : String) (fwd : List String) (canSend : Bool) := Json.mkObj [
    ("address", Json.str addr), ("mail", Json.str mail),
    ("forward_to", Json.arr ((if forwardModes.contains mail then fwd else []).toArray.map Json.str)),
    ("can_send", Json.bool canSend)]
  (as.map fun a => row a.address a.mail a.forwardTo (a.canSend && vaultModes.contains a.mail)) ++
  (ds.map fun d => row s!"*@{d.name}" d.defaultMail d.defaultForwardTo (domainSendable ds d))

/-- Project the routing to the Worker. `true` when it took it. -/
def project (st : State) : IO Bool := do
  let ds ← loadDomains st
  let as ← loadAddresses st
  let rs := routesOf ds as
  let body := Json.mkObj [("account_id", Json.str st.config.accountId), ("routes", Json.arr rs.toArray), ("replace", Json.bool true)]
  let ok := (← Worker.internal st "/internal/routes" body (method := "PUT")).isSome
  Log.info "routing.projected" [("routes", Json.num rs.length), ("ok", Json.bool ok)]
  return ok

/-! ## Validation -/

def bad (field : String) (reason : String := "invalid") : Except (Nat × String) α := .error (400, s!"{field}: {reason}")

/-- Forward targets: addresses, not on the vault's own domains, at most ten; only for a mode that forwards. -/
def targetsFor (mail : String) (raw : Option Json) (own : List String) (field : String) : Except (Nat × String) (List String) := do
  let list : List String := match raw with
    | some (.arr xs) => xs.toList.filterMap fun | .str v => some (Core.Text.Str.trim v).toLower | _ => none
    | _ => []
  let list := list.filter (· ≠ "") |>.eraseDups
  if !forwardModes.contains mail then return []
  if list.isEmpty then bad field "required"
  if list.length > forwardMax then bad field "too_many"
  for t in list do
    if (normEmail? t).isNone then bad field
    if Blocks.isOwn own (Blocks.domainOf t) then bad field "own_domain"
  return list

def mailOf (j : Json) (k : String) (d : String) : Except (Nat × String) String :=
  match jstr? j k with
  | some m => if mails.contains m then .ok m else bad k
  | none => .ok d

def sendingOf (name : String) (raw : Option Json) (d : List String) : Except (Nat × String) (List String) := do
  match raw with
  | none => return d
  | some (.arr xs) =>
    let l := xs.toList.filterMap fun | .str v => some (Core.Text.Str.trim v).toLower | _ => none
    for s in l do
      if !(s == name || s.endsWith ("." ++ name)) || !Blocks.isDomain s then bad "sending" "not_under_domain"
    return l.eraseDups
  | _ => bad "sending"

def ownDomains (st : State) : IO (List String) := do
  return (← loadDomains st).map (·.name)

/-! ## Grants -/

/-- The domains this container may route: `none` for any (the operator container, the dev
bypass); otherwise the Worker's grants for this account, empty when it did not answer. -/
def grantedDomains (st : State) : IO (Option (List String)) := do
  if st.config.isOperator || st.config.devBypassOn then return none
  return some ((← Worker.grants st).getD [])

def grantOk (granted : Option (List String)) (name : String) : Bool :=
  match granted with
  | none => true
  | some gs => gs.contains name.toLower

/-- An account container with no Cloudflare token of its own: Cloudflare is the operator's. -/
def needsOperator (c : Config) : Bool := !c.hasCloudflare && !c.isOperator && !c.devBypassOn

/-! ## Changes -/

/-- The routing JSON with the projection's outcome. -/
def configJson (st : State) : IO Json := do
  return Json.mkObj [
    ("domains", Json.arr ((← loadDomains st).toArray.map Domain.toJson)),
    ("addresses", Json.arr ((← loadAddresses st).toArray.map Address.toJson)),
    ("cloudflare", Json.mkObj [("configured", Json.bool st.config.hasCloudflare), ("worker", Json.str st.config.cfWorker)])]

def changed (st : State) (status : Nat := 200) : IO Resp := do
  let projected ← project st
  let j ← configJson st
  return Resp.json status (match j with
    | .obj kvs => Json.mkObj (kvs.toArray.toList ++ [("projected", Json.bool projected)])
    | x => x)

def addDomain (st : State) (j : Json) : IO (Except (Nat × String) Unit) := do
  let name := (Core.Text.Str.trim ((jstr? j "name").getD "")).toLower
  if !Blocks.isDomain name then return bad "name"
  if !grantOk (← grantedDomains st) name then
    Log.warn "routing.domain_not_granted" []
    return .error (403, "domain_not_granted")
  if (← st.db.queryOne "SELECT 1 AS x FROM domains WHERE name = ?" #[name]).isSome then return .error (409, "exists")
  let own := name :: (← ownDomains st)
  let r : Except (Nat × String) (String × List String × List String) := do
    let mail ← mailOf j "default_mail" "vault"
    let fwd ← targetsFor mail (j.getObjVal? "default_forward_to").toOption own "default_forward_to"
    let sending ← sendingOf name (j.getObjVal? "sending").toOption []
    return (mail, fwd, sending)
  match r with
  | .error e => return .error e
  | .ok (mail, fwd, sending) =>
    let now ← Clock.nowMs
    st.db.exec "INSERT INTO domains (name, manage, default_mail, default_forward_to, sending, created_at, updated_at)
      VALUES (?, ?, ?, ?, ?, ?, ?)" #[name, (jbool? j "manage").getD true, mail, strListJson fwd, strListJson sending, now, now]
    Log.info "routing.domain_added" []
    return .ok ()

def patchDomain (st : State) (name : String) (j : Json) : IO (Except (Nat × String) Unit) := do
  let some d := (← loadDomains st).find? (·.name == name) | return .error (404, "not_found")
  let own ← ownDomains st
  let r : Except (Nat × String) (String × List String × List String) := do
    let mail ← mailOf j "default_mail" d.defaultMail
    let fwdRaw := ((j.getObjVal? "default_forward_to").toOption).getD (Json.arr (d.defaultForwardTo.toArray.map Json.str))
    let fwd ← targetsFor mail (some fwdRaw) own "default_forward_to"
    let sending ← sendingOf name (j.getObjVal? "sending").toOption d.sending
    return (mail, fwd, sending)
  match r with
  | .error e => return .error e
  | .ok (mail, fwd, sending) =>
    st.db.exec "UPDATE domains SET manage = ?, default_mail = ?, default_forward_to = ?, sending = ?, updated_at = ? WHERE name = ?"
      #[(jbool? j "manage").getD d.manage, mail, strListJson fwd, strListJson sending, ← Clock.nowMs, name]
    return .ok ()

def deleteDomain (st : State) (name : String) : IO Bool := do
  st.db.transaction do
    st.db.execUnlocked "DELETE FROM addresses WHERE domain = ?" #[name]
    st.db.execUnlocked "DELETE FROM domains WHERE name = ?" #[name]
    return (← Db.changesRaw st.db.conn) > 0

def putAddress (st : State) (address : String) (j : Json) : IO (Except (Nat × String) Unit) := do
  let some a := normEmail? address | return bad "address"
  if a.startsWith "*" then return bad "address" "wildcard"
  let dom := Blocks.domainOf a
  let ds ← loadDomains st
  if !ds.any (·.name == dom) then return bad "address" "unknown_domain"
  let cur := (← loadAddresses st).find? (·.address == a)
  let r : Except (Nat × String) (String × List String) := do
    let mail ← mailOf j "mail" ((cur.map (·.mail)).getD "vault")
    let fwdRaw := ((j.getObjVal? "forward_to").toOption).getD (Json.arr (((cur.map (·.forwardTo)).getD []).toArray.map Json.str))
    let fwd ← targetsFor mail (some fwdRaw) (ds.map (·.name)) "forward_to"
    return (mail, fwd)
  match r with
  | .error e => return .error e
  | .ok (mail, fwd) =>
    let now ← Clock.nowMs
    let note := String.ofList (((jstr? j "note").getD ((cur.map (·.note)).getD "")).toList.take 500)
    st.db.exec "INSERT INTO addresses (address, domain, mail, forward_to, can_send, note, created_at, updated_at)
      VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?7)
      ON CONFLICT(address) DO UPDATE SET mail = ?3, forward_to = ?4, can_send = ?5, note = ?6, updated_at = ?7"
      #[a, dom, mail, strListJson fwd, (jbool? j "can_send").getD ((cur.map (·.canSend)).getD true), note, now]
    return .ok ()

def deleteAddress (st : State) (address : String) : IO Bool := do
  return (← st.db.execCount "DELETE FROM addresses WHERE address = ?" #[address.toLower]) > 0

/-! ## Cloudflare, read -/

structure Rule where
  id : String
  name : String := ""
  enabled : Bool := true
  matcher : String := "all"
  action : String := ""
  target : Option String := none
  deriving Repr, BEq, Inhabited

structure Sending where
  id : String
  name : String
  enabled : Bool
  preview : Bool
  deriving Repr, BEq, Inhabited

structure Zone where
  name : String
  zoneId : Option String := none
  readable : Bool := false
  error : Option String := none
  routingEnabled : Bool := true
  routingStatus : Option String := none
  catchAll : Option Rule := none
  rules : List Rule := []
  sending : List Sending := []
  deriving Repr, Inhabited

structure Actual where
  ok : Bool
  error : Option String := none
  zones : List Zone := []
  /-- Destination addresses and whether each is verified; `none` when not read. Used only to
  check this account's own forward targets; never shown. -/
  destinations : Option (List (String × Bool)) := none
  calls : Nat := 0
  deriving Inhabited

/-- Cloudflare ids go into API paths: hex-ish only. -/
def cfIdOk (v : String) : Bool := !v.isEmpty && v.length ≤ 64 && v.toList.all Char.isAlphanum
def cfRuleIdOk (v : String) : Bool :=
  !v.isEmpty && v.length ≤ 128 && (v.splitOn "..").length == 1 &&
    v.toList.all fun c => c.isAlphanum || "@._-".toList.contains c

/-- One API call: the envelope's `result`, or an error code (`no_token`, `forbidden`, `http_<n>`, `api_<code>`). -/
def cf (st : State) (method path : String) (body : Option Json := none) : IO (Except String Json) := do
  let c := st.config
  if !c.hasCloudflare then return .error "no_token"
  try
    let r ← st.cloudflare.request {
      method, url := c.cfApiBase ++ path, timeoutMs := 20000
      headers := [("Authorization", s!"Bearer {c.cfApiToken}"), ("Content-Type", "application/json")]
      body := (body.map (·.compress.toUTF8)).getD .empty }
    if r.status == 403 then return .error "forbidden"
    if r.status < 200 || r.status ≥ 300 then return .error s!"http_{r.status}"
    match Json.parse ((String.fromUTF8? r.body).getD "") with
    | .ok env =>
      if (env.getObjValAs? Bool "success").toOption == some false then
        let code := ((env.getObjValAs? (Array Json) "errors").toOption.bind (·[0]?)).bind fun e =>
          (e.getObjVal? "code").toOption.map (·.compress)
        return .error s!"api_{code.getD "error"}"
      return .ok ((env.getObjVal? "result").toOption.getD (Json.arr #[]))
    | .error _ => return .error "bad_json"
  catch _ => return .error "unreachable"

def jArr (j : Json) : Array Json := (j.getArr?).toOption.getD #[]

def ruleOf (r : Json) : Rule :=
  let ms := jArr ((r.getObjVal? "matchers").toOption.getD .null)
  let as := jArr ((r.getObjVal? "actions").toOption.getD .null)
  let a := as[0]?
  let value : Option String := a.bind fun a => match a.getObjVal? "value" with
    | .ok (.arr xs) => xs[0]?.bind fun | .str s => some s | _ => none
    | .ok (.str s) => some s
    | _ => none
  { id := ((jstr? r "tag").orElse fun _ => jstr? r "id").getD ""
    name := (jstr? r "name").getD ""
    enabled := (jbool? r "enabled").getD true
    matcher := (ms[0]?.bind fun m => jstr? m "value").getD "all"
    action := (a.bind fun a => jstr? a "type").getD ""
    target := value }

def readZone (st : State) (d : Domain) (zid : String) : IO (Zone × Nat) := do
  let z : Zone := { name := d.name, zoneId := some zid }
  if !cfIdOk zid then return ({ z with error := some "bad_id" }, 0)
  let fail (e : String) (n : Nat) : IO (Zone × Nat) := do
    Log.warn "routing.zone_read_failed" [("error", Json.str e)]
    return ({ z with error := some e }, n)
  let .ok s ← cf st "GET" s!"/zones/{zid}/email/routing" | fail "settings" 1
  let mut rules : Array Rule := #[]
  let mut calls := 1
  for page in [1:11] do
    match ← cf st "GET" s!"/zones/{zid}/email/routing/rules?page={page}&per_page=50" with
    | .error e => return ← fail e (calls + 1)
    | .ok rs =>
      calls := calls + 1
      let rs := jArr rs
      rules := rules ++ rs.map ruleOf
      if rs.size < 50 then break
  let .ok ca ← cf st "GET" s!"/zones/{zid}/email/routing/rules/catch_all" | fail "catch_all" (calls + 1)
  calls := calls + 1
  let catchAll := ruleOf ca
  let mut sending : List Sending := []
  if !d.sending.isEmpty then
    let .ok ss ← cf st "GET" s!"/zones/{zid}/email/sending/subdomains" | return ← fail "sending" (calls + 1)
    calls := calls + 1
    sending := (jArr ss).toList.map fun x => {
      id := (jstr? x "id").getD "", name := ((jstr? x "name").getD "").toLower
      enabled := (jbool? x "enabled").getD true, preview := (jbool? x "preview_enabled").getD false }
  return ({ z with readable := true, routingEnabled := (jbool? s "enabled").getD true, routingStatus := jstr? s "status",
                   catchAll := if catchAll.action.isEmpty then none else some catchAll,
                   rules := (rules.filter (·.matcher != "all")).toList, sending }, calls)

/-- What Cloudflare has for the vault's domains, those it was granted only (an ungranted one is
not read: `zone_unreadable` with `domain_not_granted`). Never throws. -/
def readActual (st : State) (ds : List Domain) (granted : Option (List String) := none) : IO Actual := do
  if !st.config.hasCloudflare then return { ok := false, error := some "no_token" }
  let .ok zs ← cf st "GET" "/zones?per_page=50" | return { ok := false, error := some "cf_error" }
  let zones := (jArr zs).toList.map fun z => (((jstr? z "name").getD "").toLower, (jstr? z "id").getD "")
  let dests ← cf st "GET" s!"/accounts/{st.config.cfAccountId}/email/routing/addresses?per_page=50"
  let destinations := match dests with
    | .ok xs => some ((jArr xs).toList.map fun x => (((jstr? x "email").getD "").toLower, (x.getObjVal? "verified").toOption.any (· != .null)))
    | .error _ => none
  let mut out : Array Zone := #[]
  let mut calls := 2
  for d in ds do
    if !grantOk granted d.name then
      out := out.push { name := d.name, zoneId := some "", error := some "domain_not_granted" }
      continue
    match zones.lookup d.name with
    | some zid =>
      if d.manage then
        let (z, n) ← readZone st d zid
        out := out.push z
        calls := calls + n
      else out := out.push { name := d.name, zoneId := some zid }
    | none => out := out.push { name := d.name }
  return { ok := true, zones := out.toList, destinations, calls }

/-! ## The plan (pure) -/

structure Op where
  key : String
  kind : String
  domain : String
  zoneId : String
  address : Option String := none
  ruleId : Option String := none
  sendingId : Option String := none
  danger : Bool := false
  text : String
  deriving Repr, BEq, Inhabited

structure Unmanaged where
  domain : String
  zoneId : String
  rule : Rule
  text : String
  deriving Repr, Inhabited

structure Problem where
  severity : String
  code : String
  domain : Option String := none
  text : String
  deriving Repr, Inhabited

structure Plan where
  ok : Bool
  error : Option String
  ops : List Op
  unmanaged : List Unmanaged
  problems : List Problem
  /-- Made against zones assumed empty: there was no token to read them with. -/
  assumed : Bool := false
  id : String := ""
  deriving Inhabited

def prob (severity code dom text : String) : Problem := { severity, code, domain := some dom, text }

def kindOrder : List String := ["catch_all.set", "rule.delete", "preview.off"]

def describe (r : Option Rule) (worker : String) : String :=
  match r with
  | none => "nothing"
  | some r =>
    if r.action == "worker" then (if r.target == some worker then "to this LeanMail Worker" else s!"to the {r.target.getD "?"} Worker")
    else if r.action == "forward" then s!"forwarded to {r.target.getD "?"}"
    else if r.action == "drop" then "dropped" else r.action

def reachesWorker (r : Rule) (worker : String) : Bool :=
  r.enabled && r.action == "worker" && (r.target.map (·.toLower)) == some worker.toLower

def isOwnRule (r : Rule) : Bool := ownRulePrefixes.any (r.name.startsWith ·)

def plan (ds : List Domain) (as : List Address) (a : Actual) (worker : String) (assumed : Bool := false) : Plan := Id.run do
  let mut ops : Array Op := #[]
  let mut unmanaged : Array Unmanaged := #[]
  let mut problems : Array Problem := #[]
  if !a.ok && !assumed then return { ok := false, error := a.error, ops := [], unmanaged := [], problems := [] }
  for d in ds do
    if !d.manage then continue
    let some z := a.zones.find? (·.name == d.name) | continue
    if z.zoneId.isNone then
        problems := problems.push (prob "warn" "not_on_cloudflare" d.name s!"{d.name} is not a zone on this Cloudflare account. If its mail reaches LeanMail some other way, mark it not managed here.")
        continue
    let zid := z.zoneId.getD ""
    if !z.readable then
      problems := problems.push (prob "warn" "zone_unreadable" d.name s!"Cloudflare could not be read for {d.name} ({z.error.getD "error"}); nothing about it is planned until it can.")
      continue
    if !z.routingEnabled then
      problems := problems.push (prob "bad" "routing_disabled" d.name s!"Email Routing is off for {d.name} at Cloudflare, so none of its mail arrives. Turn it on in the Cloudflare dashboard.")
      continue
    if let some s := z.routingStatus then
      if s != "ready" then
        problems := problems.push (prob "bad" "routing_not_ready" d.name s!"Cloudflare says mail for {d.name} cannot arrive yet ({s}): its MX records do not point at Cloudflare.")
        continue
    if !(z.catchAll.any (reachesWorker · worker)) then
      ops := ops.push
        { key := s!"catch_all:{d.name}", kind := "catch_all.set", domain := d.name, zoneId := zid, danger := true,
          text := s!"Hand all mail for {d.name} to the {worker} Worker, which decides each address (now: {describe z.catchAll worker})." }
    for r in z.rules do
      let addr := r.matcher.toLower
      if isOwnRule r then
        ops := ops.push
          { key := s!"delete:{r.id}", kind := "rule.delete", domain := d.name, zoneId := zid, address := (some addr),
            ruleId := some r.id, danger := !(r.action == "worker"),
            text := s!"Delete the Cloudflare rule for {addr} ({describe (some r) worker}); LeanMail decides it from its own routing." }
      else
        unmanaged := unmanaged.push
          { domain := d.name, zoneId := zid, rule := r,
            text := s!"{addr}: {describe (some r) worker}{if r.enabled then "" else " (switched off)"}" }
    for name in d.sending do
      match z.sending.find? (·.name == name) with
      | none =>
        if !assumed then
          problems := problems.push (prob "warn" "sending_not_onboarded" d.name s!"{name} is not set up for Email Sending at Cloudflare, so mail cannot be sent from it.")
      | some s =>
        if !s.enabled then
          problems := problems.push (prob "warn" "sending_disabled" d.name s!"Email Sending is switched off for {name} at Cloudflare.")
        else if s.preview then
          ops := ops.push
            { key := s!"preview:{name}", kind := "preview.off", domain := d.name, zoneId := zid, sendingId := (some s.id),
              text := s!"Turn off Email preview for {name}: Cloudflare keeps the full text of everything sent from it for about a week." }
  -- forward targets Cloudflare has not verified: accepted, never delivered
  if let some dests := a.destinations then
    let verified := dests.filter (·.2) |>.map (·.1)
    let check (targets : List String) (dom : String) (where_ : String) (ps : Array Problem) : Array Problem :=
      targets.foldl (init := ps) fun ps t =>
        if verified.contains t then ps else ps.push (prob "bad" "unverified_destination" dom s!"{t} is not a verified destination at Cloudflare, so mail forwarded to it from {where_} is never delivered.")
    for d in ds do
      if d.manage && forwardModes.contains d.defaultMail then
        problems := check d.defaultForwardTo d.name s!"{d.name}'s other addresses" problems
    for x in as do
      if (ds.find? (·.name == x.domain)).any (·.manage) && forwardModes.contains x.mail then
        problems := check x.forwardTo x.domain x.address problems
  let idx (k : String) := (kindOrder.idxOf k)
  let sorted := ops.qsort fun x y => x.domain < y.domain || (x.domain == y.domain &&
    (idx x.kind < idx y.kind || (idx x.kind == idx y.kind && x.key < y.key)))
  return { ok := a.ok, error := a.error, ops := sorted.toList, unmanaged := unmanaged.toList, problems := problems.toList, assumed }

/-- The plan's identity: a hash of what apply would do and of the rules it lists as not managed. -/
def planId (p : Plan) : String :=
  let o := p.ops.map fun o => s!"{o.kind}|{o.zoneId}|{o.ruleId.getD ""}|{o.address.getD ""}|{o.sendingId.getD ""}"
  let u := p.unmanaged.map fun u => s!"{u.zoneId}|{u.rule.id}|{u.rule.enabled}|{u.rule.action}|{u.rule.target.getD ""}"
  String.ofList ((Crypto.sha256Hex ("\n".intercalate (o ++ ["--"] ++ u)).toUTF8).toList.take 16)

def optJ (o : Option String) : Json := match o with | some s => Json.str s | none => Json.null

def Rule.toJson (r : Rule) : Json := Json.mkObj [("id", Json.str r.id), ("name", Json.str r.name), ("enabled", Json.bool r.enabled),
  ("matcher", Json.str r.matcher), ("action", Json.str r.action), ("target", optJ r.target)]

def Plan.toJson (p : Plan) (canApply : Bool) : Json := Json.mkObj [
  ("ok", Json.bool p.ok), ("error", optJ p.error), ("id", Json.str p.id), ("assumed", Json.bool p.assumed),
  ("can_apply", Json.bool canApply),
  ("ops", Json.arr (p.ops.toArray.map fun o => Json.mkObj [("key", Json.str o.key), ("kind", Json.str o.kind),
    ("domain", Json.str o.domain), ("zone_id", Json.str o.zoneId), ("address", optJ o.address), ("rule_id", optJ o.ruleId),
    ("sending_id", optJ o.sendingId), ("danger", Json.bool o.danger), ("text", Json.str o.text)])),
  ("unmanaged", Json.arr (p.unmanaged.toArray.map fun u => Json.mkObj [("domain", Json.str u.domain),
    ("zone_id", Json.str u.zoneId), ("rule", u.rule.toJson), ("text", Json.str u.text)])),
  ("problems", Json.arr (p.problems.toArray.map fun x => Json.mkObj [("severity", Json.str x.severity),
    ("code", Json.str x.code), ("domain", optJ x.domain), ("text", Json.str x.text)]))]

def optStr? (j : Json) (k : String) : Option String := jstr? j k

/-- A plan as `Plan.toJson` wrote it: what the operator answers an account container. -/
def Plan.ofJson? (j : Json) : Option Plan := do
  let ok ← jbool? j "ok"
  let ops ← (j.getObjValAs? (Array Json) "ops").toOption
  let un ← (j.getObjValAs? (Array Json) "unmanaged").toOption
  let ps ← (j.getObjValAs? (Array Json) "problems").toOption
  let ops := ops.toList.map fun o => ({
    key := (jstr? o "key").getD "", kind := (jstr? o "kind").getD "", domain := (jstr? o "domain").getD ""
    zoneId := (jstr? o "zone_id").getD "", address := optStr? o "address", ruleId := optStr? o "rule_id"
    sendingId := optStr? o "sending_id", danger := (jbool? o "danger").getD false, text := (jstr? o "text").getD "" } : Op)
  let un := un.toList.map fun u =>
    let r := (u.getObjVal? "rule").toOption.getD Json.null
    ({ domain := (jstr? u "domain").getD "", zoneId := (jstr? u "zone_id").getD "", text := (jstr? u "text").getD ""
       rule := { id := (jstr? r "id").getD "", name := (jstr? r "name").getD "", enabled := (jbool? r "enabled").getD true
                 matcher := (jstr? r "matcher").getD "", action := (jstr? r "action").getD "", target := optStr? r "target" } } : Unmanaged)
  let ps := ps.toList.map fun x => ({ severity := (jstr? x "severity").getD "", code := (jstr? x "code").getD ""
                                      domain := optStr? x "domain", text := (jstr? x "text").getD "" } : Problem)
  return { ok, error := optStr? j "error", ops, unmanaged := un, problems := ps,
           assumed := (jbool? j "assumed").getD false, id := (jstr? j "id").getD "" }

/-- The domains of `all` that `granted` allows, and their addresses. -/
def grantedRouting (all : List Domain) (allAs : List Address) (granted : Option (List String)) : List Domain × List Address :=
  let ds := all.filter (grantOk granted ·.name)
  let names := ds.map (·.name)
  (ds, allAs.filter (names.contains ·.domain))

/-- The managed domains of `all` that are not granted, as problems. -/
def refusedProblems (all : List Domain) (granted : Option (List String)) : List Problem :=
  (all.filter fun d => d.manage && !grantOk granted d.name).map fun d =>
    prob "bad" "domain_not_granted" d.name s!"{d.name} is not granted to this account, so nothing about it is read or changed at Cloudflare. Ask the operator to grant it."

/-- The plan against zones assumed empty (no token to read them with). -/
def assumedPlan (ds : List Domain) (as : List Address) (worker err : String) : Plan :=
  let zones := ds.map fun d => ({ name := d.name, zoneId := some "", readable := true } : Zone)
  let p := { plan ds as { ok := false, error := some err, zones } worker (assumed := true) with ok := false }
  { p with id := planId p }

/-- A plan for these domains and addresses (already the granted ones) from a fresh read with this
container's token; without one, against zones assumed empty. -/
def planWith (st : State) (ds : List Domain) (as : List Address) (granted : Option (List String)) : IO Plan := do
  let worker := st.config.cfWorker
  let p ← if st.config.hasCloudflare then do
      let p := plan ds as (← readActual st ds granted) worker
      pure { p with id := planId p }
    else pure (assumedPlan ds as worker (if needsOperator st.config then "routing_needs_operator" else "no_token"))
  Log.info "routing.planned" [("ops", Json.num p.ops.length), ("problems", Json.num p.problems.length),
    ("assumed", Json.bool p.assumed)]
  return p

/-- What an account container sends the operator: its granted routing. -/
def routingBody (ds : List Domain) (as : List Address) (extra : List (String × Json) := []) : Json :=
  Json.mkObj ([("domains", Json.arr (ds.toArray.map Domain.toJson)), ("addresses", Json.arr (as.toArray.map Address.toJson))] ++ extra)

/-- A plan from a fresh read. Only granted domains are planned (`grantedDomains`); each other
managed one is a `domain_not_granted` problem. An account container (`needsOperator`) has the
operator make it (`POST /internal/routing/plan` through the Worker); when that fails, the plan
against zones assumed empty, with the error `routing_needs_operator`. -/
def planNow (st : State) : IO Plan := do
  let granted ← grantedDomains st
  let all ← loadDomains st
  let (ds, as) := grantedRouting all (← loadAddresses st) granted
  let p ← if needsOperator st.config then do
      let r ← st.worker.callStatus "POST" "/internal/routing/plan" (routingBody ds as)
      match r.bind fun (s, j) => if s == 200 then Plan.ofJson? j else none with
      | some p => pure p
      | none =>
        Log.warn "routing.operator_unreachable" [("status", match r with | some (s, _) => Json.num s | none => Json.null)]
        pure (assumedPlan ds as st.config.cfWorker "routing_needs_operator")
    else planWith st ds as granted
  -- the refused domains change nothing apply would do, so the id stays
  return { p with problems := p.problems ++ refusedProblems all granted }

/-- Whether a plan answered here can be applied: with this container's token, or made by the
operator for this account. -/
def canApplyPlan (c : Config) (p : Plan) : Bool := c.hasCloudflare || (needsOperator c && p.ok && !p.assumed)

/-! ## Apply -/

structure Applied where
  key : String
  ok : Bool
  error : Option String := none
  skipped : Bool := false
  deriving Repr, Inhabited

def runOp (st : State) (o : Op) : IO (Except String Unit) := do
  if !cfIdOk o.zoneId then return .error "bad_id"
  let r ← match o.kind with
    | "catch_all.set" =>
      cf st "PUT" s!"/zones/{o.zoneId}/email/routing/rules/catch_all" (some (Json.mkObj [
        ("name", Json.str "catch-all"), ("enabled", Json.bool true),
        ("matchers", Json.arr #[Json.mkObj [("type", Json.str "all")]]),
        ("actions", Json.arr #[Json.mkObj [("type", Json.str "worker"), ("value", Json.arr #[Json.str st.config.cfWorker])]])]))
    | "rule.delete" =>
      let rid := o.ruleId.getD ""
      if !cfRuleIdOk rid then pure (.error "bad_id") else cf st "DELETE" s!"/zones/{o.zoneId}/email/routing/rules/{rid}"
    | "preview.off" =>
      let sid := o.sendingId.getD ""
      if !cfIdOk sid then pure (.error "bad_id")
      else cf st "PATCH" s!"/zones/{o.zoneId}/email/sending/subdomains/{sid}" (some (Json.mkObj [("preview_enabled", Json.bool false)]))
    | _ => pure (.error "unknown_op")
  return r.map fun _ => ()

/-- Make the changes, a domain at a time; the first failure in a domain stops the rest of it. -/
def runAll (st : State) (ops : List Op) : IO (Array Applied) := do
  let mut failed : List String := []
  let mut out : Array Applied := #[]
  for o in ops do
    if failed.contains o.domain then
      out := out.push { key := o.key, ok := false, skipped := true }
      continue
    match ← runOp st o with
    | .ok () => out := out.push { key := o.key, ok := true }
    | .error e =>
      failed := o.domain :: failed
      out := out.push { key := o.key, ok := false, error := some e }
      Log.warn "routing.apply_failed" [("kind", Json.str o.kind), ("error", Json.str e)]
  return out

def appliedJson (a : Applied) : Json := Json.mkObj [("key", Json.str a.key), ("ok", Json.bool a.ok),
  ("error", optJ a.error), ("skipped", Json.bool a.skipped)]

/-- Apply the plan the owner saw, for these (granted) domains and addresses, with this
container's token: made again first, refused (`plan_changed`) unless it is the same; unmanaged
rules are removed only when named in `remove` and still listed. `{applied, plan}`. -/
def applyWith (st : State) (ds : List Domain) (as : List Address) (granted : Option (List String))
    (planId? : Option String) (remove : List String) : IO (Except (Nat × String × Json) Json) := do
  if !st.config.hasCloudflare then return .error (503, "no_cf_token", Json.null)
  let fresh ← planWith st ds as granted
  let listed := fresh.unmanaged.map (·.rule.id)
  if !fresh.ok || (planId?.any (· != fresh.id)) || remove.any (!listed.contains ·) then
    return .error (409, "plan_changed", fresh.toJson true)
  let removals := fresh.unmanaged.filter (remove.contains ·.rule.id) |>.map fun u =>
    ({ key := s!"remove:{u.rule.id}", kind := "rule.delete", domain := u.domain, zoneId := u.zoneId, ruleId := (some u.rule.id),
       address := some u.rule.matcher, danger := true, text := s!"Remove the Cloudflare rule for {u.rule.matcher}." } : Op)
  let idx (k : String) := (kindOrder.idxOf k)
  let todo := (fresh.ops ++ removals).toArray.qsort (fun x y => x.domain < y.domain || (x.domain == y.domain && idx x.kind < idx y.kind))
  let applied ← runAll st todo.toList
  Log.info "routing.applied" [("ops", Json.num applied.size), ("failed", Json.num (applied.filter (!·.ok)).size)]
  let after ← planWith st ds as granted
  return .ok (Json.mkObj [("applied", Json.arr (applied.map appliedJson)), ("plan_id", Json.str fresh.id),
    ("plan", after.toJson true)])

/-- Apply the plan the owner saw (`applyWith`), here or, in an account container, at the
operator (`POST /internal/routing/apply` through the Worker); the outcome is kept as
`routing.last_apply`. -/
def apply (st : State) (planId? : Option String) (remove : List String) : IO (Except (Nat × String × Json) Json) := do
  let granted ← grantedDomains st
  let (ds, as) := grantedRouting (← loadDomains st) (← loadAddresses st) granted
  let r ← if needsOperator st.config then do
      let extra := [("plan_id", match planId? with | some i => Json.str i | none => Json.null),
                    ("remove", Json.arr (remove.toArray.map Json.str))]
      match ← st.worker.callStatus "POST" "/internal/routing/apply" (routingBody ds as extra) with
      | some (200, j) =>
        if (j.getObjValAs? (Array Json) "applied").isOk then pure (.ok j)
        else pure (.error (503, "routing_needs_operator", Json.null))
      | some (s, j) =>
        match jstr? j "error" with
        | some code => pure (.error (s, code, (j.getObjVal? "plan").toOption.getD Json.null))
        | none => pure (.error (503, "routing_needs_operator", Json.null))
      | none => pure (.error (503, "routing_needs_operator", Json.null))
    else applyWith st ds as granted planId? remove
  if let .ok j := r then
    let now ← Clock.nowMs
    let summary := Json.mkObj [("at", Json.num now), ("plan_id", (j.getObjVal? "plan_id").toOption.getD Json.null),
      ("applied", (j.getObjVal? "applied").toOption.getD (Json.arr #[]))]
    st.db.exec "INSERT INTO settings (key, value, updated_at) VALUES ('routing.last_apply', ?1, ?2)
      ON CONFLICT(key) DO UPDATE SET value = ?1, updated_at = ?2" #[summary.compress, now]
  return r

/-! ## The operator's side of an account's plan and apply -/

/-- The account a forwarded plan or apply is for, its routing from the body, and its grants
from `domain_grants`; refused unless this is the operator container (404), the Worker named the
account (`x-routing-account`, 400) and every domain in the body is granted to it (403). -/
def opRouting (c : Ctx) (req : Req) : IO (Except Resp (String × List Domain × List Address × List String)) := do
  if !c.st.config.isOperator then return .error Resp.notFound
  let some acct := (req.header? "x-routing-account").map Core.Text.Str.trim | return .error (Resp.error 400 "no routing account")
  if acct.isEmpty then return .error (Resp.error 400 "no routing account")
  let j := req.json
  let some dsJ := (j.getObjValAs? (Array Json) "domains").toOption | return .error (Resp.error 400 "bad domains")
  let asJ := (j.getObjValAs? (Array Json) "addresses").toOption.getD #[]
  if dsJ.size > 100 || asJ.size > 5000 then return .error (Resp.error 400 "too many")
  let ds := dsJ.toList.filterMap Domain.ofJson?
  if ds.length != dsJ.size then return .error (Resp.error 400 "bad domains")
  let granted := (← c.st.db.query "SELECT domain FROM domain_grants WHERE account_id = ?" #[acct]).toList.map (·.text "domain")
  if let some d := ds.find? (!granted.contains ·.name) then
    Log.warn "routing.op_not_granted" [("account", Json.str acct)]
    return .error (Resp.json 403 (Json.mkObj [("error", Json.str "domain_not_granted"), ("domain", Json.str d.name)]))
  let names := ds.map (·.name)
  let as := (asJ.toList.filterMap Address.ofJson?).filter (names.contains ·.domain)
  return .ok (acct, ds, as, granted)

/-- `POST /internal/routing/plan {domains, addresses}` with `x-routing-account` (operator only). -/
def opPlan (c : Ctx) (req : Req) : IO Resp := do
  match ← opRouting c req with
  | .error r => return r
  | .ok (_, ds, as, granted) =>
    return Resp.ok ((← planWith c.st ds as (some granted)).toJson c.st.config.hasCloudflare)

/-- `POST /internal/routing/apply {domains, addresses, plan_id?, remove?}` with `x-routing-account`. -/
def opApply (c : Ctx) (req : Req) : IO Resp := do
  match ← opRouting c req with
  | .error r => return r
  | .ok (acct, ds, as, granted) =>
    let j := req.json
    let remove := ((j.getObjValAs? (Array String) "remove").toOption.getD #[]).toList
    match ← applyWith c.st ds as (some granted) (jstr? j "plan_id") remove with
    | .ok r =>
      Log.info "routing.op_applied" [("account", Json.str acct)]
      return Resp.ok r
    | .error (s, code, p) => return Resp.json s (Json.mkObj [("error", Json.str code), ("plan", p)])

/-- The operator container's routes for the Worker's forwards. -/
def operatorRoutes : Array Route := #[
  route "POST" "/internal/routing/plan" .internal opPlan,
  route "POST" "/internal/routing/apply" .internal opApply
]

/-! ## Routes -/

def errResp (e : Nat × String) : Resp := Resp.error e.1 e.2

def configGet (c : Ctx) (_ : Req) : IO Resp := return Resp.ok (← configJson c.st)

def domainsPost (c : Ctx) (req : Req) : IO Resp := do
  match ← addDomain c.st req.json with
  | .error e => return errResp e
  | .ok () => changed c.st 201

def domainPatch (c : Ctx) (req : Req) : IO Resp := do
  match ← patchDomain c.st (c.param "name").toLower req.json with
  | .error e => return errResp e
  | .ok () => changed c.st

def domainDelete (c : Ctx) (_ : Req) : IO Resp := do
  if !(← deleteDomain c.st (c.param "name").toLower) then return Resp.notFound
  changed c.st

def addressPut (c : Ctx) (req : Req) : IO Resp := do
  match ← putAddress c.st (c.param "address") req.json with
  | .error e => return errResp e
  | .ok () => changed c.st

def addressDelete (c : Ctx) (_ : Req) : IO Resp := do
  if !(← deleteAddress c.st (c.param "address")) then return Resp.notFound
  changed c.st

def planGet (c : Ctx) (_ : Req) : IO Resp := do
  let p ← planNow c.st
  let j := p.toJson (canApplyPlan c.st.config p)
  -- an account container whose operator did not answer
  if needsOperator c.st.config && p.error == some "routing_needs_operator" then
    return Resp.json 503 (Json.mkObj [("error", Json.str "routing_needs_operator"), ("plan", j)])
  return Resp.ok j

def applyPost (c : Ctx) (req : Req) : IO Resp := do
  let j := req.json
  let remove := ((j.getObjValAs? (Array String) "remove").toOption.getD #[]).toList
  match ← apply c.st (jstr? j "plan_id") remove with
  | .ok r => return Resp.ok r
  | .error (s, code, p) => return Resp.json s (Json.mkObj [("error", Json.str code), ("plan", p)])

def sendableGet (c : Ctx) (_ : Req) : IO Resp := do
  return Resp.ok (Json.mkObj [("addresses", Json.arr ((← Send.fromChoices c.st).toArray.map Json.str))])

def routes : Array Route := #[
  route "GET" "/api/config" (.token "read") configGet,
  route "GET" "/api/config/plan" .session planGet,
  route "POST" "/api/config/apply" .session applyPost,
  route "POST" "/api/config/domains" .session domainsPost,
  route "PATCH" "/api/config/domains/:name" .session domainPatch,
  route "DELETE" "/api/config/domains/:name" .session domainDelete,
  route "PUT" "/api/config/addresses/:address" .session addressPut,
  route "DELETE" "/api/config/addresses/:address" .session addressDelete,
  route "GET" "/api/sendable" (.token "read") sendableGet
]

end Server.Routing
