import Lean.Data.Json
import Core.Ulid
import Server.Router.Types
import Server.Worker
import Server.Clock

/-! # The operator: sign-ups, approvals, domain grants

Active in the `_operator` container (`ROLE=operator`); its routes do not exist anywhere else.
Its tables (`accounts`, `signups`, `domain_grants`, `audit`) share the migration list with the
account database and are simply empty in an account container.

## Contract with the Worker

- `POST /auth/signup {email, note?}` is public. The Worker forwards it to the operator container
  as it forwards anything, stamping `x-account-id: _operator` (and the operator's
  `X-Internal-Secret`), and passes the client's `cf-connecting-ip` through; the rate limit is
  10 sign-ups an hour per `sha256(ip)`. Always `202 {ok:true}`, `404` when `SIGNUP_MODE=closed`.
- `/admin/*`: an administrator's session lives in THEIR account container, which the operator
  cannot look up. So the Worker verifies the `lm_session` cookie's MAC itself, takes the account
  id from it, strips any client-sent `x-admin-account`, and forwards to the operator container with
  `x-account-id: _operator`, `X-Internal-Secret: <operator's derived secret>` (the Durable Object
  stamps both) and `x-admin-account: <acct>`. The operator then requires a valid
  `X-Internal-Secret` and that `<acct>` is an `active` row in its `accounts` whose `is_admin` is
  set or whose email is in `ADMIN_EMAILS`. (The Worker only checks the MAC, not revocation; that
  is the accepted trade for not waking the admin's container on every admin request.) In a bare
  dev run the operator's own session (an `account` row that is admin) and `DEV_AUTH_BYPASS` also
  pass.
- On approve, disable and domain changes the operator calls the Worker:
  `PUT /internal/accounts {accounts:[{id,email,status}], domains:[{name,account_id}],
  replace_domains_for:[id]}` (the domains listed are ALL of that account's grants; the Worker
  should drop the account's other `domains` rows) and, on approve,
  `POST /internal/wake/<id> {provision:{email, is_admin}}`, which the Worker turns into
  `POST /internal/provision {email, is_admin}` on the new account's container.
- Bootstrap: a sign-up from an `ADMIN_EMAILS` address is approved at once, as an admin, whatever
  `SIGNUP_MODE` says (closed excepted), so a fresh install's operator can approve the rest.
- Only a NEW account is auto-approved by a sign-up. A sign-up never changes an existing account's
  status: a `pending` one waits for an admin, a `disabled` one stays disabled (the sign-up is
  dropped, logged in `audit`).
- Delivery is recorded: `accounts.projected_at` when the Worker took the account's row,
  `provisioned_at` when the wake that provisions its container went through. Either still NULL
  is retried by a repeat sign-up and by the `pending_projection` lane on every `/internal/tick`
  (`syncPending`), so a Worker that was down for a moment cannot strand an approved account.

Every admin action writes `audit(at, actor, action, target)`. -/

namespace Server.Operator

open Lean Server.Db Server.Router

def hourMs : Nat := 3600 * 1000
def rateLimit : Nat := 10

def audit (st : State) (actor action target : String) : IO Unit := do
  st.db.exec "INSERT INTO audit(at, actor, action, target) VALUES (?, ?, ?, ?)" #[← Clock.nowMs, actor, action, target]

def actorOf (c : Ctx) : String :=
  match c.principal with
  | .admin a => a
  | .dev => "dev"
  | _ => "?"

/-- An `active` row of `accounts` that is an admin. Used by the router for `x-admin-account`. -/
def isAdminAccount (st : State) (acct : String) : IO Bool := do
  let some r ← st.db.queryOne "SELECT email, is_admin, status FROM accounts WHERE id = ?" #[acct] | return false
  return r.text "status" == "active" && (r.bool "is_admin" || st.config.isAdminEmail (r.text "email"))

/-- The operator container's own account row is an admin (bare dev, its own session). -/
def selfIsAdmin (st : State) : IO Bool := do
  let some r ← st.db.queryOne "SELECT email, is_admin FROM account WHERE id = 1" | return false
  return r.bool "is_admin" || st.config.isAdminEmail (r.text "email")

/-- Push one account and all its domain grants to the Worker's D1 projection; records
`projected_at` (NULL again when the Worker did not take it, so it is retried). -/
def project (st : State) (acct : String) : IO Bool := do
  let ok ← projectRaw st acct
  let now ← Clock.nowMs
  st.db.exec "UPDATE accounts SET projected_at = ? WHERE id = ?" #[(if ok then some now else none : Option Nat), acct]
  return ok
where
  projectRaw (st : State) (acct : String) : IO Bool := do
    let some a ← st.db.queryOne "SELECT id, email, status FROM accounts WHERE id = ?" #[acct] | return false
    let doms ← st.db.query "SELECT domain FROM domain_grants WHERE account_id = ? ORDER BY domain" #[acct]
    let body := Json.mkObj [
      ("accounts", Json.arr #[Json.mkObj [("id", Json.str (a.text "id")), ("email", Json.str (a.text "email")),
                                          ("status", Json.str (a.text "status"))]]),
      ("domains", Json.arr (doms.map fun d => Json.mkObj [("name", Json.str (d.text "domain")), ("account_id", Json.str acct)])),
      ("replace_domains_for", Json.arr #[Json.str acct])]
    return (← Worker.internal st "/internal/accounts" body (method := "PUT")).isSome

/-- The wake that provisions an active account's container; records `provisioned_at`. -/
def provision (st : State) (acct : String) : IO Bool := do
  let some a ← st.db.queryOne "SELECT email, is_admin FROM accounts WHERE id = ? AND status = 'active'" #[acct] | return false
  let isAdmin := a.bool "is_admin" || st.config.isAdminEmail (a.text "email")
  let woken ← Worker.internal st s!"/internal/wake/{acct}" (Json.mkObj [("provision", Json.mkObj [
    ("email", Json.str (a.text "email")), ("is_admin", Json.bool isAdmin)])])
  let now ← Clock.nowMs
  st.db.exec "UPDATE accounts SET provisioned_at = ? WHERE id = ?" #[(if woken.isSome then some now else none : Option Nat), acct]
  return woken.isSome

/-- Retry what has not reached the Worker for one account: its projection (any account past
`pending`) and, for an active one, its provisioning. `(projected, provisioned)` as they now stand. -/
def syncAccount (st : State) (acct : String) : IO (Bool × Bool) := do
  let some r ← st.db.queryOne "SELECT status, projected_at, provisioned_at FROM accounts WHERE id = ?" #[acct] | return (false, false)
  let status := r.text "status"
  let projected ← if status == "pending" then pure false
    else if (r.nat? "projected_at").isSome then pure true else project st acct
  let provisioned ← if status != "active" then pure false
    else if (r.nat? "provisioned_at").isSome then pure true else provision st acct
  return (projected, provisioned)

/-- The `pending_projection` lane: retry up to 20 accounts whose projection or provisioning has
not gone through. How many were tried. -/
def syncPending (st : State) : IO Nat := do
  if !st.config.isOperator then return 0
  let rows ← st.db.query "SELECT id FROM accounts WHERE (status != 'pending' AND projected_at IS NULL)
    OR (status = 'active' AND provisioned_at IS NULL) ORDER BY updated_at LIMIT 20"
  for r in rows do discard <| syncAccount st (r.text "id")
  if !rows.isEmpty then Log.info "operator.sync_pending" [("accounts", Json.num rows.size)]
  return rows.size

/-- Mark an account active and tell the Worker: projection, then a wake carrying the provision. -/
def activate (st : State) (acct actor : String) : IO (Bool × Bool) := do
  let now ← Clock.nowMs
  st.db.exec "UPDATE accounts SET status = 'active', approved_by = ?, approved_at = ?, updated_at = ?,
      projected_at = NULL, provisioned_at = NULL WHERE id = ?"
    #[actor, now, now, acct]
  let some _ ← st.db.queryOne "SELECT id FROM accounts WHERE id = ?" #[acct] | return (false, false)
  let projected ← project st acct
  let woken ← provision st acct
  return (projected, woken)

/-! ## Sign-up (public) -/

def signup (c : Ctx) (req : Req) : IO Resp := do
  let st := c.st
  let mode := st.config.signupMode
  if mode == "closed" then return Resp.notFound
  let j := req.json
  let some email := (jstr? j "email").bind normEmail? | return Resp.error 400 "bad email"
  let note := String.ofList (((jstr? j "note").getD "").toList.take 1000)
  let ipHash := Crypto.sha256Text ((req.header? "cf-connecting-ip").getD "")
  let now ← Clock.nowMs
  let recent ← st.db.queryOne "SELECT count(*) AS n FROM signups WHERE ip_hash = ? AND created_at > ?"
    #[ipHash, now - hourMs]
  if (recent.map (·.nat "n")).getD 0 ≥ rateLimit then return Resp.error 429 "too many sign-ups"
  let accepted := Resp.okStatus 202
  let existing ← st.db.queryOne "SELECT id, status FROM accounts WHERE email = ?" #[email]
  match existing with
  | some r =>
    -- an existing account's status is never changed by a sign-up
    if r.text "status" == "active" then
      -- …but what did not reach the Worker the first time is tried again
      discard <| syncAccount st (r.text "id")
      return accepted
    if r.text "status" == "disabled" then
      audit st "signup" "ignored_disabled" (r.text "id")
      return accepted
  | none => pure ()
  let sid := Core.Ulid.encode now (← Crypto.randomBytes 10)
  let boss := st.config.isAdminEmail email
  -- only a new account is approved at once
  let auto := existing.isNone && (mode == "open" || boss)
  let acct ← match existing with
    | some r => pure (r.text "id")
    | none => do pure (Core.Ulid.encode now (← Crypto.randomBytes 10))
  st.db.transaction do
    st.db.execUnlocked "INSERT INTO signups(id, email, note, ip_hash, created_at, decided_at, decision) VALUES (?, ?, ?, ?, ?, ?, ?)"
      #[sid, email, note, ipHash, now, (if auto then some now else none : Option Nat),
        (if auto then some "approved" else none : Option String)]
    st.db.execUnlocked "INSERT OR IGNORE INTO accounts(id, email, status, is_admin, created_at, updated_at) VALUES (?, ?, 'pending', ?, ?, ?)"
      #[acct, email, boss, now, now]
  if auto then
    discard <| activate st acct "signup"
    audit st "signup" (if boss then "auto_approve_admin" else "auto_approve") acct
  return accepted

/-! ## Admin -/

def signupJson (r : Row) : Json := Json.mkObj [
  ("id", Json.str (r.text "id")), ("email", Json.str (r.text "email")), ("note", Json.str (r.text "note")),
  ("created_at", Json.num (r.nat "created_at")),
  ("decided_at", match r.nat? "decided_at" with | some n => Json.num n | none => Json.null),
  ("decision", match r.text? "decision" with | some s => Json.str s | none => Json.null),
  ("account_id", match r.text? "account_id" with | some s => Json.str s | none => Json.null),
  ("account_status", match r.text? "account_status" with | some s => Json.str s | none => Json.null)]

/-- `GET /admin/signups[?status=all]`: undecided ones unless `all`. -/
def listSignups (c : Ctx) (req : Req) : IO Resp := do
  let all := req.param? "status" == some "all"
  let rows ← c.st.db.query (s!"SELECT s.*, a.id AS account_id, a.status AS account_status FROM signups s " ++
    s!"LEFT JOIN accounts a ON a.email = s.email {if all then "" else "WHERE s.decision IS NULL "}ORDER BY s.created_at DESC LIMIT 500")
  return Resp.ok (Json.mkObj [("items", Json.arr (rows.map signupJson))])

def signupAccount (st : State) (sid : String) : IO (Option (String × String)) := do
  let some r ← st.db.queryOne "SELECT a.id AS account_id, a.status AS status FROM signups s JOIN accounts a ON a.email = s.email WHERE s.id = ?" #[sid]
    | return none
  return some (r.text "account_id", r.text "status")

def approve (c : Ctx) (_ : Req) : IO Resp := do
  let st := c.st
  let sid := c.param "id"
  let some (acct, _) ← signupAccount st sid | return Resp.notFound
  let now ← Clock.nowMs
  st.db.exec "UPDATE signups SET decision = 'approved', decided_at = ? WHERE id = ?" #[now, sid]
  let (projected, woken) ← activate st acct (actorOf c)
  audit st (actorOf c) "approve" acct
  let ok := projected && woken
  return Resp.json (if ok then 200 else 502) <| Json.mkObj [("ok", Json.bool ok), ("account_id", Json.str acct),
    ("projected", Json.bool projected), ("woken", Json.bool woken)]

def reject (c : Ctx) (_ : Req) : IO Resp := do
  let st := c.st
  let sid := c.param "id"
  let some r ← st.db.queryOne "SELECT email FROM signups WHERE id = ?" #[sid] | return Resp.notFound
  let now ← Clock.nowMs
  st.db.transaction do
    st.db.execUnlocked "UPDATE signups SET decision = 'rejected', decided_at = ? WHERE id = ?" #[now, sid]
    -- a pending account goes; an active one is left to `disable`
    st.db.execUnlocked "DELETE FROM accounts WHERE email = ? AND status = 'pending'" #[r.text "email"]
  audit st (actorOf c) "reject" sid
  return Resp.okStatus

def listAccounts (c : Ctx) (_ : Req) : IO Resp := do
  let rows ← c.st.db.query "SELECT * FROM accounts ORDER BY created_at DESC"
  let grants ← c.st.db.query "SELECT domain, account_id FROM domain_grants ORDER BY domain"
  let opt (r : Row) (k : String) : Json := match r.text? k with | some s => Json.str s | none => Json.null
  let optN (r : Row) (k : String) : Json := match r.nat? k with | some n => Json.num n | none => Json.null
  return Resp.ok <| Json.mkObj [("items", Json.arr <| rows.map fun r => Json.mkObj [
    ("id", Json.str (r.text "id")), ("email", Json.str (r.text "email")), ("status", Json.str (r.text "status")),
    ("is_admin", Json.bool (r.bool "is_admin" || c.st.config.isAdminEmail (r.text "email"))),
    ("approved_by", opt r "approved_by"), ("approved_at", optN r "approved_at"),
    ("created_at", Json.num (r.nat "created_at")),
    ("domains", Json.arr (grants.filterMap fun g => if g.text "account_id" == r.text "id" then some (Json.str (g.text "domain")) else none))])]

def disable (c : Ctx) (_ : Req) : IO Resp := do
  let st := c.st
  let acct := c.param "id"
  st.db.exec "UPDATE accounts SET status = 'disabled', updated_at = ? WHERE id = ?" #[← Clock.nowMs, acct]
  if (← st.db.changes) == 0 then return Resp.notFound
  let projected ← project st acct
  audit st (actorOf c) "disable" acct
  return Resp.json (if projected then 200 else 502) (Json.mkObj [("ok", Json.bool projected)])

/-- A lower-case DNS name with at least two labels of `[a-z0-9-]`, no label starting or ending in `-`. -/
def validDomain (d : String) : Bool :=
  let labels := d.splitOn "."
  d.length ≤ 253 && labels.length ≥ 2 && labels.all fun l =>
    !l.isEmpty && l.length ≤ 63 && !l.startsWith "-" && !l.endsWith "-" &&
      l.toList.all fun ch => ('a' ≤ ch && ch ≤ 'z') || ('0' ≤ ch && ch ≤ '9') || ch == '-'

/-- `PUT /admin/accounts/:id/domains {domains:[…]}`: replace the account's grants. -/
def putDomains (c : Ctx) (req : Req) : IO Resp := do
  let st := c.st
  let acct := c.param "id"
  let some _ ← st.db.queryOne "SELECT id FROM accounts WHERE id = ?" #[acct] | return Resp.notFound
  let some (.arr xs) := (req.json.getObjVal? "domains").toOption | return Resp.error 400 "bad domains"
  let doms := (xs.toList.filterMap fun | .str s => some (Core.Text.Str.trim s).toLower | _ => none).eraseDups
  if doms.length != xs.size || doms.length > 100 || !doms.all validDomain then return Resp.error 400 "bad domains"
  for d in doms do
    if let some r ← st.db.queryOne "SELECT account_id FROM domain_grants WHERE domain = ? AND account_id != ?" #[d, acct] then
      return Resp.json 409 (Json.mkObj [("error", Json.str "domain granted to another account"), ("domain", Json.str d),
                                        ("account_id", Json.str (r.text "account_id"))])
  let now ← Clock.nowMs
  st.db.transaction do
    st.db.execUnlocked "DELETE FROM domain_grants WHERE account_id = ?" #[acct]
    for d in doms do
      st.db.execUnlocked "INSERT INTO domain_grants(domain, account_id, created_at) VALUES (?, ?, ?)" #[d, acct, now]
  let projected ← project st acct
  audit st (actorOf c) "domains" s!"{acct}:{",".intercalate doms}"
  return Resp.json (if projected then 200 else 502) (Json.mkObj [("ok", Json.bool projected),
    ("domains", Json.arr (doms.toArray.map Json.str))])

def routes : Array Route := #[
  route "POST" "/auth/signup" .«public» signup,
  route "GET" "/admin/signups" .admin listSignups,
  route "POST" "/admin/signups/:id/approve" .admin approve,
  route "POST" "/admin/signups/:id/reject" .admin reject,
  route "GET" "/admin/accounts" .admin listAccounts,
  route "POST" "/admin/accounts/:id/disable" .admin disable,
  route "PUT" "/admin/accounts/:id/domains" .admin putDomains
]

end Server.Operator
