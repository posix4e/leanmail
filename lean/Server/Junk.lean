import Lean.Data.Json
import Core.Junk
import Core.Mime
import Core.Ulid
import Server.State
import Server.Router
import Server.Api
import Server.Blocks
import Server.Ingest
import Server.Clock
import Server.Crypto
import Server.Log
import Server.Gmail.Mirror

/-! # Junk: reports, "not junk", sender blocks, one-click unsubscribe

A port of MailVault's `src/api/junk.ts` with plaintext blocks (`Server.Blocks`).

| route | auth | |
|---|---|---|
| `POST /api/msg/:id/report {kind: spam\|phishing, block?}` | token `act` | into Junk (and done, if it was not); `block` is `true`/`"address"` (the sender), `"domain"` (its domain) or `{scope, value}` |
| `POST /api/msg/:id/not-junk` | token `act` | out of Junk (back to the list if the report took it out), and the blocks that match its sender lifted |
| `POST /api/msg/:id/unsubscribe` | token `act` | RFC 8058 one-click when the message offers it, else the `mailto:`/`https:` target to show |
| `GET /api/blocks` | token `read` | the blocks, newest first |
| `POST /api/blocks {scope, value}` | token `act` | block an address or a domain (never one of the vault's own domains) |
| `DELETE /api/blocks/:id` | token `act` | lift a block; mail it filed stays in Junk |

Mail the vault sent is never junk. Each report and "not junk" goes on to Gmail through
`Gmail.Mirror.onAction` as the `junk` / `notjunk` actions do. -/

namespace Server.Junk

open Lean Server.Db Server.Router

def kinds : List String := ["spam", "phishing"]
def blocksMax : Nat := 5000

/-- An answer or a refusal (status, code). -/
abbrev Result := Except (Nat × String) Json

def blockRowJson (r : Row) : Json := Json.mkObj [
  ("id", Json.str (r.text "id")), ("scope", Json.str (r.text "scope")), ("value", Json.str (r.text "value")),
  ("created_at", Json.num (r.nat "created_at")), ("hits", Json.num (r.nat "hits")),
  ("last_hit_at", match r.nat? "last_hit_at" with | some n => Json.num n | none => Json.null)]

def junkState (r : Row) : Json := Json.mkObj [
  ("id", Json.str (r.text "id")), ("junk_at", Api.optNat r "junk_at"), ("junk_kind", Api.optText r "junk_kind"),
  ("archived_at", Api.optNat r "archived_at"), ("updated_at", Json.num (r.nat "updated_at"))]

/-- Block `value` under `scope`. The block already held for it answers when there is one. -/
def addBlock (st : State) (scope value : String) : IO (Except (Nat × String) (Row × Bool)) := do
  let some v := Blocks.normalize scope value | return .error (400, "bad_block")
  let own ← st.db.withLock (Blocks.ownDomainsUnlocked st.db)
  if scope == "domain" && Blocks.isOwn own v then return .error (400, "own_domain")
  let now ← Clock.nowMs
  let n ← st.db.withLock do
    let cnt := ((← st.db.queryUnlocked "SELECT count(*) AS n FROM sender_blocks")[0]?.map (·.nat "n")).getD 0
    if cnt ≥ blocksMax then return (0 : Nat)
    st.db.execUnlocked "INSERT OR IGNORE INTO sender_blocks (id, scope, value, created_at) VALUES (?, ?, ?, ?)"
      #[Core.Ulid.encode now (← Crypto.randomBytes 10), scope, v, now]
    return if (← Db.changesRaw st.db.conn) > 0 then 2 else 1
  if n == 0 then return .error (409, "too_many_blocks")
  let some row ← st.db.queryOne "SELECT * FROM sender_blocks WHERE value = ?" #[v] | return .error (409, "conflict")
  let created := n == 2
  if created then Log.info "junk.blocked" [("scope", Json.str scope)]
  return .ok (row, created)

/-- What a report's `block` names, from the message's sender. -/
def blockTarget (j : Json) (fromAddr : String) : Option (String × String) :=
  match j.getObjVal? "block" with
  | .ok (.bool true) => some ("address", fromAddr)
  | .ok (.str "address") => some ("address", fromAddr)
  | .ok (.str "domain") => some ("domain", Blocks.domainOf fromAddr)
  | .ok o@(.obj _) => match jstr? o "scope", jstr? o "value" with
    | some s, some v => some (s, v)
    | some s, none => some (s, if s == "domain" then Blocks.domainOf fromAddr else fromAddr)
    | _, _ => none
  | _ => none

/-- Report a message as spam or phishing; `block` as in `blockTarget`. -/
def report (st : State) (id kind : String) (block : Option (String × String)) : IO Result := do
  if !kinds.contains kind then return .error (400, "bad_kind")
  let some row ← st.db.queryOne "SELECT source FROM messages WHERE id = ? AND deleted = 0" #[id] | return .error (404, "not_found")
  if row.text "source" == "sent" then return .error (400, "sent")
  let blockJson : Json ← match block with
    | none => pure Json.null
    | some (scope, value) =>
      match ← addBlock st scope value with
      | .error e => return .error e
      | .ok (b, created) => pure (Json.mkObj [("id", Json.str (b.text "id")), ("created", Json.bool created)])
  let now ← Clock.nowMs
  discard <| st.db.execCount "UPDATE messages SET junk_at = COALESCE(junk_at, ?1), junk_kind = ?2,
      archived_at = CASE WHEN archived_at IS NULL THEN COALESCE(junk_at, ?1) ELSE archived_at END, updated_at = ?1
    WHERE id = ?3 AND deleted = 0" #[now, kind, id]
  Gmail.Mirror.onAction st id "junk"
  Log.info "junk.reported" [("kind", Json.str kind), ("block", Json.bool block.isSome)]
  let some after ← Api.messageRow st id | return .error (404, "not_found")
  return .ok (match junkState after with
    | .obj kvs => Json.mkObj (kvs.toArray.toList ++ [("block", blockJson)])
    | j => j)

/-- The blocks that match a sender: its address, its domain and the parents of it. -/
def blocksFor (st : State) (fromAddr : String) : IO (Array Row) := do
  let a := fromAddr.toLower
  let d := Blocks.domainOf a
  let vals : Array Value := #[.text a] ++ ((if Blocks.isDomain d then Blocks.domainAndParents d else []).toArray.map Value.text)
  let qs := ", ".intercalate (vals.toList.map fun _ => "?")
  st.db.query s!"SELECT * FROM sender_blocks WHERE value IN ({qs})" vals

/-- Not junk: out of Junk, back in the list if the report is what took it out, and the sender's
blocks lifted. Asking twice is no error. -/
def notJunk (st : State) (id : String) : IO Result := do
  let some row ← st.db.queryOne "SELECT from_addr FROM messages WHERE id = ? AND deleted = 0" #[id] | return .error (404, "not_found")
  let now ← Clock.nowMs
  let n ← st.db.execCount "UPDATE messages SET archived_at = CASE WHEN archived_at = junk_at THEN NULL ELSE archived_at END,
      junk_at = NULL, junk_kind = NULL, updated_at = ?1
    WHERE id = ?2 AND deleted = 0 AND junk_at IS NOT NULL" #[now, id]
  let lifted ← blocksFor st (row.text "from_addr")
  for b in lifted do st.db.exec "DELETE FROM sender_blocks WHERE id = ?" #[b.text "id"]
  if n > 0 then
    Gmail.Mirror.onAction st id "notjunk"
    Log.info "junk.not_junk" [("unblocked", Json.num lifted.size)]
  let some after ← Api.messageRow st id | return .error (404, "not_found")
  return .ok (match junkState after with
    | .obj kvs => Json.mkObj (kvs.toArray.toList ++ [("unblocked", Json.arr (lifted.map fun b => Json.str (b.text "id")))])
    | j => j)

/-! ## List-Unsubscribe (RFC 2369, RFC 8058) -/

structure Unsub where
  mailto : Option String := none
  url : Option String := none
  oneClick : Bool := false
  deriving Repr, BEq, Inhabited

/-- The `<…>` targets of a `List-Unsubscribe` value and whether `List-Unsubscribe-Post` offers one-click. -/
def parseUnsub (value : String) (post : Option String) : Unsub := Id.run do
  let mut u : Unsub := { oneClick := (post.map fun p => (p.toLower.replace " " "").contains "list-unsubscribe=one-click").getD false }
  for part in (value.splitOn "<").drop 1 do
    let t := Core.Text.Str.trim ((part.splitOn ">").headD "")
    let lower := t.toLower
    if lower.startsWith "mailto:" && u.mailto.isNone then u := { u with mailto := some t }
    else if lower.startsWith "https://" && u.url.isNone then u := { u with url := some t }
  return u

/-- The host of an `https://` URL. -/
def urlHost (url : String) : String :=
  let rest := String.ofList (url.toList.drop 8)
  let hp := ((rest.splitOn "/").headD "").splitOn "?" |>.headD ""
  let hp := (hp.splitOn "@").getLast!
  ((hp.splitOn ":").headD "").toLower

/-- A host the container may POST to: a public name, not an address literal or a local name. -/
def publicHost (h : String) : Bool :=
  !h.isEmpty && h.contains '.' && !Core.Junk.isIpv4 h && !h.startsWith "[" && h != "localhost" &&
    !h.endsWith ".localhost" && !h.endsWith ".local" && !h.endsWith ".internal" &&
    h.toList.all fun c => c.isAlphanum || c == '.' || c == '-'

def unsubscribe (st : State) (id : String) : IO Result := do
  let some (_, raw) ← Api.loadRaw st id | return .error (404, "not_found")
  let m := Core.Mime.parse (Ingest.headerBlock raw)
  let some v := m.rawHeader? "list-unsubscribe" | return .error (404, "no_list_unsubscribe")
  let u := parseUnsub v (m.rawHeader? "list-unsubscribe-post")
  let manual := Json.mkObj [("status", Json.str "manual"), ("mailto", Api.optStrJ u.mailto), ("url", Api.optStrJ u.url)]
  match u.url with
  | some url =>
    if !u.oneClick || !publicHost (urlHost url) then return .ok manual
    let r ← try
        let resp ← st.web.request { method := "POST", url, timeoutMs := 15000, body := "List-Unsubscribe=One-Click".toUTF8
                                    headers := [("Content-Type", "application/x-www-form-urlencoded")] }
        pure (some resp.status)
      catch _ => pure none
    Log.info "junk.unsubscribe" [("ok", Json.bool (r.any fun s => 200 ≤ s && s < 300))]
    match r with
    | some s =>
      if 200 ≤ s && s < 300 then
        return .ok (Json.mkObj [("status", Json.str "unsubscribed"), ("via", Json.str "one-click"), ("url", Json.str url)])
      return .ok (Json.mkObj [("status", Json.str "failed"), ("http_status", Json.num s.toNat), ("mailto", Api.optStrJ u.mailto),
        ("url", Json.str url)])
    | none => return .ok (Json.mkObj [("status", Json.str "failed"), ("mailto", Api.optStrJ u.mailto), ("url", Json.str url)])
  | none => return .ok manual

/-! ## Routes -/

def answer (r : Result) : Resp :=
  match r with
  | .ok j => Resp.ok j
  | .error (s, code) => Resp.error s code

def reportRoute (c : Ctx) (req : Req) : IO Resp := do
  let j := req.json
  let kind := (jstr? j "kind").getD ""
  let fromAddr := ((← Api.messageRow c.st (c.param "id")).map (·.text "from_addr")).getD ""
  return answer (← report c.st (c.param "id") kind (blockTarget j fromAddr))

def notJunkRoute (c : Ctx) (_ : Req) : IO Resp := return answer (← notJunk c.st (c.param "id"))

def unsubscribeRoute (c : Ctx) (_ : Req) : IO Resp := return answer (← unsubscribe c.st (c.param "id"))

def blocksGet (c : Ctx) (_ : Req) : IO Resp := do
  let rows ← c.st.db.query "SELECT * FROM sender_blocks ORDER BY created_at DESC, id DESC LIMIT ?" #[blocksMax]
  return Resp.ok (Json.mkObj [("blocks", Json.arr (rows.map blockRowJson))])

def blocksPost (c : Ctx) (req : Req) : IO Resp := do
  let j := req.json
  match ← addBlock c.st ((jstr? j "scope").getD "") ((jstr? j "value").getD "") with
  | .error (s, code) => return Resp.error s code
  | .ok (row, created) => return Resp.json (if created then 201 else 200) (blockRowJson row)

def blocksDelete (c : Ctx) (_ : Req) : IO Resp := do
  let n ← c.st.db.execCount "DELETE FROM sender_blocks WHERE id = ?" #[c.param "id"]
  if n == 0 then return Resp.notFound
  Log.info "junk.unblocked" []
  return Resp.ok (Json.mkObj [("id", Json.str (c.param "id")), ("deleted", Json.bool true)])

def routes : Array Route := #[
  route "POST" "/api/msg/:id/report" (.token "act") reportRoute,
  route "POST" "/api/msg/:id/not-junk" (.token "act") notJunkRoute,
  route "POST" "/api/msg/:id/unsubscribe" (.token "act") unsubscribeRoute,
  route "GET" "/api/blocks" (.token "read") blocksGet,
  route "POST" "/api/blocks" (.token "act") blocksPost,
  route "DELETE" "/api/blocks/:id" (.token "act") blocksDelete
]

end Server.Junk
