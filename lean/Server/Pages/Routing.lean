import Server.Pages.Common
import Server.Pages.Settings
import Server.Routing

/-! # Pages: routing settings (`/settings/routing`)

The domains (default for unlisted addresses, forward targets, the names mail is sent from, whether
LeanMail manages the zone at Cloudflare) and the addresses, with forms to add, change and delete
each; every change is projected to the Worker at once. Below them the plan: what Cloudflare must
change to deliver to this Worker (`Server.Routing.plan`). With `CF_API_TOKEN` and `CF_ACCOUNT_ID`
set, "Check Cloudflare" reads the account and "Apply" makes the changes; without them the plan is
shown against zones assumed empty and the page says it cannot apply it. -/

namespace Server.Pages

open Lean Core.Html Server.Db Server.Router

def mailLabel (m : String) : String :=
  match m with
  | "vault" => "Keep in LeanMail" | "vault+forward" => "Keep and forward" | "forward" => "Forward only"
  | "drop" => "Drop" | x => x

def mailSelect (name current : String) : Node :=
  el "label" [("class", "field")] [elt "span" [] "Mail",
    el "select" [("name", name)] (Routing.mails.map fun m =>
      elt "option" ([("value", m)] ++ (if m == current then [("selected", "selected")] else [])) (mailLabel m))]

def checkBox (label name : String) (on : Bool) : Node :=
  el "label" [("class", "check")] [
    el "input" ([("type", "checkbox"), ("name", name), ("value", "1")] ++ (if on then [("checked", "checked")] else [])), txt label]

/-- `a@x, b@y` or one per line → a JSON array of strings. -/
def listField (s : String) : Json :=
  let parts := (s.split fun c => c == ',' || c == '\n' || c == ' ' || c == '\r' || c == ';').toList.map (·.toString)
  Json.arr ((parts.map Core.Text.Str.trim).filter (· ≠ "") |>.toArray.map Json.str)

def planNode (p : Page) (pl : Routing.Plan) (canApply : Bool) : Node :=
  let opItems := pl.ops.map fun o =>
    el "li" [("class", if o.danger then "op danger" else "op")] [badge "kind" o.kind, txt " ", txt o.text]
  let probItems := pl.problems.map fun x => el "li" [("class", s!"problem {x.severity}")] [badge x.severity x.severity, txt " ", txt x.text]
  let unItems := pl.unmanaged.map fun u =>
    el "li" [("class", "unmanaged")] [
      when canApply (el "input" [("type", "checkbox"), ("name", "remove"), ("value", u.rule.id), ("form", "apply-form"),
        ("aria-label", s!"Remove the rule for {u.rule.matcher}")]),
      txt s!" {u.domain}: {u.text}"]
  let _ := p
  el "section" [("class", "card"), ("id", "plan")] [
    elt "h2" [] "Cloudflare",
    if pl.assumed then
      elt "p" [("class", "flash warn"), ("role", "status")]
        "CF_API_TOKEN and CF_ACCOUNT_ID are not set on this server, so it cannot read Cloudflare or apply this plan. It shows what each managed zone needs; make these changes in the Cloudflare dashboard."
    else if !pl.ok then
      flash "err" s!"Cloudflare could not be read ({pl.error.getD "error"})."
    else empty,
    if opItems.isEmpty then emptyNote (if pl.assumed then "No managed domains." else "Cloudflare already does what LeanMail needs.")
    else el "ol" [("class", "ops")] opItems,
    when (!probItems.isEmpty) (frag [elt "h3" [] "Problems", el "ul" [("class", "problems")] probItems]),
    when (!unItems.isEmpty) (frag [elt "h3" [] "Rules LeanMail did not make", elt "p" [("class", "hint")]
      "These are left alone unless you tick them for removal when applying.", el "ul" [] unItems]),
    if canApply && pl.ok && (!pl.ops.isEmpty || !pl.unmanaged.isEmpty) then
      el "form" [("method", "post"), ("action", "/settings/routing/apply"), ("id", "apply-form"), ("data-confirm", "Apply these changes at Cloudflare?")] [
        el "input" [("type", "hidden"), ("name", "plan_id"), ("value", pl.id)],
        elt "button" [("type", "submit"), ("class", "primary")] "Apply"]
    else empty,
    when canApply (el "p" [] [elt "a" [("href", "/settings/routing?plan=1#plan")] "Check Cloudflare again"])]

def routingPage (p : Page) (notice : Node := empty) (status : Nat := 200) : IO Resp := do
  let st := p.c.st
  let ds ← Routing.loadDomains st
  let as ← Routing.loadAddresses st
  -- with a token here, or through the operator (an account container): plan on request
  let canTry := st.config.hasCloudflare || Routing.needsOperator st.config
  let pl ← if !canTry || p.req.param? "plan" == some "1" || status == 409 then do pure (some (← Routing.planNow st)) else pure none
  let canApply := match pl with
    | some x => Routing.canApplyPlan st.config x
    | none => canTry
  let saved := match p.req.param? "saved" with
    | some "ok" => flash "ok" "Saved, and sent to the Worker."
    | some "unprojected" => flash "err" "Saved, but the Worker did not take the new routes; it will on the next change."
    | some "applied" => flash "ok" "Applied at Cloudflare."
    | _ => empty
  let domainCards := ds.map fun d =>
    el "li" [("class", "item domain"), ("id", s!"domain-{d.name}")] [
      el "form" [("method", "post"), ("action", s!"/settings/routing/domains/{d.name}"), ("class", "stack")] [
        elt "h3" [] d.name,
        mailSelect "default_mail" d.defaultMail,
        field "Forward to (for forwarding modes)" "default_forward_to" (", ".intercalate d.defaultForwardTo) "text",
        field "Send from (this domain or names under it)" "sending" (", ".intercalate d.sending) "text",
        checkBox "LeanMail manages this zone at Cloudflare" "manage" d.manage,
        elt "button" [("type", "submit")] "Save"],
      postButton s!"/settings/routing/domains/{d.name}/delete" "Delete domain" "danger"]
  let addrRows := as.map fun a =>
    el "li" [("class", "item address")] [
      el "div" [] [elt "span" [("class", "tname")] a.address, txt s!" · {mailLabel a.mail}",
        when (!a.forwardTo.isEmpty) (txt s!" → {", ".intercalate a.forwardTo}"),
        when a.canSend (badge "send" "can send"), when (!a.note.isEmpty) (elt "span" [("class", "meta")] s!" {a.note}")],
      postButton s!"/settings/routing/addresses/{a.address}/delete" "Delete"]
  return page p "Routing" (pageKind := "settings") (status := status) [
    el "p" [] [elt "a" [("href", "/settings")] "‹ Settings"],
    elt "h1" [] "Mail routing", notice, saved,
    el "section" [("class", "card"), ("id", "domains")] [
      elt "h2" [] "Domains",
      elt "p" [("class", "hint")] "What happens to mail for an address that has no row of its own.",
      if domainCards.isEmpty then emptyNote "No domains yet." else el "ul" [("class", "items")] domainCards,
      el "form" [("method", "post"), ("action", "/settings/routing/domains"), ("class", "stack")] [
        elt "h3" [] "Add a domain",
        field "Domain" "name" "" "text" [("required", "required"), ("placeholder", "example.com")],
        mailSelect "default_mail" "vault",
        field "Forward to" "default_forward_to" "" "text",
        field "Send from" "sending" "" "text" [("placeholder", "example.com")],
        checkBox "LeanMail manages this zone at Cloudflare" "manage" true,
        elt "button" [("type", "submit"), ("class", "primary")] "Add domain"]],
    el "section" [("class", "card"), ("id", "addresses")] [
      elt "h2" [] "Addresses",
      if addrRows.isEmpty then emptyNote "No addresses yet: every address follows its domain." else el "ul" [("class", "items")] addrRows,
      el "form" [("method", "post"), ("action", "/settings/routing/addresses"), ("class", "stack")] [
        elt "h3" [] "Add or change an address",
        field "Address" "address" "" "email" [("required", "required")],
        mailSelect "mail" "vault",
        field "Forward to" "forward_to" "" "text",
        checkBox "Mail may be sent from it" "can_send" true,
        field "Note" "note" "" "text" [("maxlength", "500")],
        elt "button" [("type", "submit"), ("class", "primary")] "Save address"]],
    match pl with
    | some pl => planNode p pl canApply
    | none => el "section" [("class", "card"), ("id", "plan")] [elt "h2" [] "Cloudflare",
        el "p" [] [elt "a" [("href", "/settings/routing?plan=1#plan"), ("class", "btn")] "Check Cloudflare"]]]

def routingGet (p : Page) : IO Resp := routingPage p

def afterChange (p : Page) (r : Except (Nat × String) Unit) : IO Resp := do
  match r with
  | .ok () =>
    let ok ← Routing.project p.c.st
    return redirect s!"/settings/routing?saved={if ok then "ok" else "unprojected"}" 303
  | .error (_, msg) => routingPage p (flash "err" s!"Not saved: {msg}.") 400

def domainForm (f : List (String × String)) (withName : Bool) : Json :=
  Json.mkObj ((if withName then [("name", Json.str (formField f "name"))] else []) ++ [
    ("default_mail", Json.str (formField f "default_mail")),
    ("default_forward_to", listField (formField f "default_forward_to")),
    ("sending", listField (formField f "sending")),
    ("manage", Json.bool (formField f "manage" == "1"))])

def routingDomainAdd (p : Page) : IO Resp := do
  afterChange p (← Routing.addDomain p.c.st (domainForm (parseForm p.req.body) true))

def routingDomainSave (p : Page) : IO Resp := do
  afterChange p (← Routing.patchDomain p.c.st (p.c.param "name").toLower (domainForm (parseForm p.req.body) false))

def routingDomainDelete (p : Page) : IO Resp := do
  if !(← Routing.deleteDomain p.c.st (p.c.param "name").toLower) then return notFoundPage p
  afterChange p (.ok ())

def routingAddressSave (p : Page) : IO Resp := do
  let f := parseForm p.req.body
  let j := Json.mkObj [("mail", Json.str (formField f "mail")), ("forward_to", listField (formField f "forward_to")),
    ("can_send", Json.bool (formField f "can_send" == "1")), ("note", Json.str (formField f "note"))]
  afterChange p (← Routing.putAddress p.c.st (formField f "address") j)

def routingAddressDelete (p : Page) : IO Resp := do
  if !(← Routing.deleteAddress p.c.st (p.c.param "address")) then return notFoundPage p
  afterChange p (.ok ())

def routingApply (p : Page) : IO Resp := do
  let f := parseForm p.req.body
  let remove := f.filterMap fun (k, v) => if k == "remove" then some v else none
  match ← Routing.apply p.c.st (some (formField f "plan_id")) remove with
  | .ok _ => return redirect "/settings/routing?saved=applied&plan=1#plan" 303
  | .error (503, code, _) =>
    routingPage p (flash "err" (if code == "routing_needs_operator" then "The operator could not be reached to apply the plan. Try again later."
      else "This server has no CF_API_TOKEN, so it cannot apply the plan.")) 503
  | .error (403, _, _) => routingPage p (flash "err" "A domain in this plan is not granted to this account.") 403
  | .error (_, code, _) =>
    routingPage p (flash "err" (if code == "plan_changed" then "Cloudflare changed since the plan was shown. Here is the new one." else code)) 409

def routingRoutes : Array Route := #[
  route "GET" "/settings/routing" .«public» (guarded false routingGet),
  route "POST" "/settings/routing/domains" .«public» (guarded true routingDomainAdd),
  route "POST" "/settings/routing/domains/:name" .«public» (guarded true routingDomainSave),
  route "POST" "/settings/routing/domains/:name/delete" .«public» (guarded true routingDomainDelete),
  route "POST" "/settings/routing/addresses" .«public» (guarded true routingAddressSave),
  route "POST" "/settings/routing/addresses/:address/delete" .«public» (guarded true routingAddressDelete),
  route "POST" "/settings/routing/apply" .«public» (guarded true routingApply)
]

end Server.Pages
