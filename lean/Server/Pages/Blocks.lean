import Server.Pages.Common
import Server.Pages.Settings
import Server.Pages.Calendar
import Server.Junk

/-! # Pages: blocked senders (`/settings/blocks`)

The blocks, newest first, with how much mail each filed and when it last did; a form to block
an address or a domain; "Unblock" on each. Mail a block filed stays in Junk; "Not junk" on the
message brings it back. -/

namespace Server.Pages

open Lean Core.Html Server.Db Server.Router

def blocksPage (p : Page) (notice : Node := empty) (status : Nat := 200) : IO Resp := do
  let now ← Clock.nowMs
  let rows ← p.c.st.db.query "SELECT * FROM sender_blocks ORDER BY created_at DESC, id DESC LIMIT 1000"
  let saved := match p.req.param? "saved" with
    | some "added" => flash "ok" "Blocked."
    | some "deleted" => flash "ok" "Unblocked. Mail it filed stays in Junk."
    | _ => empty
  let items := rows.toList.map fun r =>
    el "li" [("class", "item")] [
      el "div" [] [elt "span" [("class", "tname")] (r.text "value"), badge "scope" (r.text "scope")],
      el "div" [("class", "meta")] [txt s!"Blocked ", timeEl (r.nat "created_at") (shortTime (r.nat "created_at") now),
        txt s!" · {r.nat "hits"} filed",
        match r.nat? "last_hit_at" with
        | some t => frag [txt " · last ", timeEl t (shortTime t now)]
        | none => empty],
      postButton s!"/settings/blocks/{r.text "id"}/delete" "Unblock"]
  return page p "Blocked senders" (pageKind := "settings") (status := status) [
    el "p" [] [elt "a" [("href", "/settings")] "‹ Settings"],
    elt "h1" [] "Blocked senders", notice, saved,
    el "section" [("class", "card")] [
      elt "p" [("class", "hint")] "Mail from a blocked address, or from anywhere on a blocked domain, goes straight to Junk and never to your phone.",
      if items.isEmpty then emptyNote "Nobody is blocked." else el "ul" [("class", "items"), ("id", "blocks")] items],
    el "section" [("class", "card")] [
      elt "h2" [] "Block a sender",
      el "form" [("method", "post"), ("action", "/settings/blocks"), ("class", "stack")] [
        el "label" [("class", "field")] [elt "span" [] "What",
          el "select" [("name", "scope")] [elt "option" [("value", "address")] "An address", elt "option" [("value", "domain")] "A whole domain"]],
        field "Address or domain" "value" "" "text" [("required", "required"), ("maxlength", "254"), ("placeholder", "spammer@example.com")],
        elt "button" [("type", "submit"), ("class", "danger")] "Block"]]]

def blocksGetPage (p : Page) : IO Resp := blocksPage p

def blocksAddPost (p : Page) : IO Resp := do
  let f := parseForm p.req.body
  match ← Server.Junk.addBlock p.c.st (formField f "scope") (formField f "value") with
  | .ok _ => return redirect "/settings/blocks?saved=added" 303
  | .error (_, code) =>
    let msg := if code == "own_domain" then "That is one of your own domains: blocking it would file your own mail in Junk."
      else "That is not an address or a domain."
    blocksPage p (flash "err" msg) 400

def blocksDeletePost (p : Page) : IO Resp := do
  let n ← p.c.st.db.execCount "DELETE FROM sender_blocks WHERE id = ?" #[p.c.param "id"]
  if n == 0 then return notFoundPage p
  return redirect (safeBack (formField (parseForm p.req.body) "back") "/settings/blocks?saved=deleted") 303

def blocksRoutes : Array Route := #[
  route "GET" "/settings/blocks" .«public» (guarded false blocksGetPage),
  route "POST" "/settings/blocks" .«public» (guarded true blocksAddPost),
  route "POST" "/settings/blocks/:id/delete" .«public» (guarded true blocksDeletePost)
]

end Server.Pages
