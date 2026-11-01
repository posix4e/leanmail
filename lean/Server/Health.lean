import Lean.Data.Json
import Core
import Server.State
import Server.Db
import Server.Gmail.Client

/-! # Health

`GET /health` (`json`) and `GET /internal/health` (`internalJson`, the Durable Object's ping):
version, schema, FTS5, database size, uptime; the internal one adds the Gmail connection and,
on the in-memory store, the spool markers waiting. -/

namespace Server.Health

open Lean

def json (st : State) : IO Json := do
  let now ← IO.monoMsNow
  let dbBytes ← (do
    let m ← System.FilePath.metadata st.config.dbPath
    pure m.byteSize.toNat) <|> pure 0
  return Json.mkObj [
    ("ok", Json.bool true),
    ("version", Json.str Core.version),
    ("role", Json.str st.config.role),
    ("account", Json.str st.config.accountId),
    ("sqlite", Json.str Db.version),
    ("fts5", Json.bool st.fts5),
    ("schema", Json.num st.schemaVersion),
    ("db_bytes", Json.num dbBytes),
    ("uptime_ms", Json.num (now - st.startedAt))
  ]

/-- `/internal/health`: `json` plus the Gmail connection's state and, when the store is
the in-memory one (a bare run, where listing is free), how many spool markers wait. -/
def internalJson (st : State) : IO Json := do
  let base ← json st
  let optN (o : Option Nat) : Json := match o with | some n => Json.num n | none => Json.null
  let gmail ← (do
    match ← Gmail.conn? st with
    | some c => pure (Json.mkObj [("connected", Json.bool c.authorized), ("last_poll_at", optN c.lastPollAt),
        ("last_push_at", optN c.lastPushAt), ("watch_expiration", optN c.watchExpiration)])
    | none => pure (Json.mkObj [("connected", Json.bool false), ("last_poll_at", Json.null), ("last_push_at", Json.null),
        ("watch_expiration", Json.null)])) <|> pure Json.null
  let spool : List (String × Json) ← if st.config.hasR2 then pure [] else do
    let n := (← st.store.list s!"in/{st.config.accountId}/").size
    pure [("spool_pending", Json.num n)]
  return match base with
    | .obj kvs => Json.mkObj (kvs.toArray.toList ++ [("gmail", gmail)] ++ spool)
    | j => j


end Server.Health
