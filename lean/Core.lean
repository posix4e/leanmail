import Core.Base64
import Core.Cbor
import Core.Html
import Core.Junk
import Core.Http
import Core.Ical
import Core.Mime
import Core.Mime.Parts
import Core.Mcp
import Core.Prompts
import Core.Query
import Core.RRule
import Core.Rfc822
import Core.Search
import Core.Ulid
import Core.Version

/-! # LeanMail core

Pure definitions shared by the container server, the CLI and the native apps. Nothing in `Core`
performs I/O beyond the interfaces it declares, and nothing here imports an `@[extern]` module,
so the iOS build's import closure never pulls in libuv, SQLite or libcurl. -/
