import Server.Config
import Server.Log
import Server.Clock
import Server.Db
import Server.Schema
import Server.Http
import Server.Crypto
import Server.Store
import Server.R2
import Server.State
import Server.Worker
import Server.Ingest
import Server.Spool
import Server.Search
import Server.Router.Types
import Server.Auth.Tokens
import Server.Auth.Session
import Server.Auth.ApiTokens
import Server.Mail.Build
import Server.Auth.Magic
import Server.Auth.Passkey
import Server.Operator
import Server.Router
import Server.Api
import Server.Account
import Server.Actions
import Server.Send
import Server.Drafts
import Server.Gmail
import Server.Jobs
import Server.Pages
import Server.Pages.Signup
import Server.Static
import Server.AI
import Server.Mcp
import Server.Routes

/-! # LeanMail server library

The container's modules. The executable's entry point is `Server.Main` (built as
`leanmail-server`), kept out of this root so the library has no `main`. -/
