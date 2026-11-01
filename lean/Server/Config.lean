namespace Server

/-- What the container is told by its environment. Every field has a development default so the
server runs bare on a laptop with no Worker in front. -/
structure Config where
  port : UInt16 := 8080
  dataDir : String := "./data"
  accountId : String := "dev"
  /-- `account` or `operator`. -/
  role : String := "account"
  workerOrigin : String := "http://127.0.0.1:8787"
  publicOrigin : String := "http://127.0.0.1:8787"
  internalSecret : String := ""
  sessionSecret : String := "dev-session-secret-change-me"
  signupMode : String := "approve"
  adminEmails : List String := []
  retentionDaysDefault : Nat := 90
  logLevel : String := "info"
  /-- R2 over the S3 API. With no bucket the server keeps objects in memory (bare local runs). -/
  r2Bucket : String := ""
  /-- `<account>.r2.cloudflarestorage.com`, with or without `https://`. -/
  r2Endpoint : String := ""
  r2AccessKeyId : String := ""
  r2SecretAccessKey : String := ""
  r2Region : String := "auto"
  /-- The address sign-in mail comes from (`MAIL_FROM`); default `no-reply@<host of PUBLIC_ORIGIN>`. -/
  mailFrom : String := "no-reply@127.0.0.1"
  /-- `DEV_AUTH_BYPASS=1`: any request carrying `x-account-id` is an admin session. Honoured only
  when `PUBLIC_ORIGIN` is a localhost origin (see `devBypassOn`). -/
  devBypass : Bool := false
  /-- `CF_API_TOKEN` (Zone Read, Email Routing Edit, Email Sending Edit): optional. Without it
  the routing page shows the plan and says it cannot apply it. -/
  cfApiToken : String := ""
  /-- `CF_ACCOUNT_ID`: the Cloudflare account the token belongs to (destination addresses). -/
  cfAccountId : String := ""
  /-- `CF_API_BASE`: the Cloudflare API, overridable for tests. -/
  cfApiBase : String := "https://api.cloudflare.com/client/v4"
  /-- `CF_WORKER_NAME`: the Worker a managed zone's catch-all hands mail to. -/
  cfWorker : String := "leanmail"
  /-- `AI_BASE_URL`, e.g. `https://api.openai.com/v1`: the optional AI, any endpoint speaking the
  OpenAI chat-completions format (`Server.AI`). Off unless all three AI settings are set. -/
  aiBaseUrl : String := ""
  /-- `AI_API_KEY`: sent as `Authorization: Bearer`. Never logged or shown. -/
  aiApiKey : String := ""
  /-- `AI_MODEL`: the `model` of every request. -/
  aiModel : String := ""
  deriving Inhabited, Repr

namespace Config

private def env (name : String) (d : String) : IO String := do
  return (← IO.getEnv name).getD d

/-- `https://mail.example.com:8443/x` → `mail.example.com`. -/
def originHost (o : String) : String :=
  let rest :=
    if o.startsWith "https://" then String.ofList (o.toList.drop 8)
    else if o.startsWith "http://" then String.ofList (o.toList.drop 7) else o
  let hostPort := (rest.splitOn "/").headD ""
  (hostPort.splitOn ":").headD ""

/-- `http://localhost` or `http://127.0.0.1`, with or without a port or path. -/
def isLocalOriginStr (o : String) : Bool :=
  ["http://localhost", "http://127.0.0.1"].any fun p =>
    o == p || o.startsWith (p ++ ":") || o.startsWith (p ++ "/")

def fromEnv : IO Config := do
  let port := (← env "PORT" "8080").toNat?.getD 8080
  return {
    port := port.toUInt16
    dataDir := ← env "DATA_DIR" "./data"
    accountId := ← env "ACCOUNT_ID" "dev"
    role := ← env "ROLE" "account"
    workerOrigin := ← env "WORKER_ORIGIN" "http://127.0.0.1:8787"
    publicOrigin := ← env "PUBLIC_ORIGIN" "http://127.0.0.1:8787"
    internalSecret := ← env "INTERNAL_SECRET" ""
    sessionSecret := ← env "SESSION_SECRET" "dev-session-secret-change-me"
    signupMode := ← env "SIGNUP_MODE" "approve"
    adminEmails := ((← env "ADMIN_EMAILS" "").splitOn ",").map (·.trimAscii.toString) |>.filter (· ≠ "")
    retentionDaysDefault := (← env "RETENTION_DAYS_DEFAULT" "90").toNat?.getD 90
    logLevel := ← env "LOG_LEVEL" "info"
    r2Bucket := ← env "R2_BUCKET" ""
    r2Endpoint := ← env "R2_ENDPOINT" ""
    r2AccessKeyId := ← env "R2_ACCESS_KEY_ID" ""
    r2SecretAccessKey := ← env "R2_SECRET_ACCESS_KEY" ""
    r2Region := ← env "R2_REGION" "auto"
    mailFrom := ← env "MAIL_FROM" ("no-reply@" ++ originHost (← env "PUBLIC_ORIGIN" "http://127.0.0.1:8787"))
    -- refused (false) unless the public origin is a localhost one; `Main` logs either way
    devBypass := (← env "DEV_AUTH_BYPASS" "") == "1" && isLocalOriginStr (← env "PUBLIC_ORIGIN" "http://127.0.0.1:8787")
    cfApiToken := (← env "CF_API_TOKEN" "").trimAscii.toString
    cfAccountId := (← env "CF_ACCOUNT_ID" "").trimAscii.toString
    cfApiBase := ← env "CF_API_BASE" "https://api.cloudflare.com/client/v4"
    cfWorker := ← env "CF_WORKER_NAME" "leanmail"
    aiBaseUrl := (← env "AI_BASE_URL" "").trimAscii.toString
    aiApiKey := (← env "AI_API_KEY" "").trimAscii.toString
    aiModel := (← env "AI_MODEL" "").trimAscii.toString
  }

def dbPath (c : Config) : String := s!"{c.dataDir}/mail.db"
def hasR2 (c : Config) : Bool := !c.r2Bucket.isEmpty && !c.r2Endpoint.isEmpty
def isOperator (c : Config) : Bool := c.role == "operator"
/-- The public origin without a trailing slash. -/
def origin (c : Config) : String :=
  if c.publicOrigin.endsWith "/" then String.ofList (c.publicOrigin.toList.dropLast) else c.publicOrigin
def isLocalOrigin (c : Config) : Bool := isLocalOriginStr c.publicOrigin
/-- A bare local run with no Worker in front: no internal secret and a localhost origin. -/
def bareDev (c : Config) : Bool := c.internalSecret.isEmpty && c.isLocalOrigin
def devBypassOn (c : Config) : Bool := c.devBypass && c.isLocalOrigin
/-- Cookies carry `Secure` unless the origin is plain http (wrangler dev). -/
def secureCookies (c : Config) : Bool := !c.publicOrigin.startsWith "http://"
/-- Both Cloudflare settings are present: the routing plan can be read and applied. -/
def hasCloudflare (c : Config) : Bool := !c.cfApiToken.isEmpty && !c.cfAccountId.isEmpty
/-- All three AI settings are present. Whether the AI runs also depends on the owner's
`ai.enabled` pref (default on), which `Server.AI.enabled` reads. -/
def aiEnabled (c : Config) : Bool := !c.aiBaseUrl.isEmpty && !c.aiApiKey.isEmpty && !c.aiModel.isEmpty
/-- The chat-completions URL: `AI_BASE_URL` without a trailing slash, plus `/chat/completions`. -/
def aiUrl (c : Config) : String :=
  let b := if c.aiBaseUrl.endsWith "/" then String.ofList c.aiBaseUrl.toList.dropLast else c.aiBaseUrl
  b ++ "/chat/completions"
def isAdminEmail (c : Config) (email : String) : Bool := c.adminEmails.any (·.toLower == email.toLower)

end Config
end Server
