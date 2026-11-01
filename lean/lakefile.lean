import Lake
open System Lake DSL

/-! LeanMail: one Lake package for the pure core, the container server and the CLI.
The iOS app (../app) depends on this package but imports only `Core.*`. -/

package leanmail where
  version := v!"0.1.0"
  keywords := #["mail", "cloudflare", "lean"]
  license := "Apache-2.0"

/-- Rebuild an object when any of its headers change. -/
private def withHeaders (headers : Array FilePath) (srcJob : Job FilePath) : FetchM (Job FilePath) := do
  let mut job := srcJob
  for header in headers do
    let headerJob ← inputTextFile header
    job := job.zipWith (fun (src : FilePath) (_ : FilePath) => src) headerJob
  return job

def opensslPrefix : String := "/opt/homebrew/opt/openssl@3"
def curlPrefix : String := "/opt/homebrew/opt/curl"

/-- The bundled SQLite amalgamation, with FTS5 and JSON on. LFS is off for the same reason as
leansqlite: Lean's bundled libc has no fcntl64 on Linux. -/
target sqlite.o pkg : FilePath := do
  let oFile := pkg.buildDir / "sqlite3.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "c" / "vendor" / "sqlite3.c"
  let weakArgs := #["-I", (pkg.dir / "ffi" / "c" / "vendor").toString]
  buildO oFile srcJob weakArgs
    (traceArgs := #["-O2", "-fPIC", "-w", "-DSQLITE_DISABLE_LFS", "-DSQLITE_ENABLE_FTS5", "-DSQLITE_ENABLE_JSON1",
                    "-DSQLITE_THREADSAFE=1", "-DSQLITE_DQS=0", "-DSQLITE_OMIT_DEPRECATED",
                    "-DSQLITE_DEFAULT_WAL_SYNCHRONOUS=1"])
    (extraDepTrace := getLeanTrace)

/-- The Lean-facing C: SQLite statements, libcurl requests, libcrypto digests. -/
target shim.o pkg : FilePath := do
  let oFile := pkg.buildDir / "lm_shim.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "c" / "lm_shim.c"
  let srcJob ← withHeaders #[pkg.dir / "ffi" / "c" / "vendor" / "sqlite3.h"] srcJob
  let mut weakArgs := #["-I", (← getLeanIncludeDir).toString, "-I", (pkg.dir / "ffi" / "c" / "vendor").toString]
  if System.Platform.isOSX then
    weakArgs := weakArgs ++ #["-I", opensslPrefix ++ "/include", "-I", curlPrefix ++ "/include"]
  buildO oFile srcJob weakArgs (traceArgs := #["-O2", "-fPIC"]) (extraDepTrace := getLeanTrace)

extern_lib lmffi pkg := do
  let sqliteObj ← sqlite.o.fetch
  let shimObj ← shim.o.fetch
  buildStaticLib (pkg.staticLibDir / nameToStaticLib "lmffi") #[sqliteObj, shimObj]

/-- System libraries the shim needs. OpenSSL comes from Homebrew on macOS and the distro on Linux. -/
def shimLinkArgs : Array String :=
  if System.Platform.isOSX then
    -- Lean's bundled lld cannot read the SDK's libcurl stub (new TAPI targets), so on macOS the
    -- shim links Homebrew's curl and openssl@3 by path.
    #[curlPrefix ++ "/lib/libcurl.dylib", "-L" ++ opensslPrefix ++ "/lib", "-lcrypto"]
  else
    #["-lcurl", "-lcrypto", "-lpthread", "-ldl", "-lm"]

@[default_target]
lean_lib Core

lean_lib Server where
  needs := #[lmffi]

lean_lib Cli where
  needs := #[lmffi]

lean_exe «leanmail-server» where
  root := `Server.Main
  moreLinkArgs := shimLinkArgs

lean_exe leanmail where
  root := `Cli.Main
  moreLinkArgs := shimLinkArgs

lean_exe «core-test» where
  root := `CoreTest
  srcDir := "test"

lean_exe «address-test» where
  root := `AddressTest
  srcDir := "test"

lean_exe «server-test» where
  root := `ServerTest
  srcDir := "test"
  moreLinkArgs := shimLinkArgs

lean_exe «junk-test» where
  root := `JunkTest
  srcDir := "test"

lean_exe «ical-test» where
  root := `IcalTest
  srcDir := "test"
