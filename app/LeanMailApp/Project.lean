import ElmShell.Manifest

/-! LeanMail's Xcode project: `lake exe ios-project` writes its XcodeGen spec (framework/native/ci/build-ios
runs it). The app never imports this module. -/

open ElmShell Lean

def manifest : Manifest where
  name := "LeanMailApp"
  displayName := "LeanMail"
  bundleId := "io.leanmail.app"
  team := some "2858MX5336"
  version := .settings "0.1.0" "3"
  deploymentTarget := "26.0"
  mac := .catalyst
  -- App Store Connect refuses an iPad build without all four (iPad multitasking)
  orientations := some ([.portrait, .landscapeLeft, .landscapeRight],
                         [.portrait, .portraitUpsideDown, .landscapeLeft, .landscapeRight])
  uses := ["http.request", "keychain.*", "build", "paths"]
  icon := some "Assets.xcassets"
  uiTests := some {}
  -- The shared LeanNative shell links CoreBluetooth and LocalAuthentication for every app, so App Store
  -- Connect asks for their purpose strings although LeanMail calls neither.
  info := [("ITSAppUsesNonExemptEncryption", toJson false),
           ("NSBluetoothAlwaysUsageDescription", toJson "LeanMail does not use Bluetooth."),
           ("NSFaceIDUsageDescription", toJson "LeanMail does not use Face ID; passkeys use the system's own prompt."),
           -- plain http only to local addresses (a development server); everything else is HTTPS
           ("NSAppTransportSecurity", Json.mkObj [("NSAllowsLocalNetworking", Json.bool true)])]
  settings := [("TARGETED_DEVICE_FAMILY", "1,2")]

/-- A development server for the UI tests reaches the generated scheme's test environment (never a
tracked file): set `LEANMAIL_TEST_ORIGIN` (and `LEANMAIL_TEST_TOKEN`, an `lm1…` token, to test the token
path; `LEANMAIL_SCREENSHOTS`, a directory for the test's PNGs) while `build-ios` generates the project. Xcode does not pass xcodebuild's environment to XCTest. -/
def main (args : List String) : IO UInt32 := do
  let vars ← ["LEANMAIL_TEST_ORIGIN", "LEANMAIL_TEST_TOKEN", "LEANMAIL_SCREENSHOTS"].filterMapM fun k => do
    return (← IO.getEnv k).bind fun v => if v.isEmpty then none else some (k, Json.str v)
  let extra := if vars.isEmpty then Json.mkObj [] else
    Json.mkObj [("schemes", Json.mkObj [("LeanMailApp", Json.mkObj [
      ("test", Json.mkObj [("environmentVariables", Json.mkObj vars)])])])]
  { manifest with spec := deepMerge manifest.spec extra }.main args
