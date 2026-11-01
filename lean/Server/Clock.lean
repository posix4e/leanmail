import Std.Time

namespace Server.Clock

/-! Wall-clock time. `IO.monoMsNow` is monotonic and starts at boot, so anything stored or sent
(received_at, updated_at, SigV4 dates) comes from here. -/

/-- Milliseconds since the Unix epoch. -/
def nowMs : IO Nat := do
  let t ← Std.Time.Timestamp.now
  return t.toMillisecondsSinceUnixEpoch.val.toNat

end Server.Clock
