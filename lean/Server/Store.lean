import Std.Data.HashMap
import Server.Clock

namespace Server

/-! # Object store

The interface ingest and the spool use for raw mail: R2 over the S3 API in production
(`Server.R2`), an in-memory map in tests and bare local runs. Keys are slash-separated paths
(`raw/<acct>/<ulid>`, `in/<acct>/<ulid>`); metadata is a small string map (R2 custom metadata,
`x-amz-meta-*` on the wire).

`listPage prefix after max` is the cursor form the orphan walk needs: keys under the prefix that
sort after `after`, at most `max`, each with its upload time when the store knows it (R2's
`LastModified`, the memory store's put time). A store built without it falls back to `list`,
with no upload times. -/

/-- One listed object. -/
structure StoreObject where
  key : String
  /-- Epoch ms of the upload, when the store reports it. -/
  uploadedAt : Option Nat := none
  deriving Inhabited, Repr, BEq

structure Store where
  put : String → ByteArray → List (String × String) → IO Unit
  /-- The object and its metadata, `none` when there is no such key. -/
  get : String → IO (Option (ByteArray × List (String × String)))
  delete : String → IO Unit
  /-- Keys starting with the prefix, in key order, at most 1000. -/
  list : String → IO (Array String)
  listPage : String → String → Nat → IO (Array StoreObject) := fun pre after max => do
    let ks ← list pre
    return ((ks.filter (· > after)).extract 0 max).map fun k => { key := k }

namespace Store

def listMax : Nat := 1000

/-- A store backed by an `IO.Ref`; it remembers when each key was put, by `clock`. -/
def memoryWith (clock : IO Nat) : IO Store := do
  let r ← IO.mkRef ({} : Std.HashMap String (ByteArray × List (String × String) × Nat))
  return {
    put := fun k v md => do
      let t ← clock
      r.modify (·.insert k (v, md, t))
    get := fun k => return ((← r.get)[k]?).map fun (v, md, _) => (v, md)
    delete := fun k => r.modify (·.erase k)
    list := fun pre => do
      let ks := (← r.get).toArray.filterMap fun (k, _) => if k.startsWith pre then some k else none
      return (ks.qsort (· < ·)).extract 0 listMax
    listPage := fun pre after max => do
      let m ← r.get
      let ks := m.toArray.filterMap fun (k, (_, _, t)) =>
        if k.startsWith pre && k > after then some ({ key := k, uploadedAt := some t } : StoreObject) else none
      return (ks.qsort (·.key < ·.key)).extract 0 max
  }

/-- The memory store on the wall clock. -/
def memory : IO Store := memoryWith Clock.nowMs

end Store
end Server
