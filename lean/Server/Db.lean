import Std.Sync.Mutex

namespace Server.Db

/-! # SQLite, thinly

Bindings to `ffi/c/lm_shim.c` over the vendored amalgamation (FTS5 on). One connection per process,
guarded by a mutex: SQLite allows one writer and the container is that writer. -/

private opaque DbPointed : NonemptyType
/-- An open connection. Closed by its finalizer. -/
def Conn : Type := DbPointed.type
instance : Nonempty Conn := DbPointed.property

private opaque StmtPointed : NonemptyType
/-- A prepared statement. Finalized by its finalizer. -/
def Stmt : Type := StmtPointed.type
instance : Nonempty Stmt := StmtPointed.property

@[extern "lm_sqlite_open"] opaque openRaw : @& String → IO Conn
@[extern "lm_sqlite_exec"] opaque execRaw : @& Conn → @& String → IO Unit
@[extern "lm_sqlite_prepare"] opaque prepare : @& Conn → @& String → IO Stmt
@[extern "lm_sqlite_bind_null"] opaque bindNull : @& Stmt → UInt32 → IO Unit
@[extern "lm_sqlite_bind_int"] opaque bindInt : @& Stmt → UInt32 → Int64 → IO Unit
@[extern "lm_sqlite_bind_real"] opaque bindReal : @& Stmt → UInt32 → Float → IO Unit
@[extern "lm_sqlite_bind_text"] opaque bindText : @& Stmt → UInt32 → @& String → IO Unit
@[extern "lm_sqlite_bind_blob"] opaque bindBlob : @& Stmt → UInt32 → @& ByteArray → IO Unit
@[extern "lm_sqlite_step"] opaque step : @& Stmt → IO Bool
@[extern "lm_sqlite_reset"] opaque reset : @& Stmt → IO Unit
@[extern "lm_sqlite_column_count"] opaque columnCount : @& Stmt → UInt32
@[extern "lm_sqlite_column_type"] opaque columnType : @& Stmt → UInt32 → UInt32
@[extern "lm_sqlite_column_name"] opaque columnName : @& Stmt → UInt32 → String
@[extern "lm_sqlite_column_int"] opaque columnInt : @& Stmt → UInt32 → Int64
@[extern "lm_sqlite_column_real"] opaque columnReal : @& Stmt → UInt32 → Float
@[extern "lm_sqlite_column_text"] opaque columnText : @& Stmt → UInt32 → String
@[extern "lm_sqlite_column_blob"] opaque columnBlob : @& Stmt → UInt32 → ByteArray
@[extern "lm_sqlite_changes"] opaque changesRaw : @& Conn → IO Int64
@[extern "lm_sqlite_last_insert_rowid"] opaque lastInsertRowidRaw : @& Conn → IO Int64
@[extern "lm_sqlite_version"] opaque versionRaw : Unit → String

def version : String := versionRaw ()

/-- A bound parameter or a result cell. -/
inductive Value where
  | null
  | int (i : Int64)
  | real (r : Float)
  | text (s : String)
  | blob (b : ByteArray)
  deriving Inhabited

instance : Coe String Value := ⟨.text⟩
instance : Coe Int64 Value := ⟨.int⟩
instance : Coe Nat Value := ⟨fun n => .int n.toInt64⟩
instance : Coe Int Value := ⟨fun n => .int n.toInt64⟩
instance : Coe Bool Value := ⟨fun b => .int (if b then 1 else 0)⟩
instance : Coe ByteArray Value := ⟨.blob⟩
instance [Coe α Value] : Coe (Option α) Value := ⟨fun | none => .null | some a => (a : Value)⟩

namespace Value

def toInt? : Value → Option Int64
  | .int i => some i
  | .real r => some (Float.toInt64 r)
  | .text s => s.toInt?.map Int.toInt64
  | _ => none

def toNat? (v : Value) : Option Nat := v.toInt?.map fun i => i.toInt.toNat

def toText? : Value → Option String
  | .text s => some s
  | .int i => some (toString i)
  | .real r => some (toString r)
  | .blob b => some (String.fromUTF8! b)
  | .null => none

def toBlob? : Value → Option ByteArray
  | .blob b => some b
  | .text s => some s.toUTF8
  | _ => none

def toBool? (v : Value) : Option Bool := v.toInt?.map (· != 0)

def textD (v : Value) (d : String := "") : String := v.toText?.getD d
def natD (v : Value) (d : Nat := 0) : Nat := v.toNat?.getD d
def intD (v : Value) (d : Int64 := 0) : Int64 := v.toInt?.getD d
def boolD (v : Value) (d : Bool := false) : Bool := v.toBool?.getD d
def isNull : Value → Bool | .null => true | _ => false

end Value

/-- A result row with its column names. -/
structure Row where
  names : Array String
  cells : Array Value
  deriving Inhabited

namespace Row

def get? (r : Row) (name : String) : Option Value :=
  (r.names.findIdx? (· == name)).bind fun i => r.cells[i]?

def get (r : Row) (name : String) : Value := (r.get? name).getD .null
def text (r : Row) (name : String) (d : String := "") : String := (r.get name).textD d
def nat (r : Row) (name : String) (d : Nat := 0) : Nat := (r.get name).natD d
def int (r : Row) (name : String) (d : Int64 := 0) : Int64 := (r.get name).intD d
def bool (r : Row) (name : String) (d : Bool := false) : Bool := (r.get name).boolD d
def blob (r : Row) (name : String) : ByteArray := ((r.get name).toBlob?).getD .empty
def text? (r : Row) (name : String) : Option String := (r.get name).toText?
def nat? (r : Row) (name : String) : Option Nat := (r.get name).toNat?

end Row

/-- The process's one connection. -/
structure Db where
  conn : Conn
  lock : Std.Mutex Unit

def openFile (path : String) : IO Db := do
  let conn ← openRaw path
  execRaw conn "PRAGMA journal_mode=WAL; PRAGMA synchronous=NORMAL; PRAGMA foreign_keys=ON; PRAGMA temp_store=MEMORY;"
  return { conn, lock := ← Std.Mutex.new () }

def openMemory : IO Db := do
  let conn ← openRaw ":memory:"
  execRaw conn "PRAGMA foreign_keys=ON;"
  return { conn, lock := ← Std.Mutex.new () }

private def bindAll (st : Stmt) (args : Array Value) : IO Unit := do
  for h : i in [:args.size] do
    let idx := (i + 1).toUInt32
    match args[i] with
    | .null => bindNull st idx
    | .int v => bindInt st idx v
    | .real v => bindReal st idx v
    | .text v => bindText st idx v
    | .blob v => bindBlob st idx v

private def readCell (st : Stmt) (i : UInt32) : Value :=
  match columnType st i with
  | 1 => .int (columnInt st i)
  | 2 => .real (columnReal st i)
  | 3 => .text (columnText st i)
  | 4 => .blob (columnBlob st i)
  | _ => .null

private def readRows (st : Stmt) : IO (Array Row) := do
  let n := columnCount st
  let names := (Array.range n.toNat).map fun i => columnName st i.toUInt32
  let mut rows := #[]
  while ← step st do
    let cells := (Array.range n.toNat).map fun i => readCell st i.toUInt32
    rows := rows.push { names, cells }
  return rows

/-- Run a statement without the lock. Callers inside `transaction` use these. -/
def Db.execUnlocked (db : Db) (sql : String) (args : Array Value := #[]) : IO Unit := do
  if args.isEmpty then
    execRaw db.conn sql
  else
    let st ← prepare db.conn sql
    bindAll st args
    discard <| step st
    reset st

def Db.queryUnlocked (db : Db) (sql : String) (args : Array Value := #[]) : IO (Array Row) := do
  let st ← prepare db.conn sql
  bindAll st args
  let rows ← readRows st
  reset st
  return rows

/-- Run `k` holding the connection lock. -/
def Db.withLock (db : Db) (k : IO α) : IO α :=
  db.lock.atomically (fun _ => k)

def Db.exec (db : Db) (sql : String) (args : Array Value := #[]) : IO Unit :=
  db.withLock (db.execUnlocked sql args)

def Db.query (db : Db) (sql : String) (args : Array Value := #[]) : IO (Array Row) :=
  db.withLock (db.queryUnlocked sql args)

def Db.queryOne (db : Db) (sql : String) (args : Array Value := #[]) : IO (Option Row) := do
  return (← db.query sql args)[0]?

def Db.changes (db : Db) : IO Int64 := changesRaw db.conn

/-- Run a statement and answer how many rows it changed, both under the lock, so another
writer's statement cannot land between them (`changes()` is per connection). -/
def Db.execCount (db : Db) (sql : String) (args : Array Value := #[]) : IO Nat :=
  db.withLock do
    db.execUnlocked sql args
    return (← changesRaw db.conn).toInt.toNat
def Db.lastInsertRowid (db : Db) : IO Int64 := lastInsertRowidRaw db.conn

/-- Run `k` in one transaction under the lock. Rolls back on any exception. -/
def Db.transaction (db : Db) (k : IO α) : IO α :=
  db.withLock do
    execRaw db.conn "BEGIN IMMEDIATE"
    try
      let r ← k
      execRaw db.conn "COMMIT"
      return r
    catch e =>
      try execRaw db.conn "ROLLBACK" catch _ => pure ()
      throw e

end Server.Db
