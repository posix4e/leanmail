/-! # Command-line arguments, parsed by hand

`leanmail <command> [args] [--flag value]`. A flag in `boolFlags` takes no value; every other
`--name` (or `-o`) takes the next word, or the text after `=`. `--` ends the flags. A flag may
repeat (`--rcpt a --rcpt b`); `flag?` reads the last one, `flags` all of them. -/

namespace Cli

structure Args where
  positional : List String := []
  /-- In order of appearance, names without the dashes. -/
  named : List (String × String) := []
  switches : List String := []
  deriving Inhabited, Repr

/-- Flags that take no value. -/
def boolFlags : List String := ["json", "html", "raw", "color", "help", "h", "force", "local", "proxy", "all",
  "keep-forever"]

/-- Short flags that stand for a long one. -/
def shortFlag : String → String
  | "o" => "output"
  | "n" => "limit"
  | s => s

def Args.flag? (a : Args) (name : String) : Option String :=
  (a.named.reverse.find? (·.1 == name)).map (·.2)

def Args.flags (a : Args) (name : String) : List String :=
  (a.named.filter (·.1 == name)).map (·.2)

def Args.has (a : Args) (name : String) : Bool := a.switches.contains name

def Args.natFlag? (a : Args) (name : String) : Except String (Option Nat) :=
  match a.flag? name with
  | none => .ok none
  | some v => match v.toNat? with
    | some n => .ok (some n)
    | none => .error s!"--{name} wants a number, got '{v}'"

/-- The worker of `parseArgs`; the third argument is whether `--` has been seen. -/
partial def parseArgsGo : List String → Args → Bool → Except String Args
    | [], acc, _ => .ok { acc with positional := acc.positional.reverse, named := acc.named.reverse }
    | w :: rest, acc, true => parseArgsGo rest { acc with positional := w :: acc.positional } true
    | w :: rest, acc, false =>
      if w == "--" then parseArgsGo rest acc true
      else if w.startsWith "-" && w.length > 1 && w != "-" then
        let body := String.ofList (w.toList.dropWhile (· == '-'))
        let (name, inline) := match body.toList.span (· != '=') with
          | (n, []) => (String.ofList n, none)
          | (n, _ :: v) => (String.ofList n, some (String.ofList v))
        let name := shortFlag name
        if name.isEmpty then .error s!"bad flag '{w}'"
        else if boolFlags.contains name then
          if inline.isSome then .error s!"--{name} takes no value"
          else parseArgsGo rest { acc with switches := name :: acc.switches } false
        else match inline, rest with
          | some v, _ => parseArgsGo rest { acc with named := (name, v) :: acc.named } false
          | none, v :: rest' => parseArgsGo rest' { acc with named := (name, v) :: acc.named } false
          | none, [] => .error s!"--{name} needs a value"
      else parseArgsGo rest { acc with positional := w :: acc.positional } false

/-- Split `argv` into positional words, valued flags and switches. -/
def parseArgs (argv : List String) : Except String Args := parseArgsGo argv {} false

end Cli
