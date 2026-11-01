/-! # A small escaping HTML builder

Pages are trees of `Node`: elements with attributes and children, text, and `raw` for fragments
the code itself wrote (never user data). `render` escapes every text node and every attribute
value, always double-quotes attributes, and writes void elements (`input`, `meta`, `link`, `br`,
`img`, `hr`) without a closing tag. Attribute and element names come from the code, never from
input, but are still filtered to `[a-z0-9-]` so a mistake cannot open a tag. -/

namespace Core.Html

inductive Node where
  | elem (tag : String) (attrs : List (String × String)) (children : List Node)
  | text (s : String)
  | raw (s : String)
  | frag (children : List Node)
  deriving Inhabited, Repr

/-- `& < > " '` as entities: safe in text and in a double-quoted attribute. -/
def escape (s : String) : String := Id.run do
  let mut out := ""
  for c in s.toList do
    out := match c with
      | '&' => out ++ "&amp;"
      | '<' => out ++ "&lt;"
      | '>' => out ++ "&gt;"
      | '"' => out ++ "&quot;"
      | '\'' => out ++ "&#39;"
      | c => out.push c
  return out

def voidTags : List String := ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "source", "wbr"]

private def safeName (s : String) : String :=
  String.ofList (s.toList.filter fun c => ('a' ≤ c && c ≤ 'z') || ('0' ≤ c && c ≤ '9') || c == '-')

partial def renderTo (out : String) : Node → String
  | .text s => out ++ escape s
  | .raw s => out ++ s
  | .frag cs => cs.foldl renderTo out
  | .elem tag attrs cs =>
    let t := safeName tag
    let o := attrs.foldl (init := out ++ "<" ++ t) fun o (k, v) => o ++ " " ++ safeName k ++ "=\"" ++ escape v ++ "\""
    let o := o ++ ">"
    if voidTags.contains t then o
    else (cs.foldl renderTo o) ++ "</" ++ t ++ ">"

def render (n : Node) : String := renderTo "" n

/-- A whole document: `<!doctype html>` then the tree. -/
def document (root : Node) : String := "<!doctype html>\n" ++ render root

/-! ## Shorthands -/

def el (tag : String) (attrs : List (String × String) := []) (children : List Node := []) : Node := .elem tag attrs children
def txt (s : String) : Node := .text s
def raw (s : String) : Node := .raw s
def frag (cs : List Node) : Node := .frag cs
def empty : Node := .frag []

/-- An element with only a text child. -/
def elt (tag : String) (attrs : List (String × String)) (s : String) : Node := .elem tag attrs [.text s]

/-- `when b n`: `n` or nothing. -/
def when (b : Bool) (n : Node) : Node := if b then n else empty

instance : Coe String Node := ⟨Node.text⟩

end Core.Html
