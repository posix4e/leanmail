import Core.Text.Str

namespace Core.Text.Charset

/-! # Bytes to text

UTF-8 with replacement characters, and the single-byte charsets mail still carries. Labels follow
the WHATWG Encoding Standard the way browsers and postal-mime do: `iso-8859-1`, `us-ascii` and
`latin1` all mean windows-1252. Anything unknown decodes as windows-1252 too, lossily, which is
what postal-mime falls back to. -/

private def pushScalar (out : String) (n : Nat) : String :=
  if n < 0xD800 || (0xDFFF < n && n ≤ 0x10FFFF) then out.push (Char.ofNat n) else out.push '�'

/-- UTF-8 with U+FFFD for every maximal invalid subsequence (WHATWG decoder). -/
def utf8Lossy (b : ByteArray) : String :=
  match String.fromUTF8? b with
  | some s => s
  | none => Id.run do
    let mut out := ""
    let mut i := 0
    let n := b.size
    while i < n do
      let c := b[i]!.toNat
      if c < 0x80 then
        out := out.push (Char.ofNat c)
        i := i + 1
      else
        -- lead byte: how many continuation bytes, and the bounds for the second byte
        let (need, lo, hi, init) : Nat × Nat × Nat × Nat :=
          if 0xC2 ≤ c && c ≤ 0xDF then (1, 0x80, 0xBF, c &&& 0x1F)
          else if c == 0xE0 then (2, 0xA0, 0xBF, c &&& 0x0F)
          else if (0xE1 ≤ c && c ≤ 0xEC) || c == 0xEE || c == 0xEF then (2, 0x80, 0xBF, c &&& 0x0F)
          else if c == 0xED then (2, 0x80, 0x9F, c &&& 0x0F)
          else if c == 0xF0 then (3, 0x90, 0xBF, c &&& 0x07)
          else if 0xF1 ≤ c && c ≤ 0xF3 then (3, 0x80, 0xBF, c &&& 0x07)
          else if c == 0xF4 then (3, 0x80, 0x8F, c &&& 0x07)
          else (0, 0, 0, 0)
        if need == 0 then
          out := out.push '�'
          i := i + 1
        else
          let mut cp := init
          let mut j := 1
          let mut ok := true
          while j ≤ need do
            if i + j ≥ n then
              ok := false
              break
            let d := b[i + j]!.toNat
            let (l, h) := if j == 1 then (lo, hi) else (0x80, 0xBF)
            if d < l || h < d then
              ok := false
              break
            cp := (cp <<< 6) ||| (d &&& 0x3F)
            j := j + 1
          if ok then
            out := pushScalar out cp
            i := i + need + 1
          else
            out := out.push '�'
            i := i + j
    return out

/-- windows-1252: Latin-1 with the 0x80–0x9F block mapped to the Encoding Standard's values. -/
private def cp1252High : Array Nat := #[
  0x20AC, 0x0081, 0x201A, 0x0192, 0x201E, 0x2026, 0x2020, 0x2021, 0x02C6, 0x2030, 0x0160, 0x2039, 0x0152, 0x008D, 0x017D, 0x008F,
  0x0090, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014, 0x02DC, 0x2122, 0x0161, 0x203A, 0x0153, 0x009D, 0x017E, 0x0178]

def windows1252 (b : ByteArray) : String := Id.run do
  let mut out := ""
  for x in b.toList do
    let c := x.toNat
    if 0x80 ≤ c && c ≤ 0x9F then out := out.push (Char.ofNat (cp1252High[c - 0x80]!))
    else out := out.push (Char.ofNat c)
  return out

/-- ISO-8859-15: Latin-1 with eight positions replaced. -/
def iso885915 (b : ByteArray) : String := Id.run do
  let mut out := ""
  for x in b.toList do
    let c := x.toNat
    let cp := match c with
      | 0xA4 => 0x20AC | 0xA6 => 0x0160 | 0xA8 => 0x0161 | 0xB4 => 0x017D
      | 0xB8 => 0x017E | 0xBC => 0x0152 | 0xBD => 0x0153 | 0xBE => 0x0178
      | n => n
    out := out.push (Char.ofNat cp)
  return out

/-- Strip the decorations clients add to a label: `x-`, `cs`, separators. -/
private def normalizeLabel (label : String) : String :=
  let l := label.toLower
  let l := if l.startsWith "x-ms-" then String.ofList (l.toList.drop 5) else if l.startsWith "x-" then String.ofList (l.toList.drop 2) else if l.startsWith "cs" then String.ofList (l.toList.drop 2) else l
  String.ofList (l.toList.filter fun c => !(c == ' ' || c == '.' || c == '_' || c == '-'))

inductive Kind | utf8 | cp1252 | latin9

/-- The WHATWG labels of the encodings this module implements; anything else is windows-1252. -/
private def labelKind? (l : String) : Option Kind :=
  match l with
  | "utf-8" | "utf8" | "unicode-1-1-utf-8" | "unicode11utf8" | "unicode20utf8" | "x-unicode20utf8" => some .utf8
  | "iso-8859-15" | "iso8859-15" | "iso885915" | "iso_8859-15" | "l9" | "csisolatin9" => some .latin9
  | _ => none

/-- postal-mime's `getDecoder`: the label as written, then the label with its decorations stripped,
then windows-1252. -/
def kindOf (label : String) : Kind :=
  let l := Str.trimJs label |>.toLower
  if l.isEmpty then .utf8 else
  match labelKind? l with
  | some k => k
  | none => (labelKind? (normalizeLabel l)).getD .cp1252

/-- `TextDecoder.decode`: a leading UTF-8 byte order mark is consumed. -/
private def stripBom (b : ByteArray) : ByteArray :=
  if b.size ≥ 3 && b[0]! == 0xEF && b[1]! == 0xBB && b[2]! == 0xBF then b.extract 3 b.size else b

/-- Decode `bytes` as `label`, the way a `TextDecoder` for that label would. -/
def decode (label : String) (bytes : ByteArray) : String :=
  match kindOf label with
  | .utf8 => utf8Lossy (stripBom bytes)
  | .cp1252 => windows1252 bytes
  | .latin9 => iso885915 bytes

end Core.Text.Charset
