exception InvalidEscape of string
exception MismatchedBracket
exception NoExpression
exception EOF

infix 7 <|>
infix 8 <*
infix 8 *>
infix 9 <$>

fun error exn get T = raise exn

fun (f <$> g) get T =
  case g get T of
  SOME (v, rest) => SOME (f v, rest)
| NONE           => NONE

fun (f <|> g) get T =
  case f get T of
  SOME (v1, rest) => SOME (v1, rest)
| NONE            =>
  case g get T of
  SOME (v2, rest) => SOME (v2, rest)
| NONE            => NONE

fun (f <* g) get T =
  case f get T of
  NONE           => NONE
| SOME (v, rest) => case g get rest of
  NONE           => NONE
| SOME (_, fin)  => SOME (v, fin)

fun (f *> g) get T =
  case f get T of
  NONE           => NONE
| SOME (_, rest) => case g get rest of
  NONE           => NONE
| SOME (v, fin)  => SOME (v, fin)

fun manyUntil f g get =
let
  fun loop acc T = case g get T of
    SOME ((), rest) => SOME (List.rev acc, rest)
  | NONE            => case f get T of
    NONE            => NONE
  | SOME (v, T')    => loop (v :: acc) T'
in
  loop []
end

fun pure v get T = SOME (v, T)
fun fail get T = NONE

fun eof get T = case get T of NONE => SOME ((), T) | SOME _ => NONE

fun ch c1 get T = case get T of
  SOME (c2, rest) => if c1 = c2 then SOME ((), rest) else NONE
| NONE            => NONE

fun lookahead f get T = case get T of
  SOME (c, _) => if f c then SOME ((), T) else NONE
| NONE        => NONE

fun negate b = fn x  => not (b x)
fun or b1 b2 = fn x  => b1 x orelse b2 x
fun equal c1 = fn c2 => c1 = c2

fun until f get T = SOME (StringCvt.splitl (negate f) get T)

fun until1 f get T = case StringCvt.splitl (negate f) get T of
  ("", _) => NONE | w => SOME w

fun dropBefore f g get T = g get (StringCvt.dropl f get T)

fun dropAfter f g get T = case g get T of
  NONE           => NONE
| SOME (v, rest) => SOME (v, StringCvt.dropl f get rest)

fun fix P get T = P (fix P) get T

structure Reader =
struct
  val ident = fn
    "false" => Bool false
  | "true"  => Bool true
  | "nil"   => Expr.eps
  | x       => Symbol x

  fun fromCString x = case String.fromCString x of
    NONE   => raise (InvalidEscape x)
  | SOME y => String y

  fun bracket c = c = #"(" orelse c = #")"
  fun nl c      = c = #"\r" orelse c = #"\n"

  fun expr get T =
  let
    val special = or Char.isSpace bracket
    val sep = lookahead special <|> eof

    val skipWS = dropBefore Char.isSpace

    fun loop P =
        (ch #")"  *> error MismatchedBracket)
    <|> (ch #";"  *> dropBefore (negate nl) (skipWS P))
    <|> (ch #"("  *> List <$> manyUntil (dropAfter Char.isSpace P) (ch #")"))
    <|> (ch #"\"" *> fromCString <$> until (equal #"\"") <* ch #"\"")
    <|> (ch #"'"  *> Quote <$> skipWS P)
    <|> (Int     <$> Int.scan StringCvt.DEC <* sep)
    <|> (Real    <$> Real.scan <* sep)
    <|> (ident   <$> until1 special)
    <|> (eof      *> error EOF)
  in
    skipWS (fix loop) get T
  end

  fun iter f g get =
  let
    fun loop T = case f get T of
      NONE         => SOME ((), T)
    | SOME (v, T') => (g v; loop T')
  in
    loop
  end
end