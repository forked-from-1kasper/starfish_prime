exception InvalidEscape of string
exception MismatchedBracket
exception UnexpectedEOF
exception NoExpression

infix 7 <|>
infix 8 <*
infix 8 *>
infix 9 <$>

fun error exn getc chan = raise exn

fun (f <$> g) getc chan =
  case g getc chan of
  SOME (v, rest) => SOME (f v, rest)
| NONE           => NONE

fun (f <|> g) getc chan =
  case f getc chan of
  SOME (v1, rest) => SOME (v1, rest)
| NONE            =>
  case g getc chan of
  SOME (v2, rest) => SOME (v2, rest)
| NONE            => NONE

fun (f <* g) getc chan =
  case f getc chan of
  NONE           => NONE
| SOME (v, rest) => case g getc rest of
  NONE           => NONE
| SOME (_, fin)  => SOME (v, fin)

fun (f *> g) getc chan =
  case f getc chan of
  NONE           => NONE
| SOME (_, rest) => case g getc rest of
  NONE           => NONE
| SOME (v, fin)  => SOME (v, fin)

fun manyUntil f g getc =
let
  fun loop acc chan = case g getc chan of
    SOME ((), rest) => SOME (List.rev acc, rest)
  | NONE            => case f getc chan of
    NONE            => NONE
  | SOME (v, chan') => loop (v :: acc) chan'
in
  loop []
end

fun pure v getc chan = SOME (v, chan)
fun fail getc chan = NONE

fun stop getc chan = NONE

fun eof getc chan = case getc chan of NONE => SOME ((), chan) | SOME _ => NONE

fun ch c1 getc chan = case getc chan of
  SOME (c2, rest) => if c1 = c2 then SOME ((), rest) else NONE
| NONE            => NONE

fun lookahead f getc chan = case getc chan of
  SOME (c, _) => if f c then SOME ((), chan) else NONE
| NONE        => NONE

fun negate b = fn x  => not (b x)
fun or b1 b2 = fn x  => b1 x orelse b2 x
fun equal c1 = fn c2 => c1 = c2

fun until f getc chan = SOME (StringCvt.splitl (negate f) getc chan)
fun until1 f getc chan = case StringCvt.splitl (negate f) getc chan of ("", _) => NONE | w => SOME w

fun dropBefore f g getc chan = g getc (StringCvt.dropl f getc chan)

fun dropAfter f g getc chan = case g getc chan of
  NONE           => NONE
| SOME (v, rest) => SOME (v, StringCvt.dropl f getc rest)

fun fix p getc chan = p (fix p) getc chan

structure Reader =
struct
  fun quoteImpl e = List [Lambda (fn _ => e)]

  val ident = fn
    "false" => Bool false
  | "true"  => Bool true
  | "nil"   => Expr.eps
  | x       => Symbol x

  fun cstring x = case String.fromCString x of
    NONE   => raise (InvalidEscape x)
  | SOME y => String y

  fun bracket c = c = #"(" orelse c = #")"
  fun nl c      = c = #"\r" orelse c = #"\n"

  fun expr () =
  let
    val special = or Char.isSpace bracket
    val sep = lookahead special <|> eof

    val skipWS = dropBefore Char.isSpace

    fun loop p =
        (ch #")"  *> error MismatchedBracket)
    <|> (ch #";"  *> dropBefore (negate nl) (skipWS p))
    <|> (ch #"("  *> List <$> manyUntil (dropAfter Char.isSpace p) (ch #")"))
    <|> (ch #"\"" *> cstring <$> until (equal #"\"") <* ch #"\"")
    <|> (ch #"'"  *> quoteImpl <$> skipWS p)
    <|> (Int     <$> Int.scan StringCvt.DEC <* sep)
    <|> (Real    <$> Real.scan <* sep)
    <|> (ident   <$> until1 special)
    <|> (eof      *> error UnexpectedEOF)
  in
    skipWS (fix loop)
  end

  fun iter f g getc =
  let
    fun loop chan = case f getc chan of
      NONE            => SOME ((), chan)
    | SOME (v, chan') => (g v; loop chan')
  in
    loop
  end
end