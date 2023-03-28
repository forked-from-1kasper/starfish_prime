datatype token =
  Literal of string
| Ident of string
| Lparen
| Rparen
| Quote
| Eof

exception InvalidEscape of string
exception MismatchedBracket
exception UnexpectedEOF
exception NoExpression

structure Tokenizer =
struct
  type t = {get : unit -> char, buffer : char list ref}

  exception EOF

  fun negate b = fn x  => not (b x)
  fun or b1 b2 = fn x  => b1 x orelse b2 x
  fun equal c1 = fn c2 => c1 = c2

  fun whitespace c = c = #" " orelse c = #"\n" orelse c = #"\r" orelse c = #"\t"
  fun bracket c    = c = #"(" orelse c = #")"
  fun nl c         = c = #"\r" orelse c = #"\n"

  val special = or whitespace bracket

  fun ofStream chan = {buffer = ref [], get = fn () => case TextIO.input1 chan of SOME c => c | NONE => raise EOF}

  fun ofString s =
  let
    val i = ref 0
    val length = String.size s
  in
    {buffer = ref [], get = fn () =>
      let
        val j = !i
      in
        i := j + 1;
        if j < length then
          String.sub (s, j)
        else raise EOF
      end}
  end

  fun push {buffer, get} c = (buffer := c :: !buffer)

  fun pop {buffer, get} = case !buffer of
    c :: cs => (buffer := cs; c)
  |   []    => get ()

  fun until t sep =
  let
    val retval = ref []

    fun content () = String.implode (List.rev (! retval))

    fun loop c =
      if sep c then (SOME c, content ())
      else (retval := c :: !retval; loop (pop t))
      handle EOF => (NONE, content ())
  in
    loop (pop t)
  end

  fun discard t f =
  let
    val c = pop t
  in
    if f c then discard t f else push t c
  end

  val next =
  let
    fun loop t = case pop t of
      #"("  => Lparen
    | #")"  => Rparen
    | #";"  => (discard t (negate nl); loop t)
    | #"\"" => let val (c, x) = until t (equal #"\"") in if Option.isSome c then Literal x else raise UnexpectedEOF end
    | #"'"  => Quote
    | c     => if whitespace c then loop t else (push t c; let val (c, x) = until t special in (Option.app (push t) c; Ident x) end)
  in
    fn t => loop t handle EOF => Eof
  end
end

structure Reader =
struct
  fun numeral x =
    case Int.fromString x of SOME z => Int z | NONE =>
    case Real.fromString x of SOME r => Real r | NONE => Symbol x

  val ident = fn
    "false" => Bool false
  | "true"  => Bool true
  | "nil"   => Expr.eps
  | x       => numeral x

  fun constImpl e = List [Lambda (fn _ => e)]

  fun expr t =
  let
    fun loop tok = case tok of
      Eof       => NONE
    | Lparen    => SOME (List (many ()))
    | Rparen    => raise MismatchedBracket
    | Ident x   => SOME (ident x)
    | Literal x => (case String.fromString x of NONE => raise (InvalidEscape x) | SOME y => SOME (String y))
    | Quote     => case loop (Tokenizer.next t) of
        NONE   => NONE
      | SOME e => SOME (constImpl e)

    and many () = case Tokenizer.next t of
      Rparen => []
    | tok    => case loop tok of
        SOME e => e :: many ()
      | NONE   => raise UnexpectedEOF
  in
    loop (Tokenizer.next t)
  end
end