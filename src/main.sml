fun repl E prompt =
let
  val t = Tokenizer.ofStream TextIO.stdIn

  fun loop () =
    (prompt (); case Reader.expr t of
      SOME e => (print (Expr.show (Expr.eval E e) ^ "\n"); loop ())
    | NONE   => ())
    handle ex => (print (Exception.show ex ^ "\n"); loop ())
in
  loop ()
end

fun load E filename =
let
  val chan = TextIO.openIn filename
  val t = Tokenizer.ofStream chan

  fun loop () =
    (case Reader.expr t of
      SOME e => (ignore (Expr.eval E e); loop ())
    | NONE   => ())
    handle ex => print (Exception.show ex ^ "\n")
in
  loop (); TextIO.closeIn chan
end

val () =
let
  val E = Environment.init (Dict.empty ())
  val tokenizer = Tokenizer.ofStream TextIO.stdIn
in
  upload E builtin;
  List.app (load E) (CommandLine.arguments ());
  repl E (fn () => print "LISP> ")
end