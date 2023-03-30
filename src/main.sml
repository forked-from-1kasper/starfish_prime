fun println x = (print x; print "\n")
val printexc = println o Exception.show

local
  fun scan g chan = ignore (TextIO.scanStream (Reader.iter (Reader.expr ()) g) chan)
in
  fun repl E =
    scan (println o Expr.show o Expr.eval E) TextIO.stdIn
    handle UnexpectedEOF => () | exn => (printexc exn; repl E)

  fun load E filename =
  let
    val chan = TextIO.openIn filename
  in
    scan (ignore o Expr.eval E) chan
    handle UnexpectedEOF => () | exn => printexc exn;
    TextIO.closeIn chan
  end
end

val () =
let
  val E = Environment.init (Dict.empty ())
in
  upload E builtin; List.app (load E) (CommandLine.arguments ()); repl E
end