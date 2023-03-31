fun println x = (print x; print "\n")
val printexc = println o Exception.show

local
  fun scan g = ignore o TextIO.scanStream (Reader.iter Reader.expr g)
in
  fun repl E =
    scan (println o Expr.show o Expr.eval E) TextIO.stdIn
    handle EOF => () | exn => (printexc exn; repl E)

  fun load E filename =
  let
    val chan = TextIO.openIn filename
  in
    scan (ignore o Expr.eval E) chan
    handle EOF => () | exn => printexc exn;
    TextIO.closeIn chan
  end
end

val () =
let
  val E = Environment.init (Dict.empty ())
in
  upload E builtin; List.app (load E) (CommandLine.arguments ()); repl E
end