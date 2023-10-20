fun println x = (print x; print "\n")
val printexc = println o Exception.show

local
  fun scan g = ignore o TextIO.scanStream (Reader.iter Reader.expr g)

  val imported = ref (Bag.empty ())
in
  fun repl E =
    scan (println o Expr.show o Expr.eval E) TextIO.stdIn
    handle EOF => () | exn => (printexc exn; repl E)

  fun loadChan E chan =
    scan (ignore o Expr.eval E) chan
    handle EOF => () | exn => printexc exn;

  fun loadFile E filename =
  let
    val chan = TextIO.openIn filename
  in
    imported := Bag.add filename (!imported);
    loadChan E chan; TextIO.closeIn chan
  end

  fun load E filename =
    if Bag.mem filename (!imported) then ()
    else loadFile E filename

  fun importImpl E =
    List.app (fn Symbol x => load E x
               | String y => load E y
               | e        => raise (TypeMismatch (e, ["symbol", "string"])))
end

val () =
let
  val E = Environment.init (Dict.empty ())
in
  upload E builtin; Environment.upGlobal E "import" (effect importImpl);
  List.app (load E) (CommandLine.arguments ()); repl E
end