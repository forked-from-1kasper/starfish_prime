fun println x = (print x; print "\n")
val printexc = println o Exception.show

local
  fun catchExn x = fn EOF => NONE | exn => (printexc exn; SOME x)
  fun readLoop f x = TextIO.scanStream (Reader.iter Reader.expr f catchExn x)

  fun printEval (E1, e1) =
  let
    val (E2, e2) = Expr.eval E1 e1
  in
    println (Expr.show e2); E2
  end

  val imported = ref (Bag.empty ())
in
  fun repl E0 = Option.app repl (readLoop printEval E0 TextIO.stdIn)

  fun loadChan E0 = ignore o readLoop (fn (E, e) => #1 (Expr.eval E e)) E0

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