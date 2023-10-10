val printImpl   = fn E => fn e => (List.app (print o printExpr) e; Expr.eps)
val newlineImpl = nulary (fn E => (print "\n"; Expr.eps))

fun readImpl E stxs =
let
  val t = case stxs of
    []  => TextIO.scanStream Reader.expr TextIO.stdIn
  | [e] => StringCvt.scanString Reader.expr (Expr.getString e)
  | es  => raise (TooManyParams es)
in
  case t of SOME e => e | NONE => raise NoExpression
end

fun set bag = List (List.map String (Bag.fold (fn x => fn xs => x :: xs) bag []))

fun dict k v =
let
  fun loop buff = fn
    []             => buff
  | [e]            => raise (Failure "dict")
  | e1 :: e2 :: es => loop (Dict.add (k e1) (v e2) buff) es
in
  loop (Dict.empty ())
end

val setImpl  = unary (fn (_, e) => Set (List.foldl (fn (x, t) => Bag.add (Expr.getString x) t) (Bag.empty ()) (Expr.getList e)))
val dictImpl = unary (fn (_, e) => Dict (dict Expr.getString idfun (Expr.getList e)))

val IO = (* Input/output *)
[("print",   eager printImpl),
 ("newline", eager newlineImpl),
 ("read",    eager readImpl)]

val Data = (* Dict/Set *)
[("set",  eager setImpl),
 ("dict", eager dictImpl)]

val builtin = Arithmetic @ Meta @ LinkedList @ Control @ IO @ Data @ Math