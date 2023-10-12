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

fun addImpl E = fn
  [e1, e2]     => Set (Bag.add (Expr.getString e1) (Expr.getSet e2))
| [e1, e2, e3] => Dict (Dict.add (Expr.getString e1) e2 (Expr.getDict e3))
| es           => raise (InvalidArity ([2, 3], List.length es))

val removeImpl = binary (fn (_, k, Dict t) => Dict (Dict.remove (Expr.getString k) t)
                          | (_, k, Set t)  => Set (Bag.remove (Expr.getString k) t)
                          | (_, _, e)      => raise (TypeMismatch (e, ["dict", "set"])))

val memImpl = binary (fn (_, k, Dict t) => Bool (Dict.mem (Expr.getString k) t)
                       | (_, k, Set t)  => Bool (Bag.mem (Expr.getString k) t)
                       | (_, _, e)      => raise (TypeMismatch (e, ["dict", "set"])))

val getImpl = binary (fn (_, e1, e2) => Option.getOpt (Dict.get (Expr.getString e1) (Expr.getDict e2), Expr.eps))

val IO = (* Input/output *)
[("print",   eager printImpl),
 ("newline", eager newlineImpl),
 ("read",    eager readImpl)]

val Data = (* Dict/Set *)
[("set",    eager setImpl),
 ("dict",   eager dictImpl),
 ("add",    eager addImpl),
 ("get",    eager getImpl),
 ("mem",    eager memImpl),
 ("remove", eager removeImpl)]

val builtin = Arithmetic @ Meta @ LinkedList @ Control @ IO @ Data @ Math