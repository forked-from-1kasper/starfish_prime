(* Control flow *)

fun fail E xs = raise (Failure (String.concatWith " " (List.map printExpr xs)))

fun ifImpl E = fn
  []           => Expr.eps
| e :: []      => Expr.ieval E e
| b :: e :: es =>
  case Expr.eval E b of
    (_, Bool true)  => Expr.ieval E e
  | (_, Bool false) => ifImpl E es
  | (_, _)          => raise (Failure "if?")

fun loopImpl E es =
let
  val truth = fn Bool false => false | _ => true
in
  while truth (Expr.iprogn E es) do (); Expr.eps
end

fun andImpl E = fn
    []    => true
| x :: xs => Expr.getBool (Expr.ieval E x) andalso andImpl E xs

fun orImpl E = fn
    []    => false
| x :: xs => Expr.getBool (Expr.ieval E x) orelse orImpl E xs

val notImpl    = unary  (fn (_, e)      => Bool (not (Expr.getBool e)))
val refImpl    = unary  (fn (_, e)      => Ref (ref e))
val derefImpl  = unary  (fn (_, e)      => !(Expr.getRef e))
val assignImpl = binary (fn (_, e1, e2) => Expr.getRef e1 := e2)

val Control =
[("if",      special ifImpl),
 ("true",    Bool true),
 ("false",   Bool false),
 ("and",     Lambda (fn (E, e) => (E, Bool (andImpl E e)))),
 ("or",      Lambda (fn (E, e) => (E, Bool (orImpl E e)))),
 ("not",     eager notImpl),
 ("ref",     eager refImpl),
 ("deref",   eager derefImpl),
 ("assign!", effect assignImpl),
 ("progn",   Lambda (fn (E, e) => Expr.progn E e)),
 ("dolocal", Lambda (fn (E, e) => (E, Expr.iprogn E e))),
 ("loop",    special loopImpl),
 ("fail",    eager fail)]