fun idfun x = x
fun const a b = a

fun flip (a, b) = (b, a)

val printExpr = fn
  Symbol x => "(symbol \"" ^ x ^ "\")"
| String s => s
| e        => Expr.show e

fun upload E = List.app (fn (k, v) => Environment.upGlobal E k v)

fun getUnary E e0 =
let
  val f = Expr.getLam e0
in
  fn e => f (E, [Quote e])
end

fun getBinary E e0 =
let
  val f = Expr.getLam e0
in
  fn (e1, e2) => f (E, [Quote e1, Quote e2])
end

fun getTernary E e0 =
let
  val f = Expr.getLam e0
in
  fn (e1, e2, e3) => f (E, [Quote e1, Quote e2, Quote e3])
end

fun nulary f E = fn
  [] => f E
| xs => raise (InvalidArity ([0], List.length xs))

fun unary f E = fn
  [x] => f (E, x)
| xs  => raise (InvalidArity ([1], List.length xs))

fun binary f E = fn
  [x, y] => f (E, x, y)
| xs     => raise (InvalidArity ([2], List.length xs))

fun ternary f E = fn
  [x, y, z] => f (E, x, y, z)
| xs        => raise (InvalidArity ([3], List.length xs))

fun quaternary f E = fn
  [x, y, z, w] => f (E, x, y, z, w)
| xs           => raise (InvalidArity ([4], List.length xs))

fun eager   f = Lambda (fn (E, e) => f E (List.map (Expr.eval E) e))
fun effect  f = Lambda (fn (E, e) => (f E (List.map (Expr.eval E) e); Expr.eps))
fun special f = Lambda (fn (E, e) => f E e)