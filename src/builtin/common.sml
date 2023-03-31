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

fun nulary f E = fn
  [] => f E
| xs => raise (InvalidArity (0, List.length xs))

fun unary f E = fn
  [x] => f (E, x)
| xs  => raise (InvalidArity (1, List.length xs))

fun binary f E = fn
  [x, y] => f (E, x, y)
| xs     => raise (InvalidArity (2, List.length xs))

fun ternary f E = fn
  [x, y, z] => f (E, x, y, z)
| xs        => raise (InvalidArity (3, List.length xs))

fun quaternary f E = fn
  [x, y, z, w] => f (E, x, y, z, w)
| xs           => raise (InvalidArity (4, List.length xs))

fun eager   f = Lambda (fn (E, e) => f E (List.map (Expr.eval E) e))
fun special f = Lambda (fn (E, e) => f E e)

fun variadic x ys E = Environment.upLocal E x (List ys)

fun uniadic xs ys E =
  ListPair.foldl (fn (k, v, E') => Environment.upLocal E' k v) E (List.map Expr.getSymbol xs, ys)
  handle ListPair.UnequalLengths => raise (InvalidArity (List.length xs, List.length ys))

fun lambdaImpl unpack E1 body =
  Lambda (fn (E2, e) => Expr.progn (unpack (List.map (Expr.eval E2) e) E1) body)

fun macroImpl unpack E1 body =
  Lambda (fn (E2, e) => Expr.eval E2 (Expr.progn (unpack e E1) body))

exception InvalidParamPack

val lambda =
  special (fn E => fn
      List xs  :: ys => lambdaImpl (uniadic xs) E ys
    | Symbol x :: ys => lambdaImpl (variadic x) E ys
    | _              => raise InvalidParamPack)

val macro =
  special (fn E => fn
      List xs  :: ys => macroImpl (uniadic xs) E ys
    | Symbol x :: ys => macroImpl (variadic x) E ys
    | _              => raise InvalidParamPack)