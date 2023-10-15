(* Basic Lisp operators *)

val applyImpl = binary (fn (E, e1, e2) => Expr.getLam e1 (E, List.map Quote (Expr.getList e2)))

val equal      = binary (fn (_, e1, e2) => Bool (Expr.equal e1 e2))
val defineImpl = binary (fn (E, e1, e2) => (Environment.upGlobal E (Expr.getString e1) e2; e1))
val evalImpl   = unary  (fn (E, e)      => Expr.eval E e)

val symbolImpl = unary (fn (E, e) => Symbol (Expr.getString e))
val typeofImpl = unary (fn (_, e) => Symbol (Expr.typeof e))
val showImpl   = unary (fn (_, e) => String (Expr.show e))

fun variadic x ys E = Environment.upLocal E x (List ys)

fun uniadic xs ys E =
  ListPair.foldlEq (fn (k, v, E') => Environment.upLocal E' k v) E (List.map Expr.getSymbol xs, ys)
  handle ListPair.UnequalLengths => raise (InvalidArity ([List.length xs], List.length ys))

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

val Meta =
[("lambda",   lambda),
 ("\206\187", lambda),
 ("apply",    eager applyImpl),
 ("macro",    macro),
 ("define",   eager defineImpl),
 ("quote",    special (unary (fn (_, e) => e))),
 ("symbol",   eager symbolImpl),
 ("eval",     eager evalImpl),
 ("show",     eager showImpl),
 ("=",        eager equal),
 ("nil",      Expr.eps),
 ("typeof",   eager typeofImpl)]