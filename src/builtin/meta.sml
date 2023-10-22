(* Basic Lisp operators *)

val applyImpl = binary (fn (E, e1, e2) => #2 (Expr.getLam e1 (E, List.map Quote (Expr.getList e2))))

val equal      = binary (fn (_, e1, e2) => Bool (Expr.equal e1 e2))
val defineImpl = binary (fn (E, e1, e2) => (Environment.upGlobal E (Expr.getString e1) e2; e1))
val evalImpl   = unary  (fn (E, e)      => Expr.ieval E e)

val symbolImpl = unary (fn (E, e) => Symbol (Expr.getString e))
val typeofImpl = unary (fn (_, e) => Symbol (Expr.typeof e))
val showImpl   = unary (fn (_, e) => String (Expr.show e))

fun variadic x ys E = Environment.upLocal E x (List ys)

fun uniadic xs ys E =
  ListPair.foldlEq (fn (k, v, E') => Environment.upLocal E' k v) E (List.map Expr.getSymbol xs, ys)
  handle ListPair.UnequalLengths => raise (InvalidArity ([List.length xs], List.length ys))

fun lambdaImpl unpack E1 body =
  Lambda (fn (E2, e) => (E2, Expr.iprogn (unpack (List.map (Expr.ieval E2) e) E1) body))

fun macroImpl unpack E1 body =
  Lambda (fn (E2, e) => Expr.eval E2 (Expr.iprogn (unpack e E1) body))

val deflocalImpl = binary (fn (E, e1, e2) =>
  let
    val t1 = Expr.getString (Expr.ieval E e1)
    val t2 = Expr.ieval E e2
  in
   (Environment.upLocal E t1 t2, String t1)
  end)

exception InvalidParamPack

val letImpl =
let
  fun unpackLet E0 = fn
    []             => E0
  | [_]            => raise InvalidParamPack
  | e1 :: e2 :: es => unpackLet (Environment.upLocal E0 (Expr.getSymbol e1) (Expr.ieval E0 e2)) es
in
  fn (E,   [])    => raise InvalidParamPack
   | (E, e :: es) => (E, Expr.iprogn (unpackLet E (Expr.getList e)) es)
end

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
 ("deflocal", Lambda (fn (E, e) => deflocalImpl E e)),
 ("let",      Lambda letImpl),
 ("quote",    special (unary #2)),
 ("symbol",   eager symbolImpl),
 ("eval",     eager evalImpl),
 ("show",     eager showImpl),
 ("=",        eager equal),
 ("nil",      Expr.eps),
 ("typeof",   eager typeofImpl)]