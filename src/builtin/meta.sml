(* Basic Lisp operators *)

val applyImpl = binary (fn (E, e1, e2) => Expr.getLam e1 (E, List.map Quote (Expr.getList e2)))

val equal    = binary (fn (_, e1, e2) => Bool (Expr.equal e1 e2))
val define   = binary (fn (E, e1, e2) => (Environment.upGlobal E (Expr.getSymbol e1) (Expr.eval E e2); e1))
val evalImpl = unary  (fn (E, e)      => Expr.eval E e)

val symbolImpl = unary (fn (E, e) => Symbol (Expr.getString e))
val typeofImpl = unary (fn (_, e) => Symbol (Expr.typeof e))

val Meta =
[("lambda",   lambda),
 ("\206\187", lambda),
 ("apply",    eager applyImpl),
 ("macro",    macro),
 ("define",   special define),
 ("quote",    special (unary (fn (_, e) => e))),
 ("symbol",   eager symbolImpl),
 ("eval",     eager evalImpl),
 ("=",        eager equal),
 ("nil",      Expr.eps),
 ("typeof",   eager typeofImpl)]