(* Formulae manipulation *)

fun postulate ident = (eager o unary) (fn (_, e) => Theorem (ident, Expr.getFormula e))

fun deftheory E = fn
    []    => raise (TooFewParams [])
| e :: es =>
let
  val E' = Environment.upLocal E "postulate" (postulate (Expr.getSymbol e))
in
  List.app (ignore o Expr.eval E') es; e
end

val var    = unary   (fn (_, e)          => Formula (Fv (Expr.getString e)))
val app    = binary  (fn (_, e1, e2)     => Formula (App (Expr.getString e1, List.map Expr.getFormula (Expr.getList e2))))
val binder = ternary (fn (_, e1, e2, e3) => Formula (Formula.bind (Expr.getString e1) (Expr.getString e2) (Expr.getFormula e3)))

val substImpl = binary (fn (_, e1, e2) => Formula (Formula.subst (Dict.map Expr.getFormula (Expr.getDict e1)) (Expr.getFormula e2)))
val freeImpl  = binary (fn (_, e1, e2) => Formula (Formula.unbind (Expr.getFormula e1) (Expr.getFormula e2)))

val rebindImpl = binary (fn (_, e1, e2) => Formula (Formula.rebind (Expr.getString e1) (Expr.getFormula e2)))
val fvImpl     = unary  (fn (_, e)      => Set (Formula.fv (Expr.getFormula e)))
val occurImpl  = binary (fn (_, e1, e2) => Bool (Formula.occur (Expr.getString e1) (Expr.getFormula e2)))
val kindofImpl = unary  (fn (_, e)      => Symbol (Formula.kind (Expr.getFormula e)))
val unifyImpl  = binary (fn (_, e1, e2) => Dict (Dict.map Formula (Formula.unify (Dict.empty ()) (Expr.getFormula e1) (Expr.getFormula e2))))

val formulaImpl = unary (fn (_, e) => Formula (Expr.getTheorem e))
val theoryImpl  = unary (fn (_, e) => String (Expr.getTheory e))

fun caseImpl E =
let
  fun loop ss = fn
    []             => Expr.eps
  | [e]            => Expr.eval (Dict.fold (fn k => fn v => fn E' => Environment.upLocal E' k (Formula v)) ss E) e
  | e1 :: e2 :: es =>
  let
    val t1 = Expr.getFormula (Expr.eval E e1)
    val t2 = Expr.getFormula (Expr.eval E e2)
  in
    loop (Formula.unify ss t2 t1) es
  end
in
  loop (Dict.empty ())
end

val Math =
[("deftheory", special deftheory),
 ("var",       eager var),
 ("app",       eager app),
 ("binder",    eager binder),
 ("subst",     eager substImpl),
 ("free",      eager freeImpl),
 ("rebind",    eager rebindImpl),
 ("fv",        eager fvImpl),
 ("kindof",    eager kindofImpl),
 ("occur?",    eager occurImpl),
 ("formula",   eager formulaImpl),
 ("theory",    eager theoryImpl),
 ("unify",     eager unifyImpl),
 ("case",      special caseImpl)]