(* Assemblages manipulation *)

fun postulate ident = (eager o unary) (fn (_, e) => Theorem (ident, Expr.getAssemblage e))

fun deftheory E = fn
    []    => raise (TooFewParams [])
| e :: es =>
let
  val E' = Environment.upLocal E "postulate" (postulate (Expr.getSymbol e))
in
  ignore (Expr.iprogn E' es); e
end

val var      = unary   (fn (_, e)          => Assemblage (Fv (Expr.getString e)))
val forme    = binary  (fn (_, e1, e2)     => Assemblage (Forme (Expr.getString e1, List.map Expr.getAssemblage (Expr.getList e2))))
val quanteur = ternary (fn (_, e1, e2, e3) => Assemblage (Assemblage.bind (Expr.getString e1) (Expr.getString e2) (Expr.getAssemblage e3)))

val substImpl = binary (fn (_, e1, e2) => Assemblage (Assemblage.subst (Dict.map Expr.getAssemblage (Expr.getDict e1)) (Expr.getAssemblage e2)))
val freeImpl  = binary (fn (_, e1, e2) => Assemblage (Assemblage.unbind (Expr.getAssemblage e1) (Expr.getAssemblage e2)))

val rebindImpl = binary (fn (_, e1, e2) => Assemblage (Assemblage.rebind (Expr.getString e1) (Expr.getAssemblage e2)))
val fvImpl     = unary  (fn (_, e)      => Set (Assemblage.fv (Expr.getAssemblage e)))
val formesImpl = unary  (fn (_, e)      => Set (Assemblage.formes (Expr.getAssemblage e)))
val occurImpl  = binary (fn (_, e1, e2) => Bool (Assemblage.occur (Expr.getString e1) (Expr.getAssemblage e2)))
val kindofImpl = unary  (fn (_, e)      => Symbol (Assemblage.kind (Expr.getAssemblage e)))
val unifyImpl  = binary (fn (_, e1, e2) => Dict (Dict.map Assemblage (Assemblage.unify (Dict.empty ()) (Expr.getAssemblage e1) (Expr.getAssemblage e2))))

val assemblageImpl = unary (fn (_, e) => Assemblage (Expr.getTheorem e))
val theoryImpl     = unary (fn (E, e) => String (Expr.getTheory e))

fun caseImpl E =
let
  fun loop ss = fn
    []             => Expr.eps
  | [e]            => Expr.ieval (Dict.fold (fn k => fn v => fn E' => Environment.upLocal E' k (Assemblage v)) ss E) e
  | e1 :: e2 :: es =>
  let
    val t1 = Expr.getAssemblage (Expr.ieval E e1)
    val t2 = Expr.getAssemblage (Expr.ieval E e2)
  in
    loop (Assemblage.unify ss t2 t1) es
  end
in
  loop (Dict.empty ())
end

val Math =
[("deftheory",  special deftheory),
 ("var",        eager var),
 ("forme",      eager forme),
 ("quanteur",   eager quanteur),
 ("subst",      eager substImpl),
 ("free",       eager freeImpl),
 ("rebind",     eager rebindImpl),
 ("fv",         eager fvImpl),
 ("formes",     eager formesImpl),
 ("kindof",     eager kindofImpl),
 ("occur?",     eager occurImpl),
 ("assemblage", eager assemblageImpl),
 ("theory",     eager theoryImpl),
 ("unify",      eager unifyImpl),
 ("case",       special caseImpl)]