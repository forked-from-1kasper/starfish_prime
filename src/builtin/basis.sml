fun idfun x = x
fun const a b = a

fun flip (a, b) = (b, a)

fun xor a b = (a orelse b) andalso not (a andalso b)
fun abj a b = a andalso (not b)

val equal    = binary (fn (_, e1, e2) => Bool (Expr.equal e1 e2))
val define   = binary (fn (E, e1, e2) => (Environment.upGlobal E (Expr.getSymbol e1) (Expr.eval E e2); e1))
val evalImpl = unary (fn (E, e) => Expr.eval E e)

val applyImpl = binary (fn (E, e1, e2) => Expr.getLam e1 (E, List.map Quote (Expr.getList e2)))

local
  val plus = fn
    (List [],  v)        => v
  | (v,        List [])  => v
  | (Int x,    Int y)    => Int (x + y)
  | (Int x,    Real y)   => Real (Real.fromInt x + y)
  | (Real x,   Int y)    => Real (x + Real.fromInt y)
  | (Real x,   Real y)   => Real (x + y)
  | (Bool b1,  Bool b2)  => Bool (xor b1 b2)
  | (String x, String y) => String (x ^ y)
  | (List xs,  List ys)  => List (xs @ ys)
  | (t1,       t2)       => raise (TypeMismatch (t1, [Expr.typeof t2]))

  val negate = fn
    Int x  => Int (~ x)
  | Real x => Real (~ x)
  | Bool b => Bool (not b)
  | t      => raise (TypeMismatch (t, ["int", "real", "bool"]))

  val minus = fn
    (Int x,   Int y)   => Int (x - y)
  | (Int x,   Real y)  => Real (Real.fromInt x - y)
  | (Real x,  Int y)   => Real (x - Real.fromInt y)
  | (Real x,  Real y)  => Real (x - y)
  | (Bool b1, Bool b2) => Bool (b1 = b2)
  | (t1,      t2)      => raise (TypeMismatch (t1, [Expr.typeof t2]))

  val mult = fn
    (List [],  v)       => v
  | (v,        List []) => v
  | (Int x,    Int y)   => Int (x * y)
  | (Int x,    Real y)  => Real (Real.fromInt x * y)
  | (Real x,   Int y)   => Real (x * Real.fromInt y)
  | (Real x,   Real y)  => Real (x * y)
  | (Bool b1,  Bool b2) => Bool (b1 andalso b2)
  | (t1,       t2)      => raise (TypeMismatch (t1, [Expr.typeof t2]))

  val inverse = fn
    Real x => Real (1.0 / x)
  | Bool b => Bool (not b)
  | t      => raise (TypeMismatch (t, ["real", "bool"]))

  val division = fn
    (Int x,   Int y)   => Int (x div y)
  | (Int x,   Real y)  => Real (Real.fromInt x / y)
  | (Real x,  Int y)   => Real (x / Real.fromInt y)
  | (Real x,  Real y)  => Real (x / y)
  | (Bool b1, Bool b2) => Bool (abj b1 b2)
  | (t1,      t2)      => raise (TypeMismatch (t1, [Expr.typeof t2]))
in
  val addImpl = eager (const (List.foldl plus Expr.eps))
  val subImpl = eager (const (fn   []    => Expr.eps
                               | e :: [] => negate e
                               | e :: es => List.foldl (minus o flip) e es))
  val mulImpl = eager (const (List.foldl mult Expr.eps))
  val divImpl = eager (const (fn   []    => Expr.eps
                               | e :: [] => inverse e
                               | e :: es => List.foldl (division o flip) e es))
end

val symbolImpl = unary (fn (E, e) => Symbol (Expr.getString e))

fun ifImpl E = fn
  []           => Expr.eps
| e :: []      => Expr.eval E e
| b :: e :: es =>
  case Expr.eval E b of
    Bool true  => Expr.eval E e
  | Bool false => ifImpl E es
  | _          => raise (Failure "if?")

fun loopImpl E es =
let
  val truth = fn Bool false => false | _ => true
in
  while truth (Expr.progn E es) do (); Expr.eps
end

fun andImpl E = fn
    []    => true
| x :: xs => Expr.getBool (Expr.eval E x) andalso andImpl E xs

fun orImpl E = fn
    []    => false
| x :: xs => Expr.getBool (Expr.eval E x) orelse orImpl E xs

val notImpl = unary (fn (_, e) => Bool (not (Expr.getBool e)))

val car = unary
  (fn (_, List xs)   => List.hd xs
    | (_, Formula t) => String (Formula.funsym t)
    | (_, e)         => raise (TypeMismatch (e, ["list", "formula"])))

val cdr = unary
  (fn (_, List xs)                    => List (List.tl xs)
    | (_, Formula (App (_, ts)))      => List (List.map Formula ts)
    | (_, Formula (Binder (_, _, t))) => Formula t
    | (_, Formula (Var _))            => raise (Failure "cdr")
    | (_, e)                          => raise (TypeMismatch (e, ["list", "formula"])))

val lengthImpl = unary
  (fn (_, List xs)  => Int (List.length xs)
    | (_, String x) => Int (String.size x)
    | (_, e)        => raise (TypeMismatch (e, ["list", "string"])))

val consImpl   = binary  (fn (_, e1, e2)     => List (e1 :: Expr.getList e2))
val nth        = binary  (fn (_, e1, e2)     => List.nth (Expr.getList e2, Expr.getInt e1))
val mapcarImpl = binary  (fn (E, e1, e2)     => List (List.map (getUnary E e1) (Expr.getList e2)))
val dolistImpl = binary  (fn (E, e1, e2)     => (List.app (ignore o getUnary E e1) (Expr.getList e2); Expr.eps))

local
  fun foldr0 f = fn
      []    => raise Empty
  | x :: [] => x
  | x :: xs => f (x, foldr0 f xs)

  fun foldl0 f = fn
      []    => raise Empty
  | x :: xs => List.foldl f x xs
in
  fun foldlImpl E = fn
    [e1, e2]     => foldl0 (getBinary E e1) (Expr.getList e2)
  | [e1, e2, e3] => List.foldl (getBinary E e1) e2 (Expr.getList e3)
  | es           => raise (InvalidArity (3, List.length es))

  fun foldrImpl E = fn
    [e1, e2]     => foldr0 (getBinary E e1) (Expr.getList e2)
  | [e1, e2, e3] => List.foldr (getBinary E e1) e2 (Expr.getList e3)
  | es           => raise (InvalidArity (3, List.length es))
end

val refImpl    = unary  (fn (_, e)      => Ref (ref e))
val derefImpl  = unary  (fn (_, e)      => !(Expr.getRef e))
val assignImpl = binary (fn (_, e1, e2) => (Expr.getRef e1 := e2; Expr.eps))

val printExpr = fn
  Symbol x => "(symbol \"" ^ x ^ "\")"
| String s => s
| e        => Expr.show e

fun fail E xs = raise (Failure (String.concatWith " " (List.map printExpr xs)))

val printImpl   = fn E => fn e => (List.app (print o printExpr) e; Expr.eps)
val newlineImpl = nulary (fn E => (print "\n"; Expr.eps))

val typeofImpl = unary (fn (_, e) => Symbol (Expr.typeof e))

fun readImpl E stxs =
let
  val t = case stxs of
    []  => TextIO.scanStream Reader.expr TextIO.stdIn
  | [e] => StringCvt.scanString Reader.expr (Expr.getString e)
  | es  => raise (TooManyParams es)
in
  case t of SOME e => e | NONE => raise NoExpression
end

fun postulate ident = (eager o unary) (fn (_, e) => Theorem (ident, Expr.getFormula e))

fun deftheory E = fn
    []    => raise (TooFewParams [])
| e :: es =>
let
  val E' = Environment.upLocal E "postulate" (postulate (Expr.getSymbol e))
in
  List.app (ignore o (Expr.eval E')) es; e
end

val var    = unary   (fn (_, e)          => Formula (Var (Expr.getString e)))
val app    = binary  (fn (_, e1, e2)     => Formula (App (Expr.getString e1, List.map Expr.getFormula (Expr.getList e2))))
val binder = ternary (fn (_, e1, e2, e3) => Formula (Binder (Expr.getString e1, Expr.getString e2, Expr.getFormula e3)))

fun set bag = List (List.map String (Bag.fold (fn x => fn xs => x :: xs) bag []))

val fvImpl = unary (fn (_, e) => Set (Formula.fv (Expr.getFormula e)))
val bvImpl = unary (fn (_, e) => Set (Formula.bv (Expr.getFormula e)))

val freeImpl    = binary (fn (_, e1, e2) => Bool (Formula.free  (Expr.getString e1) (Expr.getFormula e2)))
val boundImpl   = binary (fn (_, e1, e2) => Bool (Formula.bound (Expr.getString e1) (Expr.getFormula e2)))
val occurImpl   = binary (fn (_, e1, e2) => Bool (Formula.occur (Expr.getString e1) (Expr.getFormula e2)))
val kindofImpl  = unary (fn (_, e) => Symbol (Formula.kind (Expr.getFormula e)))
val boundofImpl = unary (fn (_, e) => String (Formula.boundof (Expr.getFormula e)))

val formulaImpl = unary (fn (_, e) => Formula (Expr.getTheorem e))
val theoryImpl  = unary (fn (_, e) => String (Expr.getTheory e))

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

val interchangeImpl = ternary
  (fn (_, e1, e2, e3) =>
  let
    val x = Expr.getString  e1
    val y = Expr.getString  e2
    val t = Expr.getFormula e3
  in
    Formula (Formula.interchange x y t)
  end)

val substImpl = binary (fn (_, e1, e2) => Formula (Formula.subst (Dict.map Expr.getFormula (Expr.getDict e1)) (Expr.getFormula e2)))
val unifyImpl = binary (fn (_, e1, e2) => Dict (Dict.map Formula (Formula.unify (Dict.empty ()) (Expr.getFormula e1) (Expr.getFormula e2))))

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

val builtin =
[("lambda",         lambda),
 ("\206\187",       lambda),
 ("apply",          eager applyImpl),
 ("macro",          macro),
 ("define",         special define),
 ("list",           eager (const List)),
 ("mapcar",         eager mapcarImpl),
 ("dolist",         eager dolistImpl),
 ("foldl",          eager foldlImpl),
 ("foldr",          eager foldrImpl),
 ("quote",          special (unary (fn (_, e) => e))),
 ("symbol",         eager symbolImpl),
 ("eval",           eager evalImpl),
 ("=",              eager equal),
 ("nil",            Expr.eps),
 ("typeof",         eager typeofImpl),
 (* Input/output *)
 ("print",          eager printImpl),
 ("newline",        eager newlineImpl),
 ("read",           eager readImpl),
 ("fail",           eager fail),
 (* Lists manipulation *)
 ("cons",           eager consImpl),
 ("car",            eager car),
 ("cdr",            eager cdr),
 ("length",         eager lengthImpl),
 ("nth",            eager nth),
 (* Dict/Set *)
 ("set",            eager setImpl),
 ("dict",           eager dictImpl),
 (* Arithmetic *)
 ("+",              addImpl),
 ("-",              subImpl),
 ("*",              mulImpl),
 ("/",              divImpl),
 ("true",           Bool true),
 ("false",          Bool false),
 (* Control flow *)
 ("if",             special ifImpl),
 ("and",            Lambda (fn (E, e) => Bool (andImpl E e))),
 ("or",             Lambda (fn (E, e) => Bool (orImpl E e))),
 ("not",            eager notImpl),
 ("ref",            eager refImpl),
 ("deref",          eager derefImpl),
 ("assign!",        eager assignImpl),
 ("progn",          special Expr.progn),
 ("loop",           special loopImpl),
 (* Formulae manipulation *)
 ("deftheory",      special deftheory),
 ("var",            eager var),
 ("app",            eager app),
 ("binder",         eager binder),
 ("kindof",         eager kindofImpl),
 ("boundof",        eager boundofImpl),
 ("subst",          eager substImpl),
 ("interchange",    eager interchangeImpl),
 ("fv",             eager fvImpl),
 ("bv",             eager bvImpl),
 ("fv?",            eager freeImpl),
 ("bv?",            eager boundImpl),
 ("occur?",         eager occurImpl),
 ("formula",        eager formulaImpl),
 ("theory",         eager theoryImpl),
 ("unify",          eager unifyImpl),
 ("case",           special caseImpl)]