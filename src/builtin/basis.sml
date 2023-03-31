val equal    = binary (fn (_, e1, e2) => Bool (Expr.equal e1 e2))
val define   = binary (fn (E, e1, e2) => (Environment.upGlobal E (Expr.getSymbol e1) (Expr.eval E e2); e1))
val evalImpl = unary (fn (E, e) => Expr.eval E e)

val applyImpl = binary (fn (E, e1, e2) => Expr.getLam e1 (E, List.map Quote (Expr.getList e2)))

fun idfun x = x
fun const a b = a

fun xor a b = (a orelse b) andalso not (a andalso b)

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

  val mult = fn
    (List [],  v)        => v
  | (v,        List [])  => v
  | (Int x,    Int y)    => Int (x * y)
  | (Int x,    Real y)   => Real (Real.fromInt x * y)
  | (Real x,   Int y)    => Real (x * Real.fromInt y)
  | (Real x,   Real y)   => Real (x * y)
  | (Bool b1,  Bool b2)  => Bool (b1 andalso b2)
  | (t1,       t2)       => raise (TypeMismatch (t1, [Expr.typeof t2]))
in
  val addImpl = eager (const (List.foldl plus Expr.eps))
  val mulImpl = eager (const (List.foldl mult Expr.eps))
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
  (fn (_, List xs)   => List (List.tl xs)
    | (_, Formula t) => List (List.map Formula (Formula.params t))
    | (_, e)         => raise (TypeMismatch (e, ["list", "formula"])))

val consImpl   = binary (fn (ctx, e1, e2) => List (e1 :: Expr.getList e2))
val nth        = binary (fn (_, e1, e2) => List.nth (Expr.getList e2, Expr.getInt e1))
val lengthImpl = unary  (fn (ctx, e) => Int (List.length (Expr.getList e)))

val refImpl    = unary  (fn (_, e)      => Ref (ref e))
val derefImpl  = unary  (fn (_, e)      => !(Expr.getRef e))
val assignImpl = binary (fn (_, e1, e2) => (Expr.getRef e1 := e2; Expr.eps))

val printExpr = fn
  Symbol x => "(symbol \"" ^ x ^ "\")"
| String s => s
| e        => Expr.show e

fun fail ctx xs = raise (Failure (String.concatWith " " (List.map printExpr xs)))

val printImpl   = fn E => fn e => (List.app (print o printExpr) e; Expr.eps)
val newlineImpl = nulary (fn E => (print "\n"; Expr.eps))

val typeofImpl = unary (fn (_, e) => Symbol (Expr.typeof e))

fun readImpl ctx stxs =
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
val fvImpl = unary (fn (_, e) => set (Formula.fv (Expr.getFormula e)))
val bvImpl = unary (fn (_, e) => set (Formula.bv (Expr.getFormula e)))

val freeImpl  = binary (fn (_, e1, e2) => Bool (Formula.free  (Expr.getString e1) (Expr.getFormula e2)))
val boundImpl = binary (fn (_, e1, e2) => Bool (Formula.bound (Expr.getString e1) (Expr.getFormula e2)))
val occurImpl = binary (fn (_, e1, e2) => Bool (Formula.occur (Expr.getString e1) (Expr.getFormula e2)))

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

val interchangeImpl = ternary
  (fn (_, e1, e2, e3) =>
  let
    val x = Expr.getString  e1
    val y = Expr.getString  e2
    val t = Expr.getFormula e3
  in
    Formula (Formula.interchange x y t)
  end)

val substImpl =
let
  val getDict = (dict Expr.getString Expr.getFormula) o Expr.getList
in
  binary (fn (_, e1, e2) => Formula (Formula.subst (getDict e1) (Expr.getFormula e2)))
end

val unifyImpl = binary (fn (_, e1, e2) =>
  List (Dict.fold (fn k => fn v => fn vs => List [String k, Formula v] :: vs)
                  (Formula.unify (Dict.empty ()) (Expr.getFormula e1) (Expr.getFormula e2)) []))

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
 (* Arithmetics *)
 ("+",              addImpl),
 ("*",              mulImpl),
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