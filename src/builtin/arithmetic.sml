(* Arithmetic *)

fun xor a b = (a orelse b) andalso not (a andalso b)
fun abj a b = a andalso (not b)

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

val Arithmetic = [("+", addImpl), ("-", subImpl), ("*", mulImpl), ("/", divImpl)]