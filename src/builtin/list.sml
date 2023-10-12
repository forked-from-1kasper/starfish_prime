(* Lists manipulation *)

val car = unary
  (fn (_, List xs)   => List.hd xs
    | (_, Formula t) => String (Formula.funsym t)
    | (_, e)         => raise (TypeMismatch (e, ["list", "formula"])))

val cdr = unary
  (fn (_, List xs)               => List (List.tl xs)
    | (_, Formula (App (_, ts))) => List (List.map Formula ts)
    | (_, Formula _)             => raise (Failure "cdr")
    | (_, e)                     => raise (TypeMismatch (e, ["list", "formula"])))

val lengthImpl = unary
  (fn (_, List xs)  => Int (List.length xs)
    | (_, String x) => Int (String.size x)
    | (_, e)        => raise (TypeMismatch (e, ["list", "string"])))

val consImpl   = binary  (fn (_, e1, e2) => List (e1 :: Expr.getList e2))
val nth        = binary  (fn (_, e1, e2) => List.nth (Expr.getList e2, Expr.getInt e1))
val mapcarImpl = binary  (fn (E, e1, e2) => List (List.map (getUnary E e1) (Expr.getList e2)))
val dolistImpl = binary  (fn (E, e1, e2) => (List.app (ignore o getUnary E e1) (Expr.getList e2); Expr.eps))

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
  | es           => raise (InvalidArity ([2, 3], List.length es))

  fun foldrImpl E = fn
    [e1, e2]     => foldr0 (getBinary E e1) (Expr.getList e2)
  | [e1, e2, e3] => List.foldr (getBinary E e1) e2 (Expr.getList e3)
  | es           => raise (InvalidArity ([2, 3], List.length es))
end

val forallImpl = binary (fn (E, e1, e2) => Bool (List.all    (Expr.getBool o getUnary E e1) (Expr.getList e2)))
val existsImpl = binary (fn (E, e1, e2) => Bool (List.exists (Expr.getBool o getUnary E e1) (Expr.getList e2)))

val LinkedList =
[("list",   eager (const List)),
 ("cons",   eager consImpl),
 ("car",    eager car),
 ("cdr",    eager cdr),
 ("length", eager lengthImpl),
 ("nth",    eager nth),
 ("mapcar", eager mapcarImpl),
 ("dolist", eager dolistImpl),
 ("foldl",  eager foldlImpl),
 ("foldr",  eager foldrImpl),
 ("forall", eager forallImpl),
 ("exists", eager existsImpl)]