(* List, Dict, Set *)

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

fun set bag = List (List.map String (Bag.fold (fn x => fn xs => x :: xs) bag []))

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

fun addImpl E = fn
  [e1, e2]     => Set (Bag.add (Expr.getString e1) (Expr.getSet e2))
| [e1, e2, e3] => Dict (Dict.add (Expr.getString e1) e2 (Expr.getDict e3))
| es           => raise (InvalidArity ([2, 3], List.length es))

val removeImpl = binary (fn (_, k, Dict t) => Dict (Dict.remove (Expr.getString k) t)
                          | (_, k, Set t)  => Set (Bag.remove (Expr.getString k) t)
                          | (_, _, e)      => raise (TypeMismatch (e, ["dict", "set"])))

val memImpl = binary (fn (_, k, Dict t) => Bool (Dict.mem (Expr.getString k) t)
                       | (_, k, Set t)  => Bool (Bag.mem (Expr.getString k) t)
                       | (_, _, e)      => raise (TypeMismatch (e, ["dict", "set"])))

val getImpl = binary (fn (_, e1, e2) => Option.getOpt (Dict.get (Expr.getString e1) (Expr.getDict e2), Expr.eps))

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

val Data =
[("list",   eager (const List)),
 ("set",    eager setImpl),
 ("dict",   eager dictImpl),
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
 ("exists", eager existsImpl),
 ("add",    eager addImpl),
 ("get",    eager getImpl),
 ("mem",    eager memImpl),
 ("remove", eager removeImpl)]