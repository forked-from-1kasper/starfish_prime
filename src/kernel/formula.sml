fun exchange x y = fn z => if z = x then y else if z = y then x else z

datatype formula =
  Var    of string
| App    of string * formula list
| Binder of string * string * formula

exception Failure      of string
exception InvalidSubst of string * formula * formula

structure Formula =
struct
  type t = formula

  val funsym = fn
    App (x, _)       => x
  | Binder (x, _, _) => x
  | _                => raise (Failure "funsym")

  val params = fn
    App (_, xs) => xs
  | _           => raise (Failure "params")

  val arity = fn
    App (_, xs) => List.length xs
  | _           => raise (Failure "arity")

  val rec show = fn
    Var x            => x
  | App (x, xs)      => x ^ "(" ^ String.concatWith ", " (List.map show xs) ^ ")"
  | Binder (x, y, t) => x ^ " " ^ y ^ ", " ^ show t

  fun equal e1 e2 = case (e1, e2) of
    (Var x,             Var y)             => x = y
  | (App (x, xs),       App (y, ys))       => x = y andalso ListPair.allEq (fn (e1, e2) => equal e1 e2) (xs, ys)
  | (Binder (i, x, t1), Binder (j, y, t2)) => i = j andalso x = y andalso equal t1 t2
  | (_,                 _)                 => false

  fun bound x = fn
    Var _            => false
  | Binder (_, y, t) => x = y orelse bound x t
  | App (_, ts)      => List.exists (bound x) ts

  fun free x = fn
    Var y            => x = y
  | Binder (_, y, t) => x <> y andalso free x t
  | App (_, ts)      => List.exists (free x) ts

  fun occur x = fn
    Var y            => x = y
  | Binder (_, y, t) => x = y orelse occur x t
  | App (_, ts)      => List.exists (occur x) ts

  val bv =
  let
    fun loop bag = fn
      Var _            => bag
    | Binder (_, x, t) => loop (Bag.add x bag) t
    | App (_, ts)      => List.foldl (fn (e, b) => loop b e) bag ts
  in
    loop (Bag.empty ())
  end

  val fv =
  let
    fun loop bvs fvs = fn
      Var x            => if Bag.mem x bvs then fvs else Bag.add x fvs
    | Binder (_, x, t) => loop (Bag.add x bvs) fvs t
    | App (_, ts)      => List.foldl (fn (e, b) => loop bvs b e) fvs ts
  in
    loop (Bag.empty ()) (Bag.empty ())
  end

  fun subst vs e =
  let
    fun loop ss bvs = fn
      App (f, ts)      => App (f, List.map (loop ss bvs) ts)
    | Binder (b, x, t) => Binder (b, x, loop (Dict.remove x ss) (Bag.add x bvs) t)
    | Var x            =>
      case Dict.get x ss of
        SOME (t, fvs) => if Bag.disjoint fvs bvs then t
                         else raise (InvalidSubst (x, t, e))
      | NONE          => Var x
  in
    loop (Dict.map (fn t => (t, fv t)) vs) (Bag.empty ()) e
  end

  fun interchange x y = fn
    Var z            => Var (exchange x y z)
  | App (f, ts)      => App (f, List.map (interchange x y) ts)
  | Binder (b, z, t) => Binder (b, exchange x y z, interchange x y t)

  fun unify ss t1 t2 =
    case (t1, t2) of
    (Var x, t) =>
    (case Dict.get x ss of
      SOME e => if equal t e then ss else raise (Failure "unify")
    | NONE   => Dict.add x t ss)
  | (App (f1, ts1), App (f2, ts2)) =>
    if f1 = f2 then ListPair.foldlEq (fn (t1, t2, ss) => unify ss t1 t2) ss (ts1, ts2)
    else raise (Failure "unify")
  | (Binder (b1, x, e1), Binder (b2, y, e2)) =>
    if b1 = b2 then (case Dict.get x ss of
        SOME _ => raise (Failure "unify")
      | NONE   => unify (Dict.add x (Var y) ss) e1 e2)
    else raise (Failure "unify")
  | (_, _) => raise (Failure "unify")
end