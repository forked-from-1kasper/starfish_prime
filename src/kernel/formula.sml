fun exchange x y = fn z => if z = x then y else if z = y then x else z

datatype formula =
  Fv     of string
| Bv     of int
| App    of string * formula list
| Binder of string * formula

exception Failure of string
exception Unify   of formula * formula

structure Formula =
struct
  type t = formula

  val kind = fn
    App _    => "app"
  | Binder _ => "binder"
  | Fv _     => "fv"
  | Bv _     => "bv"

  val rec show = fn
    Fv x          => x
  | Bv x          => Int.toString x
  | App (x, xs)   => x ^ "(" ^ String.concatWith ", " (List.map show xs) ^ ")"
  | Binder (x, t) => x ^ " " ^ show t

  fun equal e1 e2 = case (e1, e2) of
    (Fv x,           Fv y)           => x = y
  | (Bv i,           Bv j)           => i = j
  | (App (x, xs),    App (y, ys))    => x = y andalso ListPair.allEq (fn (e1, e2) => equal e1 e2) (xs, ys)
  | (Binder (i, t1), Binder (j, t2)) => i = j andalso equal t1 t2
  | (_,              _)              => false

  (* As bounded and free variables are clearly distinguished in the syntax
     (just like in Bourbaki’s “assemblages”), there is no need to worry
     about potential name collision. *)
  fun subst vs = fn
    App (f, ts)   => App (f, List.map (subst vs) ts)
  | Binder (b, t) => Binder (b, subst vs t)
  | Bv y          => Bv y
  | Fv x          => Option.getOpt (Dict.get x vs, Fv x)

  (* Turns (captures) free variable into bound. *)
  fun capture x =
  let
    fun loop k = fn
      Fv y          => if x = y then Bv k else Fv y
    | Bv n          => Bv n
    | Binder (b, t) => Binder (b, loop (k + 1) t)
    | App (t, ts)   => App (t, List.map (loop k) ts)
  in
    loop 1
  end

  (* Turns (frees) bound variable into free. *)
  fun free e =
  let
    fun loop k = fn
      Fv x          => Fv x
    | Bv k'         => if k = k' then e else Bv k'
    | Binder (b, t) => Binder (b, loop (k + 1) t)
    | App (t, ts)   => App (t, List.map (loop k) ts)
  in
    loop 1
  end

  fun bind b x t = Binder (b, capture x t)

  fun unbind e = fn
    Binder (_, t) => free e t
  | _             => raise (Failure "unbind")

  (* Helpful functions *)

  val funsym = fn
    App (x, _)    => x
  | Binder (x, _) => x
  | _             => raise (Failure "funsym")

  val arity = fn
    App (_, xs) => List.length xs
  | _           => raise (Failure "arity")

  (* Equivalent to (bind b (Var ω) ∘ unbind (Var ω))(τ), where ω ∉ Fv(τ) *)
  fun rebind b = fn
    Binder (_, t) => Binder (b, t)
  | _             => raise (Failure "rebind")

  fun occur x = fn
    Fv y          => x = y
  | Bv _          => false
  | Binder (_, t) => occur x t
  | App (_, ts)   => List.exists (occur x) ts

  val fv =
  let
    fun loop fvs = fn
      Fv x          => Bag.add x fvs
    | Bv _          => fvs
    | Binder (_, t) => loop fvs t
    | App (_, ts)   => List.foldl (fn (e, b) => loop b e) fvs ts
  in
    loop (Bag.empty ())
  end

  fun unify ss t1 t2 =
  case (t1, t2) of
    (Fv x, t) =>
    (case Dict.get x ss of
      SOME e => if equal t e then ss else raise (Unify (t1, t2))
    | NONE   => Dict.add x t ss)
  | (Bv i, Bv j) => if i = j then ss else raise (Unify (t1, t2))
  | (App (f1, ts1), App (f2, ts2)) =>
    if f1 = f2 then ListPair.foldlEq (fn (t1, t2, ss) => unify ss t1 t2) ss (ts1, ts2)
    else raise (Unify (t1, t2))
  (* TODO: give some satisfactory semantics of unification against binder *)
  | (Binder (_, _), _) => raise (Failure "unify")
  | (_, _) => raise (Unify (t1, t2))
end