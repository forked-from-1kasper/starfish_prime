datatype assemblage =
  Fv       of string
| Bv       of int
| Forme    of string * assemblage list
| Quanteur of string * assemblage

exception Failure of string
exception Unify   of assemblage * assemblage

structure Assemblage =
struct
  type t = assemblage

  val kind = fn
    Forme _    => "forme"
  | Quanteur _ => "quanteur"
  | Fv _       => "fv"
  | Bv _       => "bv"

  val rec show = fn
    Fv x            => x
  | Bv x            => Int.toString x
  | Forme (x, xs)   => x ^ "(" ^ String.concatWith ", " (List.map show xs) ^ ")"
  | Quanteur (x, t) => x ^ " " ^ show t

  fun equal e1 e2 = case (e1, e2) of
    (Fv x,             Fv y)             => x = y
  | (Bv i,             Bv j)             => i = j
  | (Forme (x, xs),    Forme (y, ys))    => x = y andalso ListPair.allEq (fn (e1, e2) => equal e1 e2) (xs, ys)
  | (Quanteur (i, t1), Quanteur (j, t2)) => i = j andalso equal t1 t2
  | (_,                _)                => false

  (* As bounded and free variables are clearly distinguished in the syntax
     (just like in Bourbaki’s “assemblages”), there is no need to worry
     about potential name collision. *)
  fun subst vs = fn
    Forme (f, ts)   => Forme (f, List.map (subst vs) ts)
  | Quanteur (b, t) => Quanteur (b, subst vs t)
  | Bv y            => Bv y
  | Fv x            => Option.getOpt (Dict.get x vs, Fv x)

  (* Turns (captures) free variable into bound. *)
  fun capture x =
  let
    fun loop k = fn
      Fv y            => if x = y then Bv k else Fv y
    | Bv n            => Bv n
    | Quanteur (b, t) => Quanteur (b, loop (k + 1) t)
    | Forme (t, ts)   => Forme (t, List.map (loop k) ts)
  in
    loop 1
  end

  (* Turns (frees) bound variable into free. *)
  fun free e =
  let
    fun loop k = fn
      Fv x            => Fv x
    | Bv k'           => if k = k' then e else Bv k'
    | Quanteur (b, t) => Quanteur (b, loop (k + 1) t)
    | Forme (t, ts)   => Forme (t, List.map (loop k) ts)
  in
    loop 1
  end

  fun bind b x t = Quanteur (b, capture x t)

  fun unbind e = fn
    Quanteur (_, t) => free e t
  | _               => raise (Failure "unbind")

  (* Helpful functions *)

  val funsym = fn
    Forme (x, _)    => x
  | Quanteur (x, _) => x
  | _               => raise (Failure "funsym")

  val arity = fn
    Forme (_, xs) => List.length xs
  | _             => raise (Failure "arity")

  (* Equivalent to (bind b (Var ω) ∘ unbind (Var ω))(τ), where ω ∉ Fv(τ) *)
  fun rebind b = fn
    Quanteur (_, t) => Quanteur (b, t)
  | _               => raise (Failure "rebind")

  fun occur x = fn
    Fv y            => x = y
  | Bv _            => false
  | Quanteur (_, t) => occur x t
  | Forme (_, ts)   => List.exists (occur x) ts

  val fv =
  let
    fun loop fvs = fn
      Fv x            => Bag.add x fvs
    | Bv _            => fvs
    | Quanteur (_, t) => loop fvs t
    | Forme (_, ts)   => List.foldl (fn (e, b) => loop b e) fvs ts
  in
    loop (Bag.empty ())
  end

  val formes =
  let
      fun loop fs = fn
      Fv _            => fs
    | Bv _            => fs
    | Quanteur (_, t) => loop fs t
    | Forme (f, ts)   => List.foldl (fn (e, b) => loop b e) (Bag.add f fs) ts
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
  | (Forme (f1, ts1), Forme (f2, ts2)) =>
    if f1 = f2 then ListPair.foldlEq (fn (t1, t2, ss) => unify ss t1 t2) ss (ts1, ts2)
    else raise (Unify (t1, t2))
  (* TODO: give some satisfactory semantics of unification against quantifier *)
  | (Quanteur (_, _), _) => raise (Failure "unify")
  | (_, _) => raise (Unify (t1, t2))
end