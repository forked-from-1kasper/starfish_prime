open Prelude

type formula =
  | Var    of string
  | App    of string * formula list
  | Binder of string * string * formula

module Formula =
struct
  type t = formula

  let funsym = function
    | App (x, _)       -> x
    | Binder (x, _, _) -> x
    | _                -> raise (Failure "funsym")

  let params = function
    | App (_, xs) -> xs
    | _           -> raise (Failure "params")

  let arity = function
    | App (_, xs) -> List.length xs
    | _           -> raise (Failure "arity")

  let rec show = function
    | Var x            -> x
    | App (x, xs)      -> x ^ "(" ^ String.concat ", " (List.map show xs) ^ ")"
    | Binder (x, y, t) -> x ^ " " ^ y ^ ", " ^ show t

  let rec equal e1 e2 = match e1, e2 with
    | Var x,             Var y             -> x = y
    | App (x, xs),       App (y, ys)       -> x = y && List.for_all2 equal xs ys
    | Binder (i, x, t1), Binder (j, y, t2) -> i = j && x = y && equal t1 t2
    | _,                 _                 -> false

  let rec bv x = function
    | Var _            -> false
    | Binder (_, y, t) -> x = y || bv x t
    | App (_, ts)      -> List.exists (bv x) ts

  let rec fv x = function
    | Var y            -> x = y
    | Binder (_, y, t) -> x <> y && fv x t
    | App (_, ts)      -> List.for_all (fv x) ts

  let rec occur x = function
    | Var y            -> x = y
    | Binder (_, y, t) -> x = y || occur x t
    | App (_, ts)      -> List.exists (occur x) ts

  let rec subst x e = function
    | Var y            -> if x = y then e else Var y
    | App (y, ts)      -> App (y, List.map (subst x e) ts)
    | Binder (i, y, t) -> if x = y then Binder (i, y, t) else Binder (i, y, subst x e t)

  let rec free e = function
    | Var _            -> true
    | App (_, ts)      -> List.for_all (free e) ts
    | Binder (_, y, t) -> not (fv y e) && free e t
end