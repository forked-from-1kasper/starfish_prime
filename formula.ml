open Prelude

type formula =
  | Var    of string
  | App    of string * formula list
  | Binder of string * string * formula

exception InvalidSubst of string * formula * formula

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

  let rec bound x = function
    | Var _            -> false
    | Binder (_, y, t) -> x = y || bound x t
    | App (_, ts)      -> List.exists (bound x) ts

  let rec free x = function
    | Var y            -> x = y
    | Binder (_, y, t) -> x <> y && free x t
    | App (_, ts)      -> List.exists (free x) ts

  let rec occur x = function
    | Var y            -> x = y
    | Binder (_, y, t) -> x = y || occur x t
    | App (_, ts)      -> List.exists (occur x) ts

  let rec bv =
    let rec loop bag = function
      | Var _            -> bag
      | Binder (_, x, t) -> loop (Bag.add x bag) t
      | App (_, ts)      -> List.fold_left loop bag ts
    in loop Bag.empty

  let rec fv =
    let rec loop bvs fvs = function
      | Var x            -> if Bag.mem x bvs then fvs else Bag.add x fvs
      | Binder (_, x, t) -> loop (Bag.add x bvs) fvs t
      | App (_, ts)      -> List.fold_left (loop bvs) fvs ts
    in loop Bag.empty Bag.empty

  let subst vs e =
    let rec loop ss bvs = function
      | App (f, ts)      -> App (f, List.map (loop ss bvs) ts)
      | Binder (b, x, t) -> Binder (b, x, loop (Dict.remove x ss) (Bag.add x bvs) t)
      | Var x            ->
      begin match Dict.find_opt x ss with
        | Some (t, fvs) -> if Bag.disjoint fvs bvs then t
                           else raise (InvalidSubst (x, t, e))
        | None          -> Var x
      end
    in loop (Dict.map (fun t -> (t, fv t)) vs) Bag.empty e

  let rec interchange x y = function
    | Var z            -> Var (exchange x y z)
    | App (f, ts)      -> App (f, List.map (interchange x y) ts)
    | Binder (b, z, t) -> Binder (b, exchange x y z, interchange x y t)
end