open Prelude
open Formula

type expr =
  | Lambda  of clos
  | List    of expr list
  | Symbol  of string
  | String  of string
  | Float   of float
  | Int     of int
  | Bool    of bool
  | Ref     of expr ref
  | Formula of formula
  | Theorem of string * formula
and ctx  = expr Dict.t
and env  = { local : ctx; global : ctx ref }
and clos = env -> expr list -> expr

exception NameError       of string
exception AlreadyDeclared of string
exception TypeMismatch    of expr * string * string
exception InvalidArity    of int * int
exception TooManyParams   of expr list
exception TooFewParams    of expr list

module Env =
struct
  type t = env

  let init ds = {local = Dict.empty; global = ref ds}

  let get env k =
    match Dict.find_opt k env.local with
    | Some v -> v
    | None   ->
      match Dict.find_opt k !(env.global) with
      | Some v -> v
      | None   -> raise (NameError k)

  let upLocal env k v = { env with local = Dict.add k v env.local }

  let upGlobal env k v = env.global :=
    Dict.update k (function
      | Some _ -> raise (AlreadyDeclared k)
      | None   -> Some v) !(env.global)
end

module Expr =
struct
  type t = expr

  let rec show = function
    | Lambda _       -> "#<CLOSURE>"
    | Symbol x       -> x
    | List vs        -> "(" ^ String.concat " " (List.map show vs) ^ ")"
    | String x       -> "\"" ^ String.escaped x ^ "\""
    | Float x        -> string_of_float x
    | Int x          -> string_of_int x
    | Bool true      -> "true"
    | Bool false     -> "false"
    | Ref _          -> "#<REFERENCE>"
    | Formula t      -> "#<FORMULA " ^ Formula.show t ^ ">"
    | Theorem (x, t) -> "#<THEOREM \"" ^ x ^ "\" " ^ Formula.show t ^ ">"

  let falsehood = Bool false
  let truth     = Bool true
  let eps       = List []

  let typeof = function
    | Lambda _   -> "closure"
    | Symbol _   -> "symbol"
    | List _     -> "list"
    | String _   -> "string"
    | Float _    -> "float"
    | Int _      -> "int"
    | Bool _     -> "bool"
    | Ref _      -> "ref"
    | Formula _  -> "formula"
    | Theorem _  -> "theorem"

  let string x = String x
  let symbol x = Symbol x
  let list xs  = List xs

  let getList    = function List xs        -> xs | e -> raise (TypeMismatch (e, "list", typeof e))
  let getString  = function String x       -> x  | e -> raise (TypeMismatch (e, "string", typeof e))
  let getSymbol  = function Symbol x       -> x  | e -> raise (TypeMismatch (e, "symbol", typeof e))
  let getLam     = function Lambda fn      -> fn | e -> raise (TypeMismatch (e, "closure", typeof e))
  let getRef     = function Ref r          -> r  | e -> raise (TypeMismatch (e, "reference", typeof e))
  let getTheorem = function Theorem (_, t) -> t  | e -> raise (TypeMismatch (e, "theorem", typeof e))
  let getTheory  = function Theorem (x, _) -> x  | e -> raise (TypeMismatch (e, "theorem", typeof e))
  let getFormula = function Formula t      -> t  | e -> raise (TypeMismatch (e, "formula", typeof e))
  let getBool    = function Bool b         -> b  | e -> raise (TypeMismatch (e, "bool", typeof e))
  let getInt     = function Int z          -> z  | e -> raise (TypeMismatch (e, "int", typeof e))
  let getFloat   = function Float r        -> r  | e -> raise (TypeMismatch (e, "float", typeof e))

  let rec equal e1 e2 = match e1, e2 with
    | Lambda f1,        Lambda f2        -> f1 == f2
    | Formula t1,       Formula t2       -> Formula.equal t1 t2
    | Theorem (x1, t1), Theorem (x2, t2) -> x1 = x2 && Formula.equal t1 t2
    | List l1,          List l2          -> equals l1 l2
    | Symbol x1,        Symbol x2        -> x1 = x2
    | String s1,        String s2        -> s1 = s2
    | Int z1,           Int z2           -> z1 = z2
    | Float r1,         Float r2         -> r1 = r2
    | Bool b1,          Bool b2          -> b1 = b2
    | Ref r1,           Ref r2           -> r1 == r2
    | _,                _                -> false

  and equals l1 l2 = match l1, l2 with
    | x :: xs, y :: ys -> equal x y && equals xs ys
    | _ :: _,    []    -> false
    |   [],    _ :: _  -> false
    |   [],      []    -> true

  let rec eval ctx e = try match e with
    | List (t :: ts) -> getLam (eval ctx t) ctx ts
    | List []        -> eps
    | Symbol x       -> Env.get ctx x
    | t              -> t
  with ex -> Printf.printf "↴\n  %s\n" (show e); raise ex

  let rec progn ctx = function
    | []      -> eps
    | [e]     -> eval ctx e
    | e :: es -> (ignore (eval ctx e); progn ctx es)
end