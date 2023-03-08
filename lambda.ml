open Prelude
open Expr

let nulary fn ctx = function
  | [] -> fn ctx
  | xs -> raise (InvalidArity (0, List.length xs))

let unary fn ctx = function
  | [x] -> fn ctx x
  | xs  -> raise (InvalidArity (1, List.length xs))

let binary fn ctx = function
  | [x; y] -> fn ctx x y
  | xs     -> raise (InvalidArity (2, List.length xs))

let ternary fn ctx = function
  | [x; y; z] -> fn ctx x y z
  | xs        -> raise (InvalidArity (3, List.length xs))

let quaternary fn ctx = function
  | [x; y; z; w] -> fn ctx x y z w
  | xs           -> raise (InvalidArity (4, List.length xs))

let eager   fn = Lambda (fun ctx -> List.map (Expr.eval ctx) >> fn ctx)
let special fn = Lambda fn

let fail ctx xs = raise (Failure (String.concat " " (List.map Expr.show xs)))

let variadic x ys ctx = Env.upLocal ctx x (List ys)

let uniadic xs ys ctx =
  try List.fold_left2 Env.upLocal ctx (List.map Expr.getSymbol xs) ys
  with Invalid_argument _ -> raise (InvalidArity (List.length xs, List.length ys))

let lambdaImpl unpack ctx1 body =
  Lambda (fun ctx2 stxs ->
    let exprs = List.map (Expr.eval ctx2) stxs in
    Expr.progn (unpack exprs ctx1) body)

let macroImpl unpack ctx1 body =
  Lambda (fun ctx2 stxs -> Expr.eval ctx2 (Expr.progn (unpack stxs ctx1) body))

let lambda =
  Lambda (fun ctx -> function
    | List xs  :: ys -> lambdaImpl (uniadic xs) ctx ys
    | Symbol x :: ys -> lambdaImpl (variadic x) ctx ys
    | _              -> raise (Failure "(lambda (param₁ param₂ ... paramₙ) ...)"))

let macro =
  Lambda (fun ctx -> function
    | List xs  :: ys -> macroImpl (uniadic xs) ctx ys
    | Symbol x :: ys -> macroImpl (variadic x) ctx ys
    | _              -> raise (Failure "(macro (param₁ param₂ ... paramₙ) ...)"))