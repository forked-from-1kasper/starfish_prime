open Prelude
open Formula
open Reader
open Lambda
open Expr

let equal    = binary (fun ctx e1 e2 -> Bool (Expr.equal e1 e2))
let define   = binary (fun ctx e1 e2 -> Env.upGlobal ctx (Expr.getSymbol e1) (Expr.eval ctx e2); Expr.eps)
let evalImpl = unary Expr.eval

let plus t1 t2 = match t1, t2 with
  | List [],  v        -> v
  | v,        List []  -> v
  | Int x,    Int y    -> Int (x + y)
  | Int x,    Float y  -> Float (float_of_int x +. y)
  | Float x,  Int y    -> Float (x +. float_of_int y)
  | Float x,  Float y  -> Float (x +. y)
  | Bool b1,  Bool b2  -> Bool (xor b1 b2)
  | String x, String y -> String (x ^ y)
  | List xs,  List ys  -> List (List.append xs ys)
  | _,        _        -> raise (TypeMismatch (t1, Expr.typeof t2, Expr.typeof t1))

let mult t1 t2 = match t1, t2 with
  | List [],  v        -> v
  | v,        List []  -> v
  | Int x,    Int y    -> Int (x * y)
  | Int x,    Float y  -> Float (float_of_int x *. y)
  | Float x,  Int y    -> Float (x *. float_of_int y)
  | Float x,  Float y  -> Float (x *. y)
  | Bool b1,  Bool b2  -> Bool (b1 && b2)
  | _,        _        -> raise (TypeMismatch (t1, Expr.typeof t2, Expr.typeof t1))

let addImpl = eager (const (List.fold_left plus Expr.eps))
let mulImpl = eager (const (List.fold_left mult Expr.eps))

let symbol = unary (fun ctx e -> Symbol (Expr.getString e))

let rec ifImpl ctx = function
  | b :: e :: es ->
  begin match Expr.eval ctx b with
    | Bool true  -> Expr.eval ctx e
    | Bool false -> ifImpl ctx es
    | _          -> raise (Failure "if?")
  end
  | e :: []      -> Expr.eval ctx e
  | []           -> Expr.eps

let loopImpl ctx body =
  while Expr.progn ctx body <> Expr.falsehood do
    ()
  done; Expr.eps

let rec andImpl ctx = function
  |   []    -> true
  | x :: xs -> if Expr.getBool (Expr.eval ctx x) then andImpl ctx xs else false

let rec orImpl ctx = function
  |   []    -> false
  | x :: xs -> if Expr.getBool (Expr.eval ctx x) then true else orImpl ctx xs

let notImpl = unary (fun ctx e -> Bool (not (Expr.getBool e)))

let car    = unary  (fun ctx e -> List.hd (Expr.getList e))
let cdr    = unary  (fun ctx e -> List (List.tl (Expr.getList e)))
let cons   = binary (fun ctx e1 e2 -> List (e1 :: Expr.getList e2))
let length = unary  (fun ctx e -> Int (List.length (Expr.getList e)))
let nth    = binary (fun ctx e1 e2 -> List.nth (Expr.getList e2) (Expr.getInt e1))

let refImpl    = unary  (fun ctx e -> Ref (ref e))
let derefImpl  = unary  (fun ctx e -> !(Expr.getRef e))
let assignImpl = binary (fun ctx e1 e2 -> Expr.getRef e1 := e2; Expr.eps)

let printExpr = print_string << function
  | Symbol x -> "(symbol \"" ^ x ^ "\")"
  | String s -> s
  | e        -> Expr.show e

let printImpl   = fun ctx exprs -> List.iter printExpr exprs; flush stdout; Expr.eps
let newlineImpl = nulary (fun ctx -> print_newline (); flush stdout; Expr.eps)

let typeofImpl = unary (fun ctx e -> Symbol (Expr.typeof e))

let readImpl ctx stxs =
  let tokenizer = match stxs with
    | []  -> Tokenizer.ofChan stdin
    | [e] -> Tokenizer.ofString (Expr.getString e)
    | es  -> raise (TooManyParams es) in

  match read tokenizer with
  | Some retval -> retval
  | None        -> raise NoExpression

let formula t = Formula t

let postulate = (unary >> eager) (fun ctx e -> Theorem (Expr.getFormula e))

let withPostulate ctx es =
  let ctx' = Env.upLocal ctx "postulate" postulate in
  List.iter (Expr.eval ctx' >> ignore) es; Expr.eps

let var = unary (fun ctx e -> Formula (Var (Expr.getString e)))

let app ctx = function
  |   []    -> raise (TooFewParams [])
  | x :: xs -> Formula (App (Expr.getString x, List.map Expr.getFormula xs))

let binder = ternary (fun ctx e1 e2 e3 -> Formula (Binder (Expr.getString e1, Expr.getString e2, Expr.getFormula e3)))

let formulaImpl = unary (fun ctx e -> Formula (Expr.getTheorem e))
let funsymImpl  = unary (fun ctx e -> String (Formula.funsym (Expr.getFormula e)))
let paramsImpl  = unary (fun ctx e -> List (List.map formula (Formula.params (Expr.getFormula e))))
let arityImpl   = unary (fun ctx e -> Int (Formula.arity (Expr.getFormula e)))

let subst = ternary (fun ctx e1 e2 e3 ->
  let x = Expr.getString  e1 in
  let e = Expr.getFormula e2 in
  let t = Expr.getFormula e3 in

  if Formula.free e t then Formula (Formula.subst x e t)
  else raise (InvalidSubst (x, e, t)))

let builtin =
  [("lambda",         lambda);
   ("λ",              lambda);
   ("macro",          macro);
   ("define",         special define);
   ("list",           eager (fun ctx stxs -> List stxs));
   ("quote",          special (fun ctx stxs -> List stxs));
   ("symbol",         eager symbol);
   ("eval",           eager evalImpl);
   ("=",              eager equal);
   ("nil",            Expr.eps);
   ("typeof",         eager typeofImpl);
   (* Input/output *)
   ("print",          eager printImpl);
   ("newline",        eager newlineImpl);
   ("read",           eager readImpl);
   ("fail",           eager fail);
   (* Lists manipulation *)
   ("cons",           eager cons);
   ("car",            eager car);
   ("cdr",            eager cdr);
   ("length",         eager length);
   ("nth",            eager nth);
   (* Arithmetics *)
   ("+",              addImpl);
   ("*",              mulImpl);
   ("true",           Bool true);
   ("false",          Bool false);
   (* Control flow *)
   ("if",             special ifImpl);
   ("and",            special (fun ctx es -> Bool (andImpl ctx es)));
   ("or",             special (fun ctx es -> Bool (orImpl ctx es)));
   ("not",            eager notImpl);
   ("ref",            eager refImpl);
   ("deref",          eager derefImpl);
   ("assign!",        eager assignImpl);
   ("progn",          special Expr.progn);
   ("loop",           special loopImpl);
   (* Formulae manipulation *)
   ("with-postulate", special withPostulate);
   ("var",            eager var);
   ("app",            eager app);
   ("binder",         eager binder);
   ("subst",          eager subst);
   ("formula",        eager formulaImpl);
   ("formula/funsym", eager funsymImpl);
   ("formula/arity",  eager arityImpl);
   ("formula/params", eager paramsImpl)]
  |> List.to_seq |> Ctx.of_seq