type 'a environment = {loc : 'a dict, global : 'a dict ref}

type ('a, 'b) eff = 'a environment * 'b

datatype expr =
  Lambda     of closure
| List       of expr list
| Symbol     of string
| Quote      of expr (* Quote E ~ List [Lambda (fn (_, _) => E)] *)
| String     of string
| Real       of real
| Int        of int
| Bool       of bool
| Ref        of expr ref
| Dict       of expr dict
| Set        of bag
| Assemblage of assemblage
| Theorem    of string * assemblage
withtype closure = (expr, expr list) eff -> (expr, expr) eff

exception NameError       of string
exception AlreadyDeclared of string
exception TypeMismatch    of expr * string list
exception InvalidArity    of int list * int

structure Environment =
struct
  type 'a t = 'a environment

  fun init ds = {loc = Dict.empty (), global = ref ds}

  fun get {loc, global} k =
    case Dict.get k loc of
      SOME v => v
    | NONE   =>
      case Dict.get k (!global) of
        SOME v => v
      | NONE   => raise (NameError k)

  fun upLocal {loc, global} k v = {loc = Dict.add k v loc, global = global}

  fun upGlobal {loc, global} k v =
    global := Dict.update k
      (fn SOME _ => raise (AlreadyDeclared k)
        | NONE   => SOME v) (!global)
end

structure Expr =
struct
  type t = expr

  fun quote x = "\"" ^ x ^ "\""

  val rec show = fn
    Lambda _       => "#<CLOSURE>"
  | Symbol x       => x
  | List vs        => "(" ^ String.concatWith " " (List.map show vs) ^ ")"
  | Quote e        => "'" ^ show e
  | String x       => quote (String.toCString x)
  | Real x         => Real.toString x
  | Int x          => Int.toString x
  | Bool true      => "true"
  | Bool false     => "false"
  | Ref _          => "#<REFERENCE>"
  | Dict t         => "#<DICT" ^ Dict.fold (fn k => fn v => fn xs => xs ^ " " ^ quote (String.toCString k) ^ " " ^ show v) t "" ^ ">"
  | Set t          => "#<SET" ^ Bag.fold (fn x => fn xs => xs ^ " " ^ quote (String.toCString x)) t "" ^ ">"
  | Assemblage t   => "#<ASSEMBLAGE " ^ Assemblage.show t ^ ">"
  | Theorem (x, t) => "#<THEOREM \"" ^ x ^ "\" " ^ Assemblage.show t ^ ">"

  val falsehood = Bool false
  val truth     = Bool true
  val eps       = List []

  val typeof = fn
    Lambda _     => "closure"
  | Symbol _     => "symbol"
  | List _       => "list"
  | Quote _      => "quote"
  | String _     => "string"
  | Real _       => "real"
  | Int _        => "int"
  | Bool _       => "bool"
  | Ref _        => "ref"
  | Dict _       => "dict"
  | Set _        => "set"
  | Assemblage _ => "assemblage"
  | Theorem _    => "theorem"

  val getList       = fn List xs        => xs | e => raise (TypeMismatch (e, ["list"]))
  val getString     = fn String x       => x  | e => raise (TypeMismatch (e, ["string"]))
  val getSymbol     = fn Symbol x       => x  | e => raise (TypeMismatch (e, ["symbol"]))
  val getQuote      = fn Quote e        => e  | e => raise (TypeMismatch (e, ["quote"]))
  val getLam        = fn Lambda f       => f  | e => raise (TypeMismatch (e, ["closure"]))
  val getRef        = fn Ref r          => r  | e => raise (TypeMismatch (e, ["reference"]))
  val getTheorem    = fn Theorem (_, t) => t  | e => raise (TypeMismatch (e, ["theorem"]))
  val getTheory     = fn Theorem (x, _) => x  | e => raise (TypeMismatch (e, ["theorem"]))
  val getAssemblage = fn Assemblage t   => t  | e => raise (TypeMismatch (e, ["assemblage"]))
  val getBool       = fn Bool b         => b  | e => raise (TypeMismatch (e, ["bool"]))
  val getInt        = fn Int z          => z  | e => raise (TypeMismatch (e, ["int"]))
  val getFloat      = fn Real r         => r  | e => raise (TypeMismatch (e, ["real"]))
  val getDict       = fn Dict t         => t  | e => raise (TypeMismatch (e, ["dict"]))
  val getSet        = fn Set t          => t  | e => raise (TypeMismatch (e, ["set"]))

  fun equal e1 e2 = case (e1, e2) of
    (Assemblage t1,    Assemblage t2)    => Assemblage.equal t1 t2
  | (Theorem (x1, t1), Theorem (x2, t2)) => x1 = x2 andalso Assemblage.equal t1 t2
  | (Quote e1,         Quote e2)         => equal e1 e2
  | (List l1,          List l2)          => equals l1 l2
  | (Symbol x1,        Symbol x2)        => x1 = x2
  | (String s1,        String s2)        => s1 = s2
  | (Int z1,           Int z2)           => z1 = z2
  | (Real r1,          Real r2)          => Real.== (r1, r2)
  | (Bool b1,          Bool b2)          => b1 = b2
  | (Ref r1,           Ref r2)           => r1 = r2
  | (_,                _)                => false

  and equals l1 l2 = case (l1, l2) of
    (x :: xs, y :: ys) => equal x y andalso equals xs ys
  | (_ :: _,    [])    => false
  |   ([],    _ :: _)  => false
  |   ([],      [])    => true

  fun eval E e =
  let
    val loop = fn
      List (t :: ts) => getLam (#2 (eval E t)) (E, ts)
    | List []        => (E, eps)
    | Symbol x       => (E, Environment.get E x)
    | Quote e        => (E, e)
    | t              => (E, t)

    fun pong ex = (print ("\226\134\180\n  " ^ show e ^ "\n"); raise ex)
  in
    loop e handle ex => pong ex
  end

  fun progn E = fn
    []      => (E, eps)
  | [e]     => eval E e
  | e :: es => let val (E', _) = eval E e in progn E' es end

  fun ieval  E = #2 o eval  E
  fun iprogn E = #2 o progn E
end