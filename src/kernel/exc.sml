exception TooManyParams of expr list
exception TooFewParams  of expr list

structure Exception =
struct
  fun quote x = "\226\128\156" ^ x ^ "\226\128\157"

  val show = fn
    NameError x              => "Unknown variable " ^ quote x ^ "."
  | AlreadyDeclared x        => quote x ^ " is already declared."
  | TypeMismatch (e, ts)     => quote (Expr.show e) ^ " expected to be " ^ String.concatWith " or " (List.map quote ts) ^ ", but it is " ^ quote (Expr.typeof e) ^ "."
  | InvalidArity (zs, z0)    => "Expected " ^ String.concatWith " or " (List.map Int.toString zs) ^ " parameters but " ^ Int.toString z0 ^ " were given."
  | TooManyParams xs         => "Too many (" ^ Int.toString (List.length xs) ^ ") parameters were given."
  | TooFewParams xs          => "Too few (" ^ Int.toString (List.length xs) ^ ") parameters were given."
  | Unify (t1, t2)           => "Unable to unify " ^ quote (Formula.show t1) ^ " with " ^ quote (Formula.show t2) ^ "."
  | InvalidEscape x          => "Invalid escape sequence in " ^ quote x ^ "."
  | MismatchedBracket        => "Mismatched bracket."
  | EOF                      => "Unexpected EOF."
  | NoExpression             => "No expression."
  | Failure err              => "Error: " ^ err
  | ex                       => "Uncaught exception:\n  " ^ exnMessage ex
end