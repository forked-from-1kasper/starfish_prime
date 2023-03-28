exception TooManyParams of expr list
exception TooFewParams  of expr list

structure Exception =
struct
  fun quote x = "\226\128\156" ^ x ^ "\226\128\157"

  val show = fn
    NameError x              => "Unknown variable " ^ quote x ^ "."
  | AlreadyDeclared x        => quote x ^ " is already declared."
  | TypeMismatch (e, ts)     => quote (Expr.show e) ^ " expected to be " ^ String.concatWith " or " (List.map quote ts) ^ ", but it is " ^ quote (Expr.typeof e) ^ "."
  | InvalidArity (z1, z2)    => "Expected " ^ Int.toString z1 ^ " parameters but " ^ Int.toString z2 ^ " were given."
  | InvalidSubst (x, t, e)   => "Cannot substitute " ^ quote x ^ " with " ^ quote (Formula.show t) ^ " in\n  " ^ Formula.show e
  | TooManyParams xs         => "Too many (" ^ Int.toString (List.length xs) ^ ") parameters were given."
  | TooFewParams xs          => "Too few (" ^ Int.toString (List.length xs) ^ ") parameters were given."
  | InvalidEscape x          => "Invalid escape sequence in " ^ quote x ^ "."
  | MismatchedBracket        => "Mismatched bracket."
  | UnexpectedEOF            => "Unexpected EOF."
  | NoExpression             => "No expression."
  | Failure err              => "Error: " ^ err
  | ex                       => "Uncaught exception:\n  " ^ exnMessage ex
end