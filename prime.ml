open Prelude
open Formula
open Builtin
open Reader
open Expr

let quote xs = "“" ^ xs ^ "”"

let showExc = function
  | NameError x              -> Printf.sprintf "Unknown variable “%s”." x
  | AlreadyDeclared x        -> Printf.sprintf "“%s” is already declared." x
  | TypeMismatch (e, ts)     -> Printf.sprintf "“%s” expected to be %s, but it is “%s”." (Expr.show e) (String.concat " or " (List.map quote ts)) (Expr.typeof e)
  | InvalidArity (z1, z2)    -> Printf.sprintf "Expected %d parameters but %d were given." z1 z2
  | InvalidSubst (x, t, e)   -> Printf.sprintf "Cannot substitute “%s” with “%s” in\n  %s" x (Formula.show t) (Formula.show e)
  | TooManyParams xs         -> Printf.sprintf "Too many (%d) parameters were given." (List.length xs)
  | TooFewParams xs          -> Printf.sprintf "Too few (%d) parameters were given." (List.length xs)
  | MismatchedBracket        -> "Mismatched bracket."
  | UnexpectedEOF            -> "Unexpected EOF."
  | NoExpression             -> "No expression."
  | Error err                -> Printf.sprintf "Error:\n  %s" err
  | ex                       -> "Uncaught exception:\n  " ^ Printexc.to_string ex

let rec load ctx tokenizer = match read tokenizer with
  | Some expr -> ignore (Expr.eval ctx expr); load ctx tokenizer
  | None      -> ()

let loadFile ctx filename =
  let chan = open_in_bin filename in
  try load ctx (Tokenizer.ofChan chan)
  with ex -> print_endline (showExc ex);
  close_in chan

let repl ctx =
  let tokenizer = Tokenizer.ofChan stdin in

  while true do
    print_string "LISP> "; flush stdout;
    try match read tokenizer with
      | Some e -> print_endline (Expr.show (Expr.eval ctx e))
      | None   -> print_newline ()
    with ex -> print_endline (showExc ex)
  done

let () =
  let files = List.tl (Array.to_list Sys.argv) in
  let ctx = Env.init builtin in List.iter (loadFile ctx) files; repl ctx