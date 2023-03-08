open Expr

type token =
  | Literal of string
  | Ident of string
  | Lparen
  | Rparen
  | Eof

exception MismatchedBracket
exception UnexpectedEOF
exception NoExpression

module Tokenizer =
struct
  type t = { get : unit -> char; mutable buffer : char list }

  let ofChan chan = { buffer = []; get = fun () -> input_char chan }
  let ofString s = let i = ref 0 in let length = String.length s in
    { buffer = []; get = fun () -> let j = !i in i := j + 1;
        if j < length then String.get s j else raise End_of_file }

  let push tokenizer c = tokenizer.buffer <- c :: tokenizer.buffer

  let pop tokenizer = match tokenizer.buffer with
    | c :: cs -> tokenizer.buffer <- cs; c
    |   []    -> tokenizer.get ()

  let until tokenizer sep =
    let retval = Buffer.create 0 in

    let rec loop ch =
      if sep ch then (Some ch, Buffer.contents retval)
      else try Buffer.add_char retval ch; loop (pop tokenizer)
      with End_of_file -> (None, Buffer.contents retval)
    in loop (pop tokenizer)

  let rec discard tokenizer fn = let ch = pop tokenizer in
    if fn ch then discard tokenizer fn else push tokenizer ch

  let negate b     = fun x -> not (b x)
  let orelse b1 b2 = fun x -> b1 x || b2 x
  let equal c1     = fun c2 -> c1 = c2

  let whitespace ch = ch = ' ' || ch = '\n' || ch = '\r' || ch = '\t'
  let bracket ch    = ch = '(' || ch = ')'
  let nl ch         = ch = '\r' || ch = '\n'

  let special = orelse whitespace bracket

  let rec next tokenizer =
    try match pop tokenizer with
    | ';'                   -> discard tokenizer (negate nl); next tokenizer
    | '('                   -> Lparen
    | ')'                   -> Rparen
    | '"'                   -> let (c, x) = until tokenizer (equal '"') in if Option.is_none c then raise UnexpectedEOF else Literal x
    | ch when whitespace ch -> next tokenizer
    | ch                    -> push tokenizer ch; let (c, x) = until tokenizer special in (Option.iter (push tokenizer) c; Ident x)
    with End_of_file -> Eof
end

module Reader =
struct
  let ident = function
    | "false" -> Bool false
    | "true"  -> Bool true
    | "nil"   -> Expr.eps
    | x       ->
      try Int (int_of_string x) with Failure _ ->
      try Float (float_of_string x) with Failure _ -> Symbol x

  let rec expr t = function
    | Eof       -> None
    | Lparen    -> Some (List (paren t))
    | Literal x -> Some (String (Scanf.unescaped x))
    | Ident x   -> Some (ident x)
    | Rparen    -> raise MismatchedBracket

  and paren t = match Tokenizer.next t with
    | Rparen -> []
    | tok    ->
      match expr t tok with
      | Some e -> e :: paren t
      | None   -> raise UnexpectedEOF
end

let read tokenizer = Reader.expr tokenizer (Tokenizer.next tokenizer)