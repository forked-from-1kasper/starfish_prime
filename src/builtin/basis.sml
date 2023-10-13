val printImpl   = fn E => fn e => List.app (print o printExpr) e
val newlineImpl = nulary (fn E => print "\n")

fun readImpl E stxs =
let
  val t = case stxs of
    []  => TextIO.scanStream Reader.expr TextIO.stdIn
  | [e] => StringCvt.scanString Reader.expr (Expr.getString e)
  | es  => raise (TooManyParams es)
in
  case t of SOME e => e | NONE => raise NoExpression
end

val IO = (* Input/output *)
[("print",   effect printImpl),
 ("newline", effect newlineImpl),
 ("read",    eager  readImpl)]

val builtin = Arithmetic @ Meta @ Control @ IO @ Data @ Math