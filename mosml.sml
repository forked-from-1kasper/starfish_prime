fun fst (x, y) = x
fun snd (x, y) = y

local
  fun kind x = fn y => "$(SRCDIR)/" ^ y ^ "." ^ x
in
  val ui  = kind "ui"
  val uo  = kind "uo"
  val sml = kind "sml"
end

val concat = String.concatWith " "

fun rule targets deps cmds =
  print (concat targets ^ ": " ^ concat deps ^ "\n" ^ String.concatWith "\n" (List.map (fn cmd => "\t" ^ cmd) cmds) ^ "\n")

fun compile target deps =
  "$(MOSML)/bin/mosmlc -c " ^ concat (List.map ui deps) ^ " -toplevel " ^ sml target

fun link xs = "$(MOSML)/bin/mosmlc -o $(BINARY) " ^ concat xs

val project =
[("algorithms/sig",     []),
 ("algorithms/bst",     ["algorithms/sig"]),
 ("algorithms/trie",    ["algorithms/sig", "algorithms/bst"]),
 ("kernel/assemblage",  ["algorithms/trie"]),
 ("kernel/expr",        ["algorithms/trie", "kernel/assemblage"]),
 ("kernel/reader",      ["kernel/expr"]),
 ("kernel/exc",         ["kernel/assemblage", "kernel/expr", "kernel/reader"]),
 ("builtin/common",     ["kernel/expr"]),
 ("builtin/arithmetic", ["kernel/expr", "builtin/common"]),
 ("builtin/meta",       ["kernel/expr", "builtin/common"]),
 ("builtin/data",       ["algorithms/trie", "kernel/assemblage", "kernel/expr", "builtin/common"]),
 ("builtin/control",    ["kernel/assemblage", "kernel/expr", "builtin/common"]),
 ("builtin/math",       ["algorithms/trie", "kernel/exc", "kernel/assemblage", "kernel/expr", "builtin/common"]),
 ("builtin/basis",      ["algorithms/trie", "kernel/assemblage", "kernel/expr", "kernel/reader", "kernel/exc", "builtin/common", "builtin/arithmetic",
                         "builtin/meta", "builtin/data", "builtin/control", "builtin/math"]),
 ("main",               ["algorithms/trie", "kernel/assemblage", "kernel/expr", "kernel/reader", "kernel/exc", "builtin/common", "builtin/basis"])]

local
  val code = List.map (uo o fst) project
in
  val () = List.app (fn (x, xs) => rule [ui x, uo x] (sml x :: List.map ui xs) [compile x xs]) project
  val () = rule ["$(BINARY)"] code [link code]
end

val () = quit ()