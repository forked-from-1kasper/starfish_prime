let const a b = a

let (>>) f g = fun x -> g (f x)
let (<<) f g = fun x -> f (g x)

let xor a b = (a || b) && not (a && b)

let exchange x y = fun z -> if z = x then y else if z = y then x else z

module Dict = Map.Make(String)
module Bag  = Set.Make(String)