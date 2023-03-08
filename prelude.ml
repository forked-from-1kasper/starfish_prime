let const a b = a

let (>>) f g = fun x -> g (f x)
let (<<) f g = fun x -> f (g x)

let xor a b = (a || b) && not (a && b)