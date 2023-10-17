fun max a b = if a >= b then a else b
fun min a b = if a >= b then b else a

signature LINORD =
sig
  eqtype t
  val compare : t * t -> order
end

signature VECTOR =
sig
  eqtype t
  eqtype el

  val compare : el * el -> order
  val lexical : t * t -> order
  val get     : t * int -> el
  val size    : t -> int
  val implode : el list -> t
  val explode : t -> el list
  val tail    : t * int -> t
end

signature DICT =
sig
  type key
  type 'a t

  val empty     : unit -> 'a t
  val isEmpty   : 'a t -> bool
  val add       : key -> 'a -> 'a t -> 'a t
  val get       : key -> 'a t -> 'a option
  val remove    : key -> 'a t -> 'a t
  val mem       : key -> 'a t -> bool
  val iter      : (key -> 'a -> unit) -> 'a t -> unit
  val map       : ('a -> 'b) -> 'a t -> 'b t
  val mapi      : (key -> 'a -> 'b) -> 'a t -> 'b t
  val update    : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val disjoint  : 'a t -> 'a t -> bool
  val inter     : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val forAll    : (key -> 'a -> bool) -> 'a t -> bool
  val forAll2   : (key -> 'a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val fold      : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

signature SET =
sig
  type el
  type t

  val empty     : unit -> t
  val isEmpty   : t -> bool
  val add       : el -> t -> t
  val remove    : el -> t -> t
  val mem       : el -> t -> bool
  val iter      : (el -> unit) -> t -> unit
  val singleton : el -> t
  val disjoint  : t -> t -> bool
  val inter     : t -> t -> t
  val forAll    : (el -> bool) -> t -> bool
  val forAll2   : (el -> bool) -> t -> t -> bool
  val fold      : (el -> 'a -> 'a) -> t -> 'a -> 'a
end

functor Explode (K : VECTOR) : LINORD =
struct
  type t = K.el
  val compare = K.compare
end

functor Implode (K : VECTOR) : LINORD =
struct
  type t = K.t
  val compare = K.lexical
end

functor Set (D : DICT) : SET =
struct
  type el = D.key
  type t  = unit D.t

  val empty   = D.empty
  val isEmpty = D.isEmpty

  fun add v  = D.add v ()
  val remove = D.remove
  val mem    = D.mem

  fun iter f = D.iter (fn k => fn () => f k)

  fun singleton v = D.singleton v ()

  val disjoint = D.disjoint

  val inter = D.inter (fn k1 => fn k2 => fn () => ())

  fun forAll  f = D.forAll  (fn k => fn () => f k)
  fun forAll2 f = D.forAll2 (fn k => fn () => fn () => f k)

  fun fold f = D.fold (fn k => fn () => f k)
end