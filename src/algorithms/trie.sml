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

  val empty = D.empty
  val isEmpty = D.isEmpty

  fun add v = D.add v ()
  val remove = D.remove
  val mem = D.mem

  fun iter f = D.iter (fn k => fn () => f k)

  fun singleton v = D.singleton v ()

  val disjoint = D.disjoint

  val inter = D.inter (fn k1 => fn k2 => fn () => ())

  fun forAll f = D.forAll (fn k => fn () => f k)
  fun forAll2 f = D.forAll2 (fn k => fn () => fn () => f k)

  fun fold f = D.fold (fn k => fn () => f k)
end

functor BST (A : LINORD) : DICT =
struct
  type key = A.t

  datatype 'a t = LEAF | NODE of 'a t * key * 'a * 'a t * int

  fun empty () = LEAF
  val isEmpty = fn LEAF => true | NODE _ => false

  val height = fn LEAF => 0 | NODE (_, _, _, _, h) => h

  fun create l k v r = NODE (l, k, v, r, max (height l) (height r) + 1)
  fun singleton k v = NODE (LEAF, k, v, LEAF, 1)

  fun balance l k v r =
  let
    val hl = height l
    val hr = height r

    fun right () = case l of
      LEAF => raise Empty
    | NODE (ll, lk, lv, lr, _) =>
      if height ll >= height lr then
        create ll lk lv (create lr k v r)
      else case lr of
        LEAF => raise Empty
      | NODE (lrl, lrk, lrv, lrr, _) =>
        create (create ll lk lv lrl) lrk lrv (create lrr k v r)

    fun left () = case r of
      LEAF => raise Empty
    | NODE (rl, rk, rv, rr, _) =>
      if height rr >= height rl then
        create (create l k v rl) rk rv rr
      else case rl of
        LEAF => raise Empty
      | NODE (rll, rlk, rlv, rlr, _) =>
        create (create l k v rll) rlk rlv (create rlr rk rv rr)
  in
    if hl > hr + 2 then right ()
    else if hr > hl + 2 then left ()
    else NODE (l, k, v, r, max hl hr + 1)
  end

  fun add k v = fn
    LEAF => singleton k v
  | NODE (l, k', v', r, h) =>
    case A.compare (k, k') of
      EQUAL   => NODE (l, k, v, r, h)
    | LESS    => balance (add k v l) k' v' r
    | GREATER => balance l k' v' (add k v r)

  fun get k = fn
    LEAF => NONE
  | NODE (l, k', v', r, _) =>
    case A.compare (k, k') of
      EQUAL   => SOME v'
    | LESS    => get k l
    | GREATER => get k r

  fun mem k = fn
    LEAF => false
  | NODE (l, k', _, r, _) =>
    case A.compare (k, k') of
      EQUAL   => true
    | LESS    => mem k l
    | GREATER => mem k r

  fun iter f = fn LEAF => () | NODE (l, k, v, r, _) => (iter f l; f k v; iter f r)
  fun mapi f = fn LEAF => LEAF | NODE (l, k, v, r, h) => NODE (mapi f l, k, f k v, mapi f r, h)

  fun map f = mapi (fn _ => f)

  val rec popMinEntry = fn
    LEAF => raise Empty
  | NODE (LEAF, k, v, r, _) => (k, v, r)
  | NODE (l, k, v, r, _) =>
  let
    val (k', v', l') = popMinEntry l
  in
    (k', v', balance l' k v r)
  end

  fun merge t1 t2 =
    case (t1, t2) of
    (LEAF, t)    => t
  | (t,    LEAF) => t
  | (_,    _)    =>
  let
    val (k, v, t) = popMinEntry t2
  in
    balance t1 k v t
  end

  fun remove k = fn
    LEAF => LEAF
  | NODE (l, k', v', r, _) =>
    case A.compare (k, k') of
      EQUAL   => merge l r
    | LESS    => balance (remove k l) k' v' r
    | GREATER => balance l k' v' (remove k r)

  fun update k f = fn
    LEAF => (case f NONE of
      NONE   => LEAF
    | SOME v => singleton k v)
  | NODE (l, k', v', r, h) =>
    case A.compare (k, k') of
      LESS    => balance (update k f l) k' v' r
    | GREATER => balance l k' v' (update k f r)
    | EQUAL   => case f (SOME v') of
        NONE   => merge l r
      | SOME v => NODE (l, k', v, r, h)

  local
    fun addMinElement k v = fn
      LEAF => singleton k v
    | NODE (l, k', v', r, _) => balance (addMinElement k v l) k' v' r

    fun addMaxElement k v = fn
      LEAF => singleton k v
    | NODE (l, k', v', r, _) => balance l k' v' (addMaxElement k v r)
  in
    fun join l k v r =
    case (l, r) of
      (LEAF, _) => addMinElement k v r
    | (_, LEAF) => addMaxElement k v l
    | (NODE (ll, lk, lv, lr, lh), NODE (rl, rk, rv, rr, rh)) =>
      if lh > rh + 2 then balance ll lk lv (join lr k v r) else
      if rh > lh + 2 then balance (join l k v rl) rk rv rr else
      create l k v r
  end

  local
    fun split k v = fn
      LEAF => SOME (LEAF, fn () => LEAF)
    | NODE (l, k', v', r, _) =>
      case A.compare (k, k') of
        EQUAL   => NONE
      | LESS    => (case split k v l of
          NONE => NONE
        | SOME (ll, rl) => SOME (ll, fn () => join (rl ()) k' v' r))
      | GREATER => (case split k v r of
          NONE => NONE
        | SOME (lr, rr) => SOME (join l k' v' lr, rr))
  in
    fun disjoint t1 t2 =
      case (t1, t2) of
      (LEAF, _) => true
    | (_, LEAF) => true
    | (NODE (l1, k1, v1, r1, _), _) =>
      case split k1 v1 t2 of
        NONE          => false
      | SOME (l2, r2) => disjoint l1 l2 andalso disjoint r1 (r2 ())
  end

  local
    val rec minElem = fn
      LEAF                    => raise Empty
    | NODE (LEAF, k, v, _, _) => (k, v)
    | NODE (l, _, _, _, _)    => minElem l

    val rec removeMinElem = fn
      LEAF                    => raise Empty
    | NODE (LEAF, _, _, r, _) => r
    | NODE (l, k, v, r, _)    => balance (removeMinElem l) k v r
  in
    fun concat t1 t2 =
      case (t1, t2) of
      (LEAF, t)    => t
    | (t,    LEAF) => t
    | (_,    _)    =>
    let
      val (k, v) = minElem t2
    in
      join t1 k v (removeMinElem t2)
    end
  end

  fun split k = fn
    LEAF => (LEAF, NONE, LEAF)
  | NODE (l, k', v', r, _) =>
    case A.compare (k, k') of
      EQUAL   => (l, SOME v', r)
    | LESS    => let val (ll, pres, rl) = split k l in (ll, pres, join rl k' v' r) end
    | GREATER => let val (lr, pres, rr) = split k r in (join l k' v' lr, pres, rr) end

  fun inter f t1 t2 =
      case (t1, t2) of
      (LEAF, _) => LEAF
    | (_, LEAF) => LEAF
    | (NODE (l1, k, v1, r1, _), _) =>
      case split k t2 of
        (l2, NONE,    r2) => concat (inter f l1 l2) (inter f r1 r2)
      | (l2, SOME v2, r2) => join (inter f l1 l2) k (f k v1 v2) (inter f r1 r2)

  fun forAll f = fn LEAF => true | NODE (l, k, v, r, _) => f k v andalso forAll f l andalso forAll f r

  fun forAll2 f t1 t2 =
      case (t1, t2) of
      (LEAF, _) => true
    | (_, LEAF) => true
    | (NODE (l1, k, v1, r1, _), _) =>
      case split k t2 of
        (l2, NONE,    r2) => forAll2 f l1 l2 andalso forAll2 f r1 r2
      | (l2, SOME v2, r2) => f k v1 v2 andalso forAll2 f l1 l2 andalso forAll2 f r1 r2

  fun fold f t acc = case t of
    LEAF                 => acc
  | NODE (l, k, v, r, _) => fold f r (f k v (fold f l acc))
end

structure CHAR : LINORD =
struct
  type t = char
  val compare = Char.compare
end

structure STRING : VECTOR =
struct
  type t  = string
  type el = char

  val compare     = Char.compare
  val lexical     = String.compare
  val get         = String.sub
  val size        = String.size
  val implode     = String.implode
  val explode     = String.explode
  fun tail (k, i) = String.extract (k, i, NONE)
end

functor Trie (K : VECTOR) : DICT =
struct
  type key = K.t

  structure T = BST(Explode(K))

  datatype 'a t = JOIN of 'a t T.t * 'a option

  fun empty () = JOIN (T.empty (), NONE)

  fun isEmpty (JOIN (t, v)) = T.isEmpty t andalso not (Option.isSome v)

  fun singleton k v =
  let
    fun loop n t =
      if n < 0 then t
      else loop (n - 1) (JOIN (T.singleton (K.get (k, n)) t, NONE))
  in
    loop (K.size k - 1) (JOIN (T.empty (), SOME v))
  end

  fun add k v =
  let
    val s = K.size k

    fun loop n (JOIN (ts, vs)) =
      if n >= s then JOIN (ts, SOME v)
      else JOIN (T.update (K.get (k, n))
        (fn NONE   => SOME (singleton (K.tail (k, n + 1)) v)
          | SOME t => SOME (loop (n + 1) t)) ts, vs)
  in
    loop 0
  end

  fun get k =
  let
    val s = K.size k

    fun loop n (JOIN (ts, vs)) =
      if n >= s then vs
      else case T.get (K.get (k, n)) ts of
        NONE   => NONE
      | SOME t => loop (n + 1) t
  in
    loop 0
  end

  fun mem k =
  let
    val s = K.size k

    fun loop n (JOIN (ts, vs)) =
      if n >= s then Option.isSome vs
      else case T.get (K.get (k, n)) ts of
        NONE   => false
      | SOME t => loop (n + 1) t
  in
    loop 0
  end

  fun join t = fn
    NONE => if T.isEmpty t then NONE else SOME (JOIN (t, NONE))
  | v    => SOME (JOIN (t, v))

  fun remove k t =
  let
    val s = K.size k

    fun loop n (JOIN (ts, vs)) =
      if n >= s then join ts NONE
      else join (T.update (K.get (k, n))
                          (fn NONE   => NONE
                            | SOME t => loop (n + 1) t) ts) vs
  in
    Option.getOpt (loop 0 t, empty ())
  end

  fun update k f t =
  let
    val s = K.size k

    fun loop n (JOIN (ts, vs)) =
      if n >= s then case f vs of
        NONE => join ts NONE
      | v    => SOME (JOIN (ts, v))
      else join (T.update (K.get (k, n))
                          (fn SOME t => loop (n + 1) t
                            | NONE   => case f NONE of
                                SOME v => SOME (singleton (K.tail (k, n + 1)) v)
                              | NONE   => NONE) ts) vs
  in
    Option.getOpt (loop 0 t, empty ())
  end

  fun iter f =
  let
    fun loop acc (JOIN (ts, vs)) =
      (Option.app (f (K.implode (List.rev acc))) vs;
       T.iter (fn c => loop (c :: acc)) ts)
  in
    loop []
  end

  fun mapi f =
  let
    fun loop acc (JOIN (ts, vs)) =
      JOIN (T.mapi (fn c => loop (c :: acc)) ts,
            Option.map (f (K.implode (List.rev acc))) vs)
  in
    loop []
  end

  fun map f = mapi (fn _ => f)

  fun disjoint (JOIN (ts1, v1)) (JOIN (ts2, v2)) =
    if Option.isSome v1 andalso Option.isSome v2 then false
    else T.forAll2 (fn _ => disjoint) ts1 ts2

  fun inter f =
  let
    fun loop acc (JOIN (ts1, ov1)) (JOIN (ts2, ov2)) =
      let
        val v = case (ov1, ov2) of
          (SOME v1, SOME v2) => SOME (f (K.implode (List.rev acc)) v1 v2)
        | (_,       _)       => NONE
      in
        JOIN (T.inter (fn c => loop (c :: acc)) ts1 ts2, v)
      end
  in
    loop []
  end

  fun forAll f =
  let
    fun forOption f = fn
      SOME v => f v
    | NONE   => true

    fun loop acc (JOIN (ts, ov)) =
      forOption (f (K.implode (List.rev acc))) ov
      andalso T.forAll (fn c => loop (c :: acc)) ts
  in
    loop []
  end

  fun forAll2 f =
  let
    fun forOption2 f = fn
      (SOME v1, SOME v2) => f v1 v2
    | (_,       _)       => true

    fun loop acc (JOIN (ts1, ov1)) (JOIN (ts2, ov2)) =
      forOption2 (f (K.implode (List.rev acc))) (ov1, ov2)
      andalso T.forAll2 (fn c => loop (c :: acc)) ts1 ts2
  in
    loop []
  end

  fun fold f =
  let
    fun loop k (JOIN (ts, ov)) acc =
    let
      val acc' = T.fold (fn c => loop (c :: k)) ts acc
    in
      case ov of NONE => acc' | SOME v => f (K.implode (List.rev k)) v acc
    end
  in
    loop []
  end
end

structure Dict = Trie(STRING)
structure Bag = Set(BST(Implode(STRING)))

type bag = Bag.t
type 'a dict = 'a Dict.t