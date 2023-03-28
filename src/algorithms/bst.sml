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