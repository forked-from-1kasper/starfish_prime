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