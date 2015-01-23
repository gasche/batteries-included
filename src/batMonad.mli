open BatInterfaces

(** monad interface with one free parameter *)
module type Monad1 = sig
  type ('a, 'p1) m
  val bind : ('a, 'p1) m -> ('a -> ('b, 'p1) m) -> ('b, 'p1) m
  val return: 'a -> ('a, 'p1) m
end

(** Compared to the [Monad] module with type [type 'a m = ...], the
    [Monad1] signature above has one extra type parameter ['p1]. To
    use a [Monad1] implementation where a [Monad] module is expected
    (for example as a parameter to the {!Enum.WithMonad} functor), you
    can use the [Make1] functor below. You must decide how to
    instantiate the ['p1] parameter. Here are two representative
    examples, one where it is instantiated monomorphically, one where
    the instantiation is a polymorphic type:

{|
  (* monomorphic case: instantiate the functor with a fixed type *)
  let sequence (seq : ('a, exn) Result.t Enum.t) : ('a Enum.t, exn) Result.t =
    let module M = Monad.Make1(Result.Monad)(struct type t = exn end) in
    let module RE = Enum.WithMonad(M) in
    RE.sequence seq

  (* polymorphic case: instantiate the functor with a type parameter *)
  let sequence (type b) (seq : ('a, b) Result.t Enum.t) : ('a Enum.t, b) Result.t =
    let module M = Monad.Make1(Result.Monad)(struct type t = b end) in
    let module RE = Enum.WithMonad(M) in
    RE.sequence seq
|}
 *)

module Make1 (M : Monad1) (P1 : sig type t end)
: (Monad with type 'a m = ('a, P1.t) M.m)

(** monad interface with two free parameters *)
module type Monad2 = sig
  type ('a, 'p1, 'p2) m
  val bind : ('a, 'p1, 'p2) m -> ('a -> ('b, 'p1, 'p2) m) -> ('b, 'p1, 'p2) m
  val return: 'a -> ('a, 'p1, 'p2) m
end

module Make2 (M : Monad2) (P1 : sig type t end) (P2 : sig type t end)
: (Monad with type 'a m = ('a, P1.t, P2.t) M.m)
