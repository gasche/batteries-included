open BatInterfaces

(** monad interface with one free parameter *)
module type Monad1 = sig
  type ('a, 'p1) m
  val bind : ('a, 'p1) m -> ('a -> ('b, 'p1) m) -> ('b, 'p1) m
  val return: 'a -> ('a, 'p1) m
end

(** monad interface with two free parameters *)
module type Monad2 = sig
  type ('a, 'p1, 'p2) m
  val bind : ('a, 'p1, 'p2) m -> ('a -> ('b, 'p1, 'p2) m) -> ('b, 'p1, 'p2) m
  val return: 'a -> ('a, 'p1, 'p2) m
end

(** free parameters can be instantiated to recover a Monad signature *)
module Make1 (M : Monad1) (P1 : sig type t end)
  : (Monad with type 'a m = ('a, P1.t) M.m)
= struct
  type 'a m = ('a, P1.t) M.m
  let bind = M.bind
  let return = M.return
end

module Make2 (M : Monad2) (P1 : sig type t end) (P2 : sig type t end)
  : (Monad with type 'a m = ('a, P1.t, P2.t) M.m)
= struct
  type 'a m = ('a, P1.t, P2.t) M.m
  let bind = M.bind
  let return = M.return
end
