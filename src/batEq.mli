(*
 * Monad -- Base monadic operations
 * Copyright (C) 2010 Oleg Kiseliov
 * Copyright (C) 2010 Gabriel Scherer
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** This module contains three presentation of type isomorphisms.

    Iso denotes a simple type isomorphism, that does not necessarily
    translate into a type equality. It maye be useful when handling
    recursive datatypes : we may want to express such datatypes as
    fixpoint of simpler datatypes
    (eg. ['a list = µ'b.(unit + 'a * 'b)]), but we only have an
    isomorphism rather than a representation equality.

    Eq denotes a representational equality between datatypes. It takes
    advantage of the exact equality to use unsafe (Obj.magic) coercion
    methods. It can also provide a Leibniz equality principle,
    a functor lifting the type equality to parametrized types.

    With OCaml 3.12, a new way of defining equality witnesses using
    first-class module makes the Leibniz equality usable even for safe
    equality implementations. See http://ocaml.janestreet.com/?q=node/81
    Batteries currently does not depend on OCaml >= 3.12, so we will
    wait before using such advanced features.
*)

module type EQUIV =
  sig
    type ('a, 'b) t
    (** [('a, 'b) t] values witness equivalence between the types ['a] and ['b] *)

    val refl : ('a, 'a) t
    val symm : ('a, 'b) t -> ('b, 'a) t
    val trans : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t

    val apply : ('a, 'b) t -> 'a -> 'b
    (** apply use a [('a, 'b)] equivalence witness to convert any
       element of type ['a] to an element of type ['b] *)
  end

module Iso : EQUIV
(** Iso represents type isomorphisms :
    [('a, 'b) Iso.t] witnesses a total bijection between 'a and 'b
 
    It has a perfectly safe implementation :
      [type ('a, 'b) t = ('a -> 'b) * ('b -> 'a)]
 *)


module Eq :
  sig
    include EQUIV

    module EQF1 :
      functor (X : sig type 'a r end) ->
        sig val feq : ('a, 'b) t -> ('a X.r, 'b X.r) t end
    (** Leibniz equality : if ['a] = ['b], then for any parametrized type
      ['a t], ['a t] = ['b t]. 

       It is useful to lift equality to more structured types without
       using mapping functions at runtime : [('a list, 'b list) Eq.t]
       can be derived from [EQF1], without using [List.map]. *)
  end
(** Eq represents type equality, and is implemented using unsafe operations *)
