(* 
 * ExtSet - Extended operations on sets
 * Copyright (C) 1996 Xavier Leroy
 *               2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans
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

(** Sets over ordered types.

    This module implements the set data structure, given a total
    ordering function over the set elements. All operations over sets
    are purely applicative (no side-effects).  The implementation uses
    balanced binary trees, and is therefore reasonably efficient:
    insertion and membership take time logarithmic in the size of the
    set, for instance.

    {b Note} OCaml, Batteries Included, provides two implementations
    of sets: polymorphic sets (module {!PSet}) and functorized sets
    (this module). Module {!Set} offers a more complex and slightly
    poorer set of features but stronger type-safety. Module {!PSet} is
    easier to use and has a few more powerful features but makes it
    easier to shoot yourself in the foot. In case of doubt, use
    {!Set}.

    This module is built upon Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Set.html}Set}
    module, but provides the complete interface.

    @author Xavier Leroy (Base module)
    @author David Teller
*)

module type OrderedType = BatInterfaces.OrderedType
(** Input signature of the functor {!Set.Make}. *)

module type S_root =
  sig
    include Set.S
    
    val compare_subset: t -> t -> int
    (** Partial ordering between sets as generated by [subset] *)

    val map: (elt -> elt) -> t -> t
      (** [map f x] creates a new set with elements [f a0],
	  [f a1]... [f an], where [a1], ..., [an] are the
	  values contained in [x]*)

    val filter_map: (elt -> elt option) -> t -> t
      (** [filter_map f m] combines the features of [filter] and
	  [map].  It calls calls [f a0], [f a1], [f an] where [a0..an]
	  are the elements of [m] and returns the set of pairs [bi]
	  such as [f ai = Some bi] (when [f] returns [None], the
	  corresponding element of [m] is discarded). *)

    val enum: t -> elt BatEnum.t
      (** Return an enumeration of all elements of the given set.
	  The returned enumeration is sorted in increasing order with respect
	  to the ordering [Ord.compare], where [Ord] is the argument
	  given to {!Set.Make}. *)

    val backwards: t -> elt BatEnum.t
      (** Return an enumeration of all elements of the given set.
	  The returned enumeration is sorted in decreasing order with respect
	  to the ordering [Ord.compare], where [Ord] is the argument
	  given to {!Set.Make}. *)

    val of_enum: elt BatEnum.t -> t

    module StdSet : Set.S

    val to_stdset : t -> StdSet.t
      (** Convert a batteries set into a stdlib set *)

    (** {6 Boilerplate code}*)

    (** {6 Override modules}*)

    (**
       The following modules replace functions defined in {!Set} with functions
       behaving slightly differently but having the same name. This is by design:
       the functions meant to override the corresponding functions of {!Set}.
    *)
      
    (** Operations on {!Set} without exceptions.*)
    module Exceptionless : sig
      val min_elt: t -> elt option
      val max_elt: t -> elt option
      val choose:  t -> elt option
    end
      
      
    (** Operations on {!Set} with labels.
	
	This module overrides a number of functions of {!Set} by
	functions in which some arguments require labels. These labels are
	there to improve readability and safety and to let you change the
	order of arguments to functions. In every case, the behavior of the
	function is identical to that of the corresponding function of {!Set}.
    *)
    module Labels : sig
      val iter : f:(elt -> unit) -> t -> unit
      val fold : f:(elt -> 'a -> 'a) -> t -> init:'a -> 'a
      val for_all : f:(elt -> bool) -> t -> bool
      val exists : f:(elt -> bool) -> t -> bool
      val map: f:(elt -> elt) -> t -> t
      val filter : f:(elt -> bool) -> t -> t
      val filter_map: f:(elt -> elt option) -> t -> t
      val partition : f:(elt -> bool) -> t -> t * t
    end
      
  end
(** Output signature of the functor {!Set.Make}. *)

module type S = sig
  include S_root

  (** {7 Printing}*)
    
  val print :  ?first:string -> ?last:string -> ?sep:string -> 
    ('a BatInnerIO.output -> elt -> unit) -> 
    'a BatInnerIO.output -> t -> unit
end


module type PS = sig
  include S_root
    
  (** {7 Printing}*)
    
  val print :  ?first:string -> ?last:string -> ?sep:string -> 
    'a BatInnerIO.output -> t -> unit
end

module StringSet  : PS with type elt = String.t
(** A set of strings. Comparison of strings takes case into account (i.e. "foo" <> "Foo")*)

module IStringSet : PS with type elt = String.t
(** A set of strings. Comparison of strings ignores case (i.e. "foo" = "Foo")*)

module NumStringSet : PS with type elt = String.t
(** A set of strings. Comparison of strings takes into account embedded numbers (i.e. "a23" < "a123", "a01" = "a1") *)

module RopeSet    : PS with type elt = BatRope.t
(** A set of ropes. Comparison of ropes takes case into account (i.e. r"foo" <> r"Foo")*)

module IRopeSet   : PS with type elt = BatRope.t
(** A set of ropes. Comparison of ropes ignores case (i.e. r"foo" = r"Foo")*)

module IntSet     : PS with type elt = BatInt.t
(** A set of integers.*)


module Make (Ord : OrderedType) : S with type elt = Ord.t
(** Functor building an implementation of the set structure
   given a totally ordered type. *)

module Make_printable (E: BatInterfaces.OrderedPrintable) : PS with type elt = E.t
