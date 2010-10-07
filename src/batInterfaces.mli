(*
 * Interfaces - Common interfaces for data structures
 * Copyright (C) 2008 David Teller, LIFO, Universite d'Orleans
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

(**
   Common signatures for data structures.
*)


(**
   Signatures for mappable data structure

   The core idea of the *Mappable* signatures is to provide
   a standardized interface for the following functions :
   
     map : ('a -> 'b) -> ('a t -> 'b t)
     mapi : (key -> 'a -> 'b) -> ('a t -> 'b t)

   Map applies a given function to every element of a data structure,
   and mapi applies a given function to every element of an
   associative data structure : for each association (i,a) in the
   structure, produce the new association (i, f i a).

   Examples : Array has map and mapi ('a array are considered as
   associations from int to 'a).

   Not all map function look like ('a -> 'b) -> ('a t -> 'b t).
   For example, String.map : (char -> char) -> string -> string
   does not make use of a parametric type : it's a *monomorphic*
   map function.

   There are actually three orthogonal aspects :
   1. is the data structure associative : does¹ it have a mapi function ?
        yes : 'a array, 'a Map.S.t, ('a, 'b) PMap.t
        no : 'a list, 'a DllList.t
   2. is the element type polymorphic ?
        yes : 'a array, 'a list
        no : string, Set.S.t
   3. is the *index* type (for an associative structure) polymorphic ?
        yes : ('a, 'b) PMap.t, ('a, 'b) Hashtbl.t
        no : 'a array, 'a Map.S.t
   
   There are five corresponding interfaces :
   - Mappable : non-associative, polymorphic map (List.map)
   - MonoMappable : non-associative, monomorphic map (Buffer.map ?)
   - MappableAssoc : associative, polymorphic map, polymorphic key/index
       PMap.map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
       PMap.mapi : ('a -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
   - MappableMonoAssoc : associative, polymorphic, monomorphic key
       Array.mapi : (int -> 'a -> 'b) -> 'a array -> 'b array
   - MonoMappableMonoAssoc : associative, monomorphic, monomorphic keys
       String.mapi ? : (int -> char -> char) -> string -> string

  *)

module type Mappable = sig
  type 'a mappable (** The data structure, e.g. ['a List.t] *)

  val map : ('a -> 'b) -> ('a mappable -> 'b mappable)
    (** [map f e] applies [f] to every element of [e] and returns the corresponding data structure *)
end
(** A signature for data structures which have a
    [map : ('a -> 'b) -> ('a t -> 'b t)] operation.
*)

module type MonoMappable = sig
  type map_elem
  type mappable
        
  val map : (map_elem -> map_elem) -> mappable -> mappable
  (** [map f e] applies [f] to every element of [e] and returns the corresponding data structure *)
end
(**
   A signature for monomorphic mappable data structures (constant [elem] type).
 *)

module type MappableAssoc = sig
  type ('a, 'b) mappable

  val map : ('b -> 'c) -> ('a, 'b) mappable -> ('a, 'c) mappable
  val mapi : ('a -> 'b -> 'c) -> ('a, 'b) mappable -> ('a, 'c) mappable
end
(** 
  A signature for associative data structures featuring a [mapi] operation :
    [mapi : ('a -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t]
*)

module type MappableMonoAssoc = sig
  include Mappable
  type mapi_key
  val mapi : (mapi_key -> 'a -> 'b) -> 'a mappable -> 'b mappable
end
(**
  A signature for associative data structure of monomorphic [key] type:
   [mapi : (key -> 'a -> 'b) -> 'a t -> 'b t]
*)

module type MonoMappableMonoAssoc = sig
  include MonoMappable
  type mapi_key
  val mapi : (mapi_key -> map_elem -> map_elem) -> mappable -> mappable
end
(**
  A signature for associative data structure of both constant [key] and [elem] type:
   [mapi : (key -> elem -> elem) -> t -> t]
*)


module type OrderedType =
sig
  type t
  val compare : t -> t -> int
    (** A total ordering function
        This is a two-argument function [f] such that
        [f e1 e2] is zero if the values [e1] and [e2] are equal,
        [f e1 e2] is strictly negative if [e1] is smaller than [e2],
        and [f e1 e2] is strictly positive if [e1] is greater than [e2].
        Example: a suitable ordering function is the generic structural
        comparison function {!Pervasives.compare}. *)
end


val reindex : (int -> 'a -> 'b) -> ('a -> 'b)
(** Reindex an int-indexed [(int -> 'a -> 'b)] function into an ['a ->
   'b] function. At the [n]th call of the reindexed function, it is
   given the int parameter [n].

   [# List.iter (reindex (Printf.printf "%d : %s\n")) ["a"; "b"; "c"];;]
   {v
   0 : a
   1 : b
   2 : c
    -: unit = ()
   v}

   This is used inside Batteries to derive a [mapi] function from the
   corresponding [map] function : [List.mapi f] is equivalent to
   [List.map (reindex f)].
*)
