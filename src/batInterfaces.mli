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
   a standardized interface for the following functions :<ul>
   <li>[map : ('a -> 'b) -> ('a t -> 'b t)]</li>
   <li>[mapi : (mapi_key -> 'a -> 'b) -> ('a t -> 'b t)]</li></ul>
   where [mapi_key] is a type defined by the interface (see below).

   [map] and [mapi] apply a given function to every element of a data
   structure :<ul>
   <li>if [a0,a1..aN] are the elements of [e], [map f e] returns the
   structure formed from [f a0], [f a1], ..., [f aN].</li>
   <li> if [a0,a1..aN] are respectively indexed by [k0,k1..kN], [mapi
   f e] returns the structure formed from [f k0 a0], [f k1 a1], ...,
   [f kN aN].</li></ul>

   For non-associative data structures, the default indexing choice is
   to use integers starting from 0, in increasing order: [type
   mapi_key = int]. For example, !BatList.mapi will pass integer [k]
   along with the element at position [k] in the list.

   Not all map function are parametric like [('a -> 'b) -> ('a t -> 'b
   t)].  For example, String.map : [(char -> char) -> string ->
   string] is a monomorphic function over a non-parametric type.

   There are actually three different interfaces, corresponding to the
   different levels of parametrization :<ul>
   <li>!BatInterfaces.Mappable0 : map on monomorphic [t] data structures
   [Map.S.mapi : (key -> 'a -> 'b) -> 'a t -> 'b t]</li>
   <li>!BatInterfaces.Mappable1 : map on ['a t] 1-parametric data structures
   [String.map : (char -> char) -> string -> string] </li>
   <li>!BatInterfaces.Mappable2 : map on [('a, 'b) t] 2-parametric data
   structures, with key type ['a] 
   [PMap.mapi : ('a -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t]</li>
   </ul>

   Note that the [mapi_key] type on which mapi indexes is parametric
   for [Mappable2] but constant for [Mappable0] and [Mappable1]. In
   that case, it is generally [int], but other types are possible,
   such as [key] in the !BatMap.S example.
*)

module type Mappable0 = sig
  type mappable
  type map_elem
  type mapi_key

  val map : (map_elem -> map_elem) -> mappable -> mappable
  val mapi : (mapi_key -> map_elem -> map_elem) -> mappable -> mappable
end

module type Mappable1 = sig
  type 'a mappable
  type mapi_key

  val map : ('a -> 'b) -> 'a mappable -> 'b mappable
  val mapi : (mapi_key -> 'a -> 'b) -> 'a mappable -> 'b mappable
end

module type Mappable2 = sig
  type ('a, 'b) mappable
  
  val map : ('a -> 'b) -> ('c, 'a) mappable -> ('c, 'b) mappable
  val mapi : ('a -> 'b -> 'c) -> ('a, 'b) mappable -> ('a, 'c) mappable
end

module type Mappable = Mappable1
(** We use [Mappable] as default name for [Mappable1], as it is the
    most natural/common mappable signature. *)

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
