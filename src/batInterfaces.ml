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

module type Mappable = sig
  type 'a mappable
  val map : ('a -> 'b) -> ('a mappable -> 'b mappable)
end

module type MonoMappable = sig
  type map_elem
  type mappable
        
  val map : (map_elem -> map_elem) -> mappable -> mappable
end

module type MappableAssoc = sig
  type ('a, 'b) mappable

  val map : ('b -> 'c) -> ('a, 'b) mappable -> ('a, 'c) mappable
  val mapi : ('a -> 'b -> 'c) -> ('a, 'b) mappable -> ('a, 'c) mappable
end

module type MappableMonoAssoc = sig
  include Mappable
  type mapi_key
  val mapi : (mapi_key -> 'a -> 'b) -> 'a mappable -> 'b mappable
end

module type MonoMappableMonoAssoc = sig
  include MonoMappable
  type mapi_key
  val mapi : (mapi_key -> map_elem -> map_elem) -> mappable -> mappable
end

module type OrderedType =
sig
  type t
  val compare : t -> t -> int
end

let reindex f =
  let count = ref (-1) in
  fun x ->
    incr count;
    f !count x
