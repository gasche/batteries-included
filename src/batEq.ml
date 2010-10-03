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

module type EQUIV = sig
  type ('a, 'b) t
  val refl : ('a, 'a) t
  val symm : ('a, 'b) t -> ('b, 'a) t
  val trans : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val apply : ('a, 'b) t -> 'a -> 'b
end

module Iso : EQUIV = struct
  type ('a, 'b) t = ('a -> 'b) * ('b -> 'a)
  let refl = (fun x -> x), (fun x -> x)
  let symm (a, b) = (b, a)
  let trans (fab, fba) (fbc, fcb) =
    (fun a -> fbc (fab a)), (fun c -> fba (fcb c))

  let apply = fst
end;;

(* The following modules is from http://okmij.org/ftp/ML/GADT.ml *)
module Eq : sig
  include EQUIV
  module EQF1(X:sig type 'a r end) : sig		
    (* Leibniz principle *)
    val feq : ('a,'b) t -> ('a X.r, 'b X.r) t
  end
end = struct
  type ('a,'b) t = Refl
  let refl = Refl
  let symm Refl = Refl
  let trans Refl Refl = Refl
  let apply Refl = Obj.magic

  module EQF1(X:sig type 'a r end) = struct
    (* Just like in Agda *)
    let feq Refl = Refl
  end
end;;
