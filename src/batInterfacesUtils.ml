(*
 * InterfacesUtils - auxiliary functions for interface definitions
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

(** A powerful interface can be obtained from a very small set of
    combinators. However, for ease of use and consistency reasons, it
    is helpful to provide derived functions built from those combinators.

    For example, a [mapi : (int -> 'a -> 'b) -> 'a t -> 'b t] can
    usually be derived from the corresponding [map] function, by using
    a counter to produce an additional increasing integer
    parameter. This is done by the function [reindex] in this file. Of
    course, not all [mapi] implementation use [reindex] : it is
    sometimes more efficient to reuse a counter directly available in
    the data structure.

    This file is for combinators used to build such derived functions.
*)

(**
    {b Implementation note} : We do not place those definitions in the
    BatInterfaces module for two reasons :
    <ul>
      <li>we wish to avoid compilation dependencies on BatInterfaces
      when possible (helps separate compilation)</li>
      <li>BatInterfaces is mainly a set of module types (interfaces),
      and it is therefore very interesting to use the ability of the
      OCaml compiler to derive the module from the .mli file only,
      without .ml file : this reduces the implementation/interface
      redundancy. By adding values to BatInterfaces, we would lose
      that ability.</li>
   </ul>

    Similarly, there is no .mli associated to batInterfacesUtils,
    which is only an implementation file. Those functions are not
    necessarily meant to be published outside batInterfaces. If
    a particular function is considered useful, it will be added to
    the BatStd or BatPervasives module and documented there.

    We did not use batStd or batPervasives in the first place because
    of cyclic dependencies : for example, BatEnum may use
    a BatInterfacesUtils function, but BatStd depends on BatEnum.
*)

(* documented in batStd.mli *)
let reindex f =
  let count = ref (-1) in
  fun x ->
    incr count;
    f !count x
