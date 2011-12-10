(*
 * Uref -- unifiable references
 * Copyright (C) 2011  Batteries Included Development Team
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

(* Implements union-find with ranks and path-compression  *)

type 'a uref_contents =
  | Ranked of 'a head
  | Ptr of 'a uref
and 'a head = { mutable data : 'a; mutable rank : int }
and 'a uref = 'a uref_contents ref

type 'a t = 'a uref

let rec find curr = match !curr with
  | Ranked head -> (curr, head)
  | Ptr next -> find_back curr next
and find_back back curr =
  match !curr with
    | Ranked head -> (curr, head)
    | Ptr next ->
      let result = find_back curr next in
      (* we could write equivalently:
           let (head, _, _) = find_back curr next in
           back := Ptr head
         but !curr is (Ptr head) already, so this avoids allocation. *)
      back := !curr;
      result

let uref x = ref (Ranked { data = x; rank = 0 })

let uget ur =
  match !ur with
    | Ranked head -> head.data
    | Ptr p ->
      let (_, head) = find_back ur p in
      head.data

let uset ur x =
  match !ur with
    | Ranked head -> head.data <- x;
    | Ptr p ->
      let (_, head) = find_back ur p in
      head.data <- x

let equal ur vr =
  let (ur, _) = find ur in
  let (vr, _) = find vr in
  ur == vr

let unite ?sel ur vr =
  (* we use ?sel instead of ?(sel=(fun x _y -> x)) because we want to be
     able to know whether a selection function was passed, for
     optimization purposes: when sel is the default (expected common
     case), we can take a short path in the (ur == vr) case. *)
  let (ur, ({data=x; rank=xr} as uhead)) = find ur in
  let (vr, ({data=y; rank=yr} as vhead)) = find vr in
  if ur == vr then begin
    match sel with
      | None -> ()
      | Some sel ->
        (* even when ur and vr are the same reference, we need to apply
           the selection function, as [sel x x] may be different from [x].
           
           For example, [unite ~sel:(fun _ _ -> v) r r] would fail
           to set the content of [r] to [v] otherwise. *)
        uhead.data <- sel x x;
  end 
  else
    (* in this branch we can set (vr := Ptr ur) or (ur := Ptr vr)
       because we know that ur != vr, so we will not create a cycle *)
    let head =
      if xr = yr then begin
        uhead.rank <- xr + 1;
        vr := Ptr ur;
        uhead
      end else if xr < yr then begin
        vr := Ptr ur;
        uhead
      end else begin
        ur := Ptr vr;
        vhead
      end in
    match sel with
      | None -> (* in the default case, pick x over y *)
        if xr > yr (* if vhead was chosen above, we need to set x *)
        then head.data <- x
      | Some sel ->
        head.data <- sel x y

let print elepr out ur =
  let (_ur, head) = find ur in
  BatInnerIO.nwrite out "uref " ;
  elepr out head.data

let t_printer elepr paren out ur =
  if paren then BatInnerIO.nwrite out "(" ;
  print (elepr false) out ur ;
  if paren then BatInnerIO.nwrite out ")"

let uref_printer = t_printer
