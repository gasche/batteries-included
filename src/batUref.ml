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
  | Ranked of 'a * int
  | Ptr of 'a uref
and 'a uref = 'a uref_contents ref

type 'a t = 'a uref

(* a richer interface for [find]: the simple [find] above only returns
   the reference to the head, forcing an assert-falty programming style:
   
     match !(find ur) with
       | Ptr _ -> assert false
       | Ranked (x, r) -> ...

   With this [find_triple] version we return [x] and [r] directly, so
   that no conditional is necessary, plus the reference to the head to
   allow further mutation.
*)
let rec find_triple curr = match !curr with
  | Ranked (x, r) -> (curr, x, r)
  | Ptr next -> find_back curr next
and find_back back curr =
  match !curr with
    | Ranked (x, r) -> (curr, x, r)
    | Ptr next ->
      let result = find_back curr next in
      (* we could write equivalently:
           let (head, _, _) = find_back curr next in
           back := Ptr head
         but !curr is (Ptr head) already, so this avoids allocation. *)
      back := !curr;
      result

let uref x = ref (Ranked (x, 0))

let uget ur =
  match !ur with
    | Ranked (x, _) -> x
    | Ptr p ->
      let (_a, x, r) = find_back ur p in
      x

let uset ur y =
  match !ur with
    | Ranked (_x, r) -> ur := Ranked (y, r)
    | Ptr p ->
      let (a, _x, r) = find_back ur p in
      a := Ranked (y, r)

let equal ur vr =
  let (ur, _, _) = find_triple ur in
  let (vr, _, _) = find_triple vr in
  ur == vr

let unite ?sel ur vr =
  (* we use ?sel instead of ?(sel=(fun x _y -> x)) because we want to be
     able to know whether a selection function was passed, for
     optimization purposes: when sel is the default (expected common
     case), we can take a short path in the (ur == vr) case. *)
  let (ur, x, xr) = find_triple ur in
  let (vr, y, yr) = find_triple vr in
  if ur == vr then begin
    match sel with
      | None -> ()
      | Some sel ->
        (* even when ur and vr are the same reference, we need to apply
           the selection function, as [sel x x] may be different from [x].
           
           For example, [unite ~sel:(fun _ _ -> v) r r] would fail
           to set the content of [r] to [v] otherwise. *)
        ur := Ranked(sel x x, xr)
  end 
  else
    let z = match sel with
      | None -> x (* in the default case, pick x over y *)
      | Some sel -> sel x y in
    if xr = yr then begin
      ur := Ranked (z, xr + 1) ;
      vr := Ptr ur
    end else if xr < yr then begin
      ur := Ranked (z, xr) ;
      vr := Ptr ur
    end else begin
      vr := Ranked (z, yr) ;
      ur := Ptr vr
    end

let print elepr out ur =
  let (_ur, x, _r) = find_triple ur in
  BatInnerIO.nwrite out "uref " ;
  elepr out x

let t_printer elepr paren out ur =
  if paren then BatInnerIO.nwrite out "(" ;
  print (elepr false) out ur ;
  if paren then BatInnerIO.nwrite out ")"

let uref_printer = t_printer
