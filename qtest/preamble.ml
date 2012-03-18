(* This is the preamble for qtest, which will be included before the
   code of the inline tests. *)

open OUnit
module Q = Quickcheck
module Exn = Exncheck

let ( ==> ) = Q.( ==> )

let () = Random.self_init()

open Batteries
