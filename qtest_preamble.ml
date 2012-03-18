(* This file is part of Batteries 'qtest' usage; it will be included
   at the top of the generated test runner, and is therefore a good
   location to add functions that would be convenient to write tests
   but have not yet found their place into Batteries proper. 

   The content of this file is included just after qtest's hard-code
   preamble, which is the following:
   
     open OUnit;;
     module Q = Quickcheck;;
*)

let ( ==> ) = Q.( ==> )

let () = Random.self_init()

open Batteries

