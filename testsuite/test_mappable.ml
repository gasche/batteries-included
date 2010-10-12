open OUnit

(* The purpose of this test file is to test properties that should be
   verified by all instances of a given interface, here
   BatInterfaces.Mappable.

   It is very minimal for now : it only check for one property, and
   only a few of the Mappable modules (it is actually a regression
   test for a very specific bug). New properties will be added, and
   hopefully they will be verified against all Mappable modules.
*)

module TestMappable
  (M : sig
    include BatEnum.Enumerable

    include BatInterfaces.Mappable
    with type 'a mappable = 'a enumerable
  end)
  =
struct
  (* The property we test is that the order in which the [map]
     function traverse the structure (applying a given function on
     each element) is the same as the order of the [enum] function of
     the module (the order in which the elements are produced in the
     enumeration).
     
     The test is eager as it supposes that map will force evaluation
     of the function 'f'. For lazy data structures such as Seq, we need
     another test.
  *)
  let test_eager_map_evaluation_order printer t =
    let elems_in_enum_order = BatList.of_enum (M.enum t) in
    let elems_in_map_order =
      let li = ref [] in
      ignore (M.map (fun x -> li := x :: !li) t);
      List.rev !li in
    assert_equal ~printer:(BatList.sprint printer)
      elems_in_enum_order
      elems_in_map_order
end

let test_list_mappable () =
  let module T = TestMappable(BatList) in
  T.test_eager_map_evaluation_order BatInt.print [1; 2; 3]

let test_array_mappable () =
  let module T = TestMappable(BatArray) in
  T.test_eager_map_evaluation_order BatInt.print [|1; 2; 3|]

let test_pair_mappable () =
  let module T = TestMappable(BatPair) in
  T.test_eager_map_evaluation_order BatInt.print (1, 2)

let test_dllist_mappable () =
  let module L = BatDllist in
  let module T = TestMappable(L) in
  let u = L.create 1 in
  L.add u 2;
  L.add u 3;
  T.test_eager_map_evaluation_order BatInt.print u

let test_reflist_mappable () =
  let module L = BatRefList in
  let module T = TestMappable(L) in
  let u = L.empty () in
  L.add u 1;
  L.add u 2;
  L.add u 3;
  T.test_eager_map_evaluation_order BatInt.print u

let test_option_mappable () =
  let module T = TestMappable(BatOption) in
  T.test_eager_map_evaluation_order BatInt.print None;
  T.test_eager_map_evaluation_order BatInt.print (Some 1)

let test_pset_mappable () =
  let module S = BatPSet in
  let module T = TestMappable(S) in
  T.test_eager_map_evaluation_order BatInt.print
    (S.add 1 (S.add 3 (S.add 2 (S.create BatInt.compare))))

let tests = "Mappable" >::: [
  "Array" >:: test_array_mappable;
  "List" >:: test_list_mappable;
  "Pair" >:: test_pair_mappable;
  "Dllist" >:: test_dllist_mappable;
  "RefList" >:: test_reflist_mappable;
  "Option" >:: test_option_mappable;
  "PSet" >:: test_pset_mappable;
]
