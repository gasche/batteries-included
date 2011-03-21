open OUnit
open BatRandom
open BatPervasives

let print_enum out enum =
  BatEnum.print (fun out (c, _) -> BatPrintf.fprintf out "%d" c) out enum

let assert_equal_enums enum_1 enum_2 =
  match BatEnum.compare compare (enum_1 ()) (enum_2 ()) with
    | 0 -> (* pass *) ()
    | _ ->
        assert_failure
          (BatPrintf.sprintf2 "Expected %a, got %a"
             print_enum (enum_1 ()) print_enum (enum_2 ()))

let assert_equal_maps map_1 map_2 =
  let enum_1 () = BatPMap.enum map_1 in
  let enum_2 () = BatPMap.enum map_2 in
  assert_equal_enums enum_1 enum_2

let test_traversal_order () =
  let init = State.make [|0|] in
  let keys = BatEnum.take 50 (State.enum_int init 10) in
  let map  = BatPMap.of_enum (BatEnum.map (fun x -> (x,x)) keys) in
  let enum_1 () = BatPMap.enum map
  and enum_2 () =
    let list = BatRefList.empty () in
      BatPMap.iter (fun k v -> BatRefList.push list (k, v)) map;
      BatRefList.backwards list
  in
    match BatEnum.compare compare (enum_1 ()) (enum_2 ()) with
      | 0 -> (* pass *) ()
      | _ ->
          assert_failure
            (BatPrintf.sprintf2 "Expected %a, got %a"
               print_enum (enum_1 ()) print_enum (enum_2 ()))

let gen_map state bound count =
  let keys = BatEnum.take count (State.enum_int state bound) in
  BatPMap.of_enum (BatEnum.map (fun x -> (x,x)) keys)

let test_split () =
  let do_test map v =
    let m1, vo, m2 = BatPMap.split v map in
    assert_equal_maps m1 (BatPMap.filteri (fun k _ -> k < v) map);
    assert_equal_maps m2 (BatPMap.filteri (fun k _ -> k > v) map);
    assert_equal vo (if BatPMap.mem v map then Some v else None)
  in
  let init = State.make [|0|] in
  for i = 0 to 50 do
    let bound = 40 in
    let count = i * 5 in
    do_test (gen_map init bound count) (State.int init bound)
  done

let tests = "PMap" >::: [
  "traversal order iter vs. enum" >:: test_traversal_order;
  "split" >:: test_split;
]
