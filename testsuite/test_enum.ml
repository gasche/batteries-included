open OUnit
open BatBigarray
open BatPervasives

let array     = [|'1';'2';'3';'4';'5'|]
let array2    = [|[|'1';'2';'3';'4';'5'|];
                  [|'6';'7';'8';'9';'A'|];
                  [|'B';'C';'D';'E';'F'|]|]
let array3    = [|[|[|'1';'2';'3';'4';'5'|];
                    [|'6';'7';'8';'9';'A'|];
                    [|'B';'C';'D';'E';'F'|]|];
                  [|[|'G';'H';'I';'J';'K'|];
                    [|'L';'M';'N';'O';'P'|];
                    [|'Q';'R';'S';'T';'U'|]|]|]
let list      = ['1';'2';'3';'4';'5']
let string    = "12345"
let bigarray1 = Array1.of_array char c_layout array
let bigarray2 = Array2.of_array char c_layout array2
let bigarray3 = Array3.of_array char c_layout array3

let text   = BatText.of_string string

module C =
struct
  type t = char
  let compare x y = Char.code x - Char.code y
end

module S = BatSet.Make(C)
module M = BatMap.Make(C)

let theset = List.fold_right S.add list S.empty
let themap = List.fold_left (fun m c -> M.add c () m) M.empty list

open BatArray
let test_array_enums () =
    let source = array in
    let aeq = assert_equal ~printer:(BatPrintf.sprintf2 "%a" (print BatChar.print)) in
      aeq (of_backwards (enum source)) (of_enum (backwards source));
      aeq source (of_backwards (backwards source));

open BatList
let test_list_enums () =
    let source = list in
    let aeq = assert_equal ~printer:(BatPrintf.sprintf2 "%a" (print BatChar.print)) in
      aeq (of_backwards (enum source)) (of_enum (backwards source));
      aeq source (of_backwards (backwards source));

open BatString
let test_string_enums () =
    let source = string in
    let aeq = assert_equal ~printer:(Printf.sprintf "%S") in
      aeq (of_backwards (enum source)) (of_enum (backwards source));
      aeq source (of_backwards (backwards source));

open S
let test_set_enums () =
    let source = theset in
    let aeq = assert_equal
                ~cmp:(fun s1 s2 -> S.compare s1 s2 = 0)
                ~printer:(BatPrintf.sprintf2 "%a" (print BatChar.print))
    in
      aeq (of_enum (enum source)) (of_enum (backwards source));
      aeq source (of_enum (backwards source));

open M
let test_map_enums () =
    let source = themap in
    let aeq = assert_equal
                ~cmp:(fun m1 m2 -> M.compare (fun _ _ -> 0) m1 m2 = 0)
                ~printer:(BatPrintf.sprintf2 "%a"
                            (print BatChar.print (fun _io _v -> ())))
    in
      aeq (of_enum (enum source)) (of_enum (backwards source));
      aeq source (of_enum (backwards source))

(*
open Ulib.Text
let test_rope_enums () =
    let source = text in
    let aeq = assert_equal ~printer:(BatPrintf.sprintf2 "%a" print) in
      aeq (of_backwards (enum source)) (of_enum (backwards source));
      aeq source (of_backwards (backwards source));

open BatUTF8
let test_UTF8_enums () =
    let source = utf8 in
    let aeq = assert_equal ~printer:(BatPrintf.sprintf2 "%a" print) in
      aeq (of_backwards (enum source)) (of_enum (backwards source));
      aeq source (of_backwards (backwards source));
*)

open BatArray
let test_bigarray_enums () =
    let aeq = assert_equal ~printer:(BatPrintf.sprintf2 "%a" (print BatChar.print)) in
    let enum_flatten x = BatEnum.flatten (BatEnum.map enum x) in
      aeq (of_enum (enum array)) (of_enum (Array1.enum bigarray1));
      aeq
        (enum array2 |> enum_flatten |> of_enum)
        (of_enum (Array2.enum bigarray2));
      aeq
        (enum array3 |> enum_flatten |> enum_flatten |> of_enum)
        (of_enum (Array3.enum bigarray3))

let test_uncombine () =
  let pair_list = [1,2;3,4;5,6;7,8;9,0] in
  let a,b = BatEnum.uncombine (BatList.enum pair_list) in
  let a = BatArray.of_enum a in
  let b = BatArray.of_enum b in
  let c,d = BatEnum.uncombine (BatList.enum pair_list) in
  let d = BatArray.of_enum d in
  let c = BatArray.of_enum c in
  let aeq = assert_equal ~printer:(BatIO.to_string (BatArray.print BatInt.print)) in
  aeq a [|1;3;5;7;9|];
  aeq b [|2;4;6;8;0|];
  aeq a c;
  aeq b d

(* BatEnum.from should not call the user function after No_more_elements was raised. *)
let test_from () =
  let nb_calls = ref 0 in
  let next () =
  	incr nb_calls ;
    if !nb_calls <= 5 then !nb_calls
    else raise BatEnum.No_more_elements in
  let e = BatEnum.merge (fun _ _ -> true) (BatEnum.from next) (1 -- 5) in
  let nb_res = BatEnum.hard_count e in
  assert_equal ~printer:string_of_int 10 nb_res ;
  assert_equal ~printer:string_of_int 6 !nb_calls

(* Same as above for BatEnum.from_while *)
let test_from_while () =
  let nb_calls = ref 0 in
  let next () =
    incr nb_calls ;
    if !nb_calls <= 5 then Some !nb_calls
    else None in
  let e = BatEnum.merge (fun _ _ -> true) (BatEnum.from_while next) (1 -- 5) in
  let nb_res = BatEnum.hard_count e in
  assert_equal ~printer:string_of_int 10 nb_res ;
  assert_equal ~printer:string_of_int 6 !nb_calls

(* Same as above for BatEnum.from_loop *)
let test_from_loop () =
  let nb_calls = ref 0 in
  let next prev =
    incr nb_calls ;
    if !nb_calls <= 5 then prev+1, !nb_calls
    else raise BatEnum.No_more_elements in
  let e = BatEnum.merge (fun _ _ -> true) (BatEnum.from_loop 0 next) (1 -- 5) in
  let nb_res = BatEnum.hard_count e in
  assert_equal ~printer:string_of_int 10 nb_res ;
  assert_equal ~printer:string_of_int 6 !nb_calls


(* A custom test fuzzer for BatEnum.clone

   Here is an informal specification of "clone": if you have
   a *deterministic* function f() which produces a "fresh"
   enumeration, unrelated to any pre-existing mutable state in your
   application or delayed computation, then, for any processing
   function P(e1, e2) that takes two enumerations to produce a result,
   then `P(f(),f())` and `let e1 = f () in P(e1, clone e1)` should
   always produce the same result.

   This "test_clone_resilience" function does exactly that: it uses
   random fuzzing to generate a lot of functions P of the form "do
   some stuff, then pick an element in one of the enumerations at
   random", and test it once with two fresh enumerations, and once for
   a third fresh enumeration and its clone.

   It is parametrized by several aspects, such as what the "do some
   stuff" part means: if you're trying to clone an enumeration reading
   from a data stream, you may want to try reading some of the data
   yourself in the middle of picking elements from the operation. The
   cloning function itself is parametrized, because functions
   producing "enumerations of enumerations" must be cloned at the two
   levels (map clone -| clone). Finally, "strict" is used to map the
   elements of the observed enumerations to strict, comparable and
   marshallable structures (eg. enumerations of integers are turned
   into lists of integers).

   The code will probably evolve as we try to capture more behavior
   (eg. being able to do some stuff of the picked elements themselves)
   and test more functions. We currently were able to re-discover the
   bug in "group" that we already knew about (reported by
   Philippe Veber), and discover an unknown bug in "scan".
*)
type ('a, 'b) clone_test = {
  clone_fun : 'a BatEnum.t -> 'a BatEnum.t;
  effects : (unit -> unit) array;
  nb_iter : int;
  depth : int;
  strict : 'a -> 'b;
  printer : ('b, string) BatIO.printer option;
}

let default = { 
  clone_fun = BatEnum.clone;
  effects = [| |];
  nb_iter = 1000;
  depth = 10;
  strict = (fun x -> x);
  printer = Some BatInt.print;
}

let test_clone_resilience ~input_msg producer test =
  let testing_plan () =
    let rev_tests = ref [] in
    let nb_effects = Array.length test.effects in
    for _i = 1 to test.depth do
      let action =
        let acc = ref [] in
        while nb_effects > 0 && Random.bool () do
          acc := test.effects.(BatRandom.int nb_effects) :: !acc;
        done;
        let acc = List.rev !acc in
        fun () ->
          List.iter (fun eff -> eff ()) acc
      in
      let e1_or_e2 = Random.bool () in
      rev_tests := (action, e1_or_e2) :: !rev_tests
    done;
    List.rev !rev_tests
  in
  let run_plan plan e1 e2 =
    let rev_elems = ref [] in
    let run_test (action, e1_or_e2) =
      let () = action () in
      let elem = BatEnum.get_exn (if e1_or_e2 then e1 else e2) in
      rev_elems := (e1_or_e2, elem) :: !rev_elems
    in
    let reached_end =
      try List.iter run_test plan; false
      with BatEnum.No_more_elements -> true
    in
    (List.rev_map (BatTuple.Tuple2.map2 test.strict) !rev_elems, reached_end)
  in    
  for i = 1 to test.nb_iter do
    let plan = testing_plan () in
    let (elems1, end1) =
      let e1 = producer () in
      let e2 = producer () in
      run_plan plan e1 e2 in
    let (elems2, end2) =
      let e1 = producer () in
      let e2 = test.clone_fun e1 in
      let switch = BatRandom.bool () in
      let e1, e2 = if switch then e2, e1 else e1, e2 in
      run_plan plan e1 e2 in
    let printer = match test.printer with
      | None -> None
      | Some p ->
        Some (BatIO.to_string
                (BatList.print
                   (BatTuple.Tuple2.print BatBool.print p)))
    in 
    let msg = match input_msg with
      | None -> None
      | Some inp -> Some (Printf.sprintf "input (%d): %s" i inp) in
    assert_equal ?msg ?printer elems1 elems2;
    assert_equal end1 end2;
  done

(* common input values *)
let input =
  Array.init 10 (fun _ -> Random.int 10)
let input_msg =
  Some (BatIO.to_string (BatArray.print BatInt.print) input)

let test_clone_scan () =
  let enum () = BatEnum.scan (+) (BatArray.enum input) in
  test_clone_resilience ~input_msg enum default

let test_clone_take () =
  let enum () = BatEnum.take 3 (BatArray.enum input) in
  test_clone_resilience ~input_msg enum default

let test_clone_skip () =
  let enum () = BatEnum.skip 3 (BatArray.enum input) in
  test_clone_resilience ~input_msg enum default

let test_clone_take_while () =
  let enum () = BatEnum.take_while (fun n -> n < 5) (BatArray.enum input) in
  test_clone_resilience ~input_msg enum default

let test_clone_drop_while () =
  let enum () = BatEnum.drop_while (fun n -> n < 5) (BatArray.enum input) in
  test_clone_resilience ~input_msg enum default

(* tests for [span] and [break] are missing, because I'm not sure how
   to test them well *)

let test_clone_group () =
  let enum () = BatEnum.group (fun n -> n mod 2) (BatArray.enum input) in
  test_clone_resilience ~input_msg enum { default with
    clone_fun = BatEnum.(map clone -| clone);
    strict = BatList.of_enum;
    printer = Some (BatList.print BatInt.print);
  }

let test_clone_clump () =
  let input = Array.init 100 (fun _ -> Random.int 10) in
  let enum () =
    let acc = ref 0 in
    BatEnum.clump 9
      (fun n -> acc := !acc + n)
      (fun () -> let v = !acc in acc := 0; v)
      (BatArray.enum input) in
  test_clone_resilience enum default 

let tests = "BatEnum" >::: [
  "Array" >:: test_array_enums;
  "List" >:: test_list_enums;
  "String" >:: test_string_enums;
  (*  "Rope" >:: test_rope_enums;
      "UTF8" >:: test_UTF8_enums; *)
  "bigarray" >:: test_bigarray_enums;
  "Set" >:: test_set_enums;
  "uncombine" >:: test_uncombine;
  "from" >:: test_from;
  "from_while" >:: test_from_while;
  "from_loop" >:: test_from_loop;
  "clone_scan" >:: test_clone_scan;
  "clone_take" >:: test_clone_take;
  "clone_skip" >:: test_clone_skip;
  "clone_take_while" >:: test_clone_take_while;
  "clone_drop_while" >:: test_clone_drop_while;
  "clone_group" >:: test_clone_group;
]
