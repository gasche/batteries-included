(* cd .. && ocamlbuild benchsuite/test_maps.native && _build/benchsuite/test_maps.native *)

(* The purpose of this test is to compare different implementation of
   the Map associative data structure. *)

let total_length = 500_000

let (-|) = BatStd.(-|)
let (<|) = BatStd.(<|)

module MapBench (M : sig val input_length : int end) = struct
  let input_length = M.input_length

  let random_key () = Random.int input_length
  let random_value () = Random.int input_length

  let random_inputs random_elt () =
    BatList.init input_length (fun _ -> random_elt ())

  let make_samples input tests () =
    Benchmark.throughputN 1
      (List.map (fun (name, test) -> name, test, input) tests)

  (* we don't use BatInt to ensure that the same comparison function
     is used (PMap use Pervasives.compare by default), in order to
     have comparable performance results. *)
  module StdMap = BatMap.Make(struct type t = int let compare = compare end)
  module Splay = BatSplay.Map(struct type t = int let compare = compare end)
  module PMap = BatPMap

  let same_elts emap1 emap2 =
    BatList.of_enum emap1 = BatList.of_enum emap2

  let same_elts_st_pm stdmap pmap =
    same_elts (StdMap.enum stdmap) (PMap.enum pmap)
  let same_elts_st_sp stdmap splay =
    same_elts (StdMap.enum stdmap) (Splay.enum splay)

  (* A benchmark for key insertion *)
  let create_std_map input =
    List.fold_left
      (fun t (k, v) -> StdMap.add k v t)
      StdMap.empty input
    
  let create_poly_map input =
    List.fold_left
      (fun t (k, v) -> PMap.add k v t)
      PMap.empty input

  let create_splay_map input =
    List.fold_left
      (fun t (k, v) -> Splay.add k v t)
      Splay.empty input

  let create_input =
    let keys = random_inputs random_key () in
    let values = random_inputs random_value () in
    BatList.combine keys values

  let std_created_map = create_std_map create_input
  let poly_created_map = create_poly_map create_input
  let splay_created_map = create_splay_map create_input

  let () =
    assert (same_elts_st_pm std_created_map poly_created_map)

  let samples_create = make_samples create_input [
    "stdmap create", ignore -| create_std_map;
    "pmap create", ignore -| create_poly_map;
    "splay create", ignore -| create_splay_map;
  ]

  (* A benchmark for fast import *)
  let import_std_map input =
    StdMap.of_enum (BatList.enum input)
    
  let import_poly_map input =
    PMap.of_enum (BatList.enum input)

  let import_splay_map input =
    Splay.of_enum (BatList.enum input)

  let import_input = create_input

  let () =
    let std_imported_map = import_std_map import_input in
    assert (same_elts_st_pm std_imported_map poly_created_map);
    let poly_imported_map = import_poly_map import_input in
    assert (same_elts_st_pm std_created_map poly_imported_map);
    let splay_imported_map = import_splay_map import_input in
    assert (same_elts_st_sp std_created_map splay_imported_map);
    ()

  let samples_import = make_samples import_input [
    "stdmap import", ignore -| import_std_map;
    "pmap import", ignore -| import_poly_map;
    "splay import", ignore -| import_splay_map;
  ]

  (* A benchmark for key lookup *)
  let lookup_input =
    random_inputs random_key ()

  let lookup_std_map input =
    List.iter
      (fun k -> ignore (StdMap.mem k std_created_map))
      input

  let lookup_poly_map input =
    List.iter
      (fun k -> ignore (PMap.mem k poly_created_map))
      input

  let lookup_splay_map input =
    List.iter
      (fun k -> ignore (Splay.mem k splay_created_map))
      input

  let samples_lookup = make_samples lookup_input [
    "stdmap lookup", lookup_std_map;
    "pmap lookup", lookup_poly_map;
    "splay lookup", lookup_splay_map;
  ]

  (* some benchmarks for key lookups with frequent repetitions,
     to advantage splay rebalancings *)
  let gauss_biased_input =
    let binomial2 n =
      let rec test acc = function
        | 0 -> acc
        | n ->
          let success = if Random.bool () then 0 else 1 in
          test (acc + success) (n - 1)
      in test 0 n in
    let sqrt_input_length =
      1 + int_of_float (sqrt (float_of_int input_length)) in
    random_inputs (fun () -> binomial2 sqrt_input_length) ()

  let decay_biased_input =
    let input = Array.make input_length 0 in
    let flip p = (Random.int p = 0) in
    let rec exp_decay p = if flip p then 0 else 1 + exp_decay p in
    (* the choice of fresh_prob and decay_param are arbitrary; the
       smaller decay_param, the bigger fresh_prob, the better for splays *)
    let decay_param = 2 in
    let fresh_prob = 10 in
    for i = 0 to input_length - 1 do
      let d = exp_decay decay_param in
      input.(i) <-
        if d > i || flip fresh_prob then random_key ()
        else input.(i - d)
    done;
    Array.to_list input

  let samples_gauss_biased_lookup = make_samples gauss_biased_input [
    "map gauss-biased lookup", lookup_std_map;
    "splay gauss-biased lookup", lookup_splay_map;
  ]

  let samples_decay_biased_lookup = make_samples decay_biased_input [
    "map decay-biased lookup", lookup_std_map;
    "splay decay-biased lookup", lookup_splay_map;
  ]


  (* A benchmark for key removal *)
  let remove_input =
    random_inputs random_key ()

  let remove_std_map input =
    List.fold_left
      (fun t k -> StdMap.remove k t)
      std_created_map input

  let remove_poly_map input =
    List.fold_left
      (fun t k -> PMap.remove k t)
      poly_created_map input

  let remove_splay_map input =
    List.fold_left
      (fun t k -> Splay.remove k t)
      splay_created_map input

  let () =
    let std_output = remove_std_map remove_input in
    assert (same_elts_st_pm std_output
              (remove_poly_map remove_input));
    assert (same_elts_st_sp std_output
              (remove_splay_map remove_input));
    ()

  let samples_remove = make_samples remove_input [
    "stdmap remove", ignore -| remove_std_map;
    "pmap remove", ignore -| remove_poly_map;
    "splay remove", ignore -| remove_splay_map;
  ]

  (* A benchmark for merging *)
  let random_pairlist () =
    BatList.combine
      (random_inputs random_key ())
      (random_inputs random_value ())

  let p1 = random_pairlist ()
  let p2 = random_pairlist ()

  let merge_fun k a b =
    if k mod 2 = 0 then None else Some ()

  let merge_std_map =
    let m1 = StdMap.of_enum (BatList.enum p1) in
    let m2 = StdMap.of_enum (BatList.enum p2) in
    fun () ->
      StdMap.merge merge_fun m1 m2

  let merge_poly_map_equal, merge_poly_map_equiv, merge_poly_map_ineq, merge_unsafe_poly_map =
    let m1 = PMap.of_enum (BatList.enum p1) in
    let m2_equal = PMap.of_enum (BatList.enum p2) in
    let m2_equiv =
      PMap.of_enum ~cmp:BatInt.compare (BatList.enum p2) in
    let m2_ineq =
      let cmp x y = BatInt.compare y x in
      PMap.of_enum ~cmp (BatList.enum p2) in
    (fun () -> PMap.merge merge_fun m1 m2_equal),
    (fun () -> PMap.merge merge_fun m1 m2_equiv),
    (fun () -> PMap.merge merge_fun m1 m2_ineq),
    (fun () -> PMap.merge_unsafe merge_fun m1 m2_equal)

  let merge_splay_map =
    let m1 = Splay.of_enum (BatList.enum p1) in
    let m2 = Splay.of_enum (BatList.enum p2) in
    fun () ->
      Splay.merge merge_fun m1 m2

  let () =
    let test impl_merge =
      same_elts_st_pm (merge_std_map ()) (impl_merge ()) in
    assert (test merge_poly_map_equal);
    assert (test merge_poly_map_equiv);
    assert (test merge_poly_map_ineq);
    assert (test merge_unsafe_poly_map);
    assert (same_elts_st_sp (merge_std_map ()) (merge_splay_map ()));
    ()

  let samples_merge = make_samples () [
    "stdmap merge", ignore -| merge_std_map;
    "splay merge", ignore -| merge_splay_map;
    "pmap merge (<>)", ignore -| merge_poly_map_ineq;
    "pmap merge (~~)", ignore -| merge_poly_map_equiv;
    "pmap merge (==)", ignore -| merge_poly_map_equal;
    "pmap merge_unsafe", ignore -| merge_unsafe_poly_map;
  ]

  (* compare fold-based and merge-based union, diff, intersect *)
  let pmap_union (m1, m2) = PMap.union m1 m2
  let fold_union (m1, m2) =
    PMap.foldi PMap.add m1 m2
  let merge_union (m1, m2) =
    let merge_fun k a b = if a <> None then a else b in
    PMap.merge merge_fun m1 m2
  let merge_unsafe_union (m1, m2) =
    let merge_fun k a b = if a <> None then a else b in
    PMap.merge_unsafe merge_fun m1 m2
    
  let union_input =
    let m1 = PMap.of_enum (BatList.enum p1) in
    let m2 = PMap.of_enum (BatList.enum p2) in
    m1, m2

  let () =
    let li m = BatList.of_enum (PMap.enum m) in
    let test impl_union =
      li (pmap_union union_input) = li (impl_union union_input) in
    assert (test fold_union);
    assert (test merge_union);
    assert (test merge_unsafe_union);
    ()

  let samples_union = make_samples union_input [
    "pmap union", ignore -| pmap_union;
    "fold-based union", ignore -| fold_union;
    "merge-based union", ignore -| merge_union;
    "merge-unsafe union", ignore -| merge_unsafe_union;
  ]

  let pmap_diff (m1, m2) =
    PMap.diff m1 m2
  let fold_diff (m1, m2) =
    PMap.foldi (fun k _ acc -> PMap.remove k acc) m2 m1
  let merge_diff (m1, m2) =
    let merge_fun k a b = if b <> None then None else a in
    PMap.merge merge_fun m1 m2

  let diff_input =
    let m1 = PMap.of_enum (BatList.enum p1) in
    let m2 = PMap.of_enum (BatList.enum p2) in
    m1, m2

  let () =
    let li m = BatList.of_enum (PMap.enum m) in
    let test impl_diff =
      li (pmap_diff diff_input) = li (impl_diff diff_input) in
    assert (test fold_diff);
    assert (test merge_diff);
    ()

  let samples_diff = make_samples diff_input [
    "pmap diff", ignore -| pmap_diff;
    "fold-based diff", ignore -| fold_diff;
    "merge-based diff", ignore -| merge_diff;
  ]

  let pmap_intersect f (m1, m2) =
    PMap.intersect f m1 m2

  let filter_intersect f (m1, m2) =
    let filter_fun k v1 =
      match
        try Some (PMap.find k m2)
        with Not_found -> None
      with
        | None -> None
        | Some v2 -> Some (f v1 v2) in
    PMap.filter_map filter_fun m1

  let merge_intersect f (m1, m2) =
    let merge_fun k a b =
      match a, b with
        | Some v1, Some v2 -> Some (f v1 v2)
        | None, _ | _, None -> None in
    PMap.merge merge_fun m1 m2

  let intersect_input =
    let m1 = PMap.of_enum (BatList.enum p1) in
    let m2 = PMap.of_enum (BatList.enum p2) in
    m1, m2

  let () =
    let li m = BatList.of_enum (PMap.enum m) in
    let test impl_intersect =
      li (pmap_intersect (-) intersect_input)
      = li (impl_intersect (-) intersect_input) in
    assert (test filter_intersect);
    assert (test merge_intersect);
    ()

  let samples_intersect = make_samples intersect_input [
    "pmap intersect", ignore -| pmap_intersect (-);
    "filter-based intersect", ignore -| filter_intersect (-);
    "merge-based intersect", ignore -| merge_intersect (-);
  ]

  let () =
    let create = samples_create () in
    let import = samples_import () in
    let lookup = samples_lookup () in
    let gauss_biased_lookup = samples_gauss_biased_lookup () in
    let decay_biased_lookup = samples_decay_biased_lookup () in
    let remove = samples_remove () in
    let merge = samples_merge () in
    let union = samples_union () in
    let diff = samples_diff () in
    let intersect = samples_intersect () in
    List.iter
      (print_newline -| Benchmark.tabulate)
      [
        create;
        import;
        lookup;
        gauss_biased_lookup;
        decay_biased_lookup;
        remove;
        merge;
        union;
        diff;
        intersect;
      ]
end

let big_length = 100_000
let small_length = 500

let () =
  Printf.printf "Test with small maps (length = %d)\n%!" small_length;
  let () =
    let module M = MapBench(struct let input_length = small_length end) in
    () in

  print_newline ();
  print_newline ();
  
  Printf.printf "Test with big maps (length = %d)\n%!" big_length;
  let () =
    let module M = MapBench(struct let input_length = big_length end) in
    () in


  ()

  
