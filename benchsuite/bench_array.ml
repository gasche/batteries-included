(* cd .. && ocamlbuild benchsuite/bench_array.native && _build/benchsuite/bench_array.native *)
open Batteries_uni
open Std

let length = Array.length
let init = Array.init

(* Performances tests for the `Array.filter p xs` function :
     ('a -> bool) -> 'a array -> 'a array

   The implementation needs to do following:

   1. iterate on the input array, remembering which elements should be
      kept (in a temporary data structure), and counting the number of
      kept elements

   2. create an output array of the right size (the number of kept
      elements) and use the temporary data structure to decide which
      elements to add to the output

   Before this performance measurements work, the temporary data
   structure used where associative arrays of boolean (temp.(i) iff
   p (xs.(i)). An array is a natural choice here, but a bitset can
   reduce the temporary memory consumption, and therefore perform
   better on big arrays (despite the slightly worse get/set
   performance of bitsets).

   I added the following implementation choices:

   - Instead of using an associative structure of length proportional
     to the original input, what if the temporary structure only
     remembers kept elements? It should be faster in cases where only
     a few of the input elements are kept.

     I therefore experimented with adding the kept elements to
     a mutable list and converting the list to an array. It turns out
     that it is by far the fastest solution for small inputs (input
     length 100), but it gets slower as long as the *result* list is
     big: when keeping 9.10^5 elements out of 10^5, bitset
     implementation is 150% faster.

     Instead of using lists, a good idea is to use DynArray : it's an
     exponentially resized array that is faster than lists for big
     structures, due to better locality. It's also reasonably fast to
     turn into an array.

     DynArray implementation consistently outperform lists on big
     output sizes. When keeping 9 over 10 elements of an array of
     length 50_000, a DynArray implementation is 29% faster than the
     list one. When keeping 1 over 2 elements of an array of size
     1_000_000, it's 167% faster.

     Resulting from that experience I added a 'hybrid' version that
     used the list implementation for inputs of size < 1000, then the
     dynarray implementation. For uses when we know that the output
     will be small, we should still use the list version. Perhaps the
     list and dynarray versions could be exposed separately for the
     power user.
   
     The first implementation of DynArray used the
     (DynArray.create ()) method, that starts the array length at 0,
     incurring numerous redimensionning. I changed to DynArray.make,
     with the starting length set at (n / 8), to have the same memory
     usage that bitsets. This consistently improved performances for
     large results : instead of being 5% to 19% slower than
     filter_bitset in the 9/10 50_000 and 9/10 1000_000 cases, it's
     now faster in all cases.

   - count filter : This particular implementation relies on the
     specific assumption that the predicate is a constant function
     which returns very fast. The idea is that instead of keeping the
     filter results in an intermediate data structure, we can just
     throw them away and compute only their number, allocate the right
     array, then build the output in a second pass on the input.

     This function has an important drawback : it breaks the
     assumption that the filtering function is only called once per
     input item. It would therefore be unsafe for some side-effecting
     predicates. If exposed, this should be emphasized.
   
     This function makes two pass on the whole input, contrary to the
     list/dynarray versions which only iter once on the input, and
     once on the temporary data structure. It is therefore not very
     interesting in cases where the output result is much smaller than
     the input. In the 9/10 50_000 case, it performs 50% better than
     bitset or dynarray, 61% better than bitset in the 9/10 1000_000
     case, but 70% worse than dynarray in the 1/20 50_000 case.

   I think filter_hybrid should be used as the generalist filter
   implementation, as it respects the unicity and ordering
   requirements on the predicate function and beats filter_bitset in
   all cases. filter_count, that breaks those requirements, could be
   exposed to the advanced user.
*)

let test_filter () = Printf.printf "test use of BatSet over a simple
   array as an intermediate Array.filter structure";
  
  let filter_bitset_old p xs =
    let n = length xs in
    (* Use a bitset to store which elements will be in the final array. *)
    let bs = BatBitSet.create n in
    for i = 0 to n-1 do
      if p xs.(i) then BatBitSet.set bs i
    done;
    (* Allocate the final array and copy elements into it. *)
    let n' = BatBitSet.count bs in
    let j = ref 0 in
    let xs' = init n'
      (fun _ ->
        (* Find the next set bit in the BitSet. *)
        while not (BatBitSet.is_set bs !j) do incr j done;
        let r = xs.(!j) in
        incr j;
        r) in
    xs'
  in

  (* It turns out that the above implementation using bitset calls
     BitSet.count, which is wasteful as direct reference counting is
     much simpler. The following version consistently beats
     filter_bitset, and is therefore always used in the following
     comparisons. *)
  let filter_bitset p xs =
    let n = length xs in
    let n' = ref 0 in
    (* Use a bitset to store which elements will be in the final array. *)
    let bs = BatBitSet.create n in
    for i = 0 to n-1 do
      if p xs.(i) then begin
        incr n';
        BatBitSet.set bs i
      end
    done;
    (* Allocate the final array and copy elements into it. *)
    let j = ref 0 in
    let xs' = init !n'
      (fun _ ->
        (* Find the next set bit in the BitSet. *)
        while not (BatBitSet.is_set bs !j) do incr j done;
        let r = xs.(!j) in
        incr j;
        r) in
    xs'
  in

  let filter_array p xs =
    let n = length xs in
    (* Use an array to store which elements will be in the final array. *)
    let bs = Array.create n false in
    for i = 0 to n-1 do
      if p xs.(i) then Array.set bs i true
    done;
    (* Allocate the final array and copy elements into it. *)
    let n' = Array.fold_left (fun i p -> i + if p then 1 else 0) 0 bs in
    let j = ref 0 in
    let xs' = init n'
      (fun _ ->
        (* Find the next set bit in the BitSet. *)
        while not (Array.get bs !j) do incr j done;
        let r = xs.(!j) in
        incr j;
        r) in
    xs'
  in

  let filter_to_dynarray ?size p xs =
    let n = length xs in
    let size = match size with
      | None ->
        (* n / 8 is chosen by default as it is the memory used by the BitSet; using
           this instead of no initial size significantly improves
           performances by reducing resizing.  *)
        n / 8
      | Some s -> s in
    let res = DynArray.make size in
    for i = 0 to n-1 do
      let r = xs.(i) in
      if p r then DynArray.add res r 
    done;
    res
  in
  let filter_dynarray p xs =
    DynArray.to_array (filter_to_dynarray p xs)
  in
  let filter_dynarray_small p xs =
    (* equivalent to using DynArray.create () in filter_to_dynarray *)
    DynArray.to_array (filter_to_dynarray ~size:0 p xs)
  in

  let filter_counter p xs =
    let n = length xs in
    (* do not store which elements will be in the array, just count them *)
    let count = ref 0 in
    for i = 0 to n-1 do
      if p xs.(i) then incr count
    done;
    (* Allocate the final array and copy elements into it. *)
    let result = Array.make !count xs.(0) in
    let rec copy i j =
      if i = !count then ()
      else
        let r = xs.(j) in
        let i' =
          if p r
          then (result.(i) <- r; i + 1)
          else i in
        copy i' (j + 1)
    in
    copy 0 0;
    result
  in

  let filter_list p xs =
    let res = ref [] in
    let n' = ref 0 in
    for i = 0 to Array.length xs - 1 do
      let r = xs.(i) in
      if p r then begin
        incr n';
        res := r :: !res
      end
    done;
    let t = Array.make !n' xs.(0) in
    for i = !n' - 1 downto 0 do
      match !res with
        | hd::tl ->
          t.(i) <- hd;
          res := tl
        | _ -> assert false
    done;
    t
  in

  let filter_hybrid p xs =
    let list_limit = 1000 in
    let n = length xs in
    if n < list_limit
    then filter_list p xs
    else filter_dynarray p xs
  in

  let nb_iter = 20 in
  let test length (p, q) =
    (* filter 'p' over 'q' elements of an array of length 'length' *)
    let array = init length (fun k -> k) in
    let f k = k mod q < p in
    fun filter ->
      (* assert (filter f array = filter_array f array); *)
      for i = 1 to nb_iter do
        ignore (filter f array)
      done
  in
  
  let samples length (p, q) =
    let name impl =
      Printf.sprintf "%s filter (%d/%d for %d)" impl p q length in
    let test = test length (p, q) in

    (* ignore all tests, so that you can test selectively by
       commenting lines in the list below, without getting an 'unused
       variable' warning.  *)
    ignore (filter_bitset_old, filter_bitset,
            filter_array,
            filter_list,
            filter_dynarray, filter_dynarray_small,
            filter_hybrid,
            filter_counter
    );
    Benchmark.throughputN ~repeat:1 1
      [
        name "bitset_old", test, filter_bitset_old;
        name "bitset", test, filter_bitset;
        (* name "array", test, filter_array; *)
        (* name "list", test, filter_list; *)
        (* name "dynarray", test, filter_dynarray; *)
        (* name "dynarray_small", test, filter_dynarray_small; *)
        (* name "hybrid", test, filter_hybrid; *)
        (* name "count", test, filter_counter; *)
      ]
  in

  List.iter (print_newline -| Benchmark.tabulate) [
    samples 100 (9,10);
    samples 100 (1,2);
    samples 100 (1,20);
    samples 50_000 (9,10);
    samples 50_000 (1,2);
    samples 50_000 (1,20);
    samples 1_000_000 (9,10);
    samples 1_000_000 (1,2);
    samples 1_000_000 (1,20);
    samples 1_000_000 (1,1000);
  ]

(* Test whether it is worth duplicating code between 'filter' and
   'filteri', when each one could reuse the other implementation by
   just being a simple wrapper over it.

   Note : implementing "filteri" on top of "filter" assumes that
   elements are tested only once, in increasing index order. It is
   therefore more robust to implement "filter" on top of "filteri", by
   just dropping the index.

   The results are pretty clear: for rapidly returning predicate
   functions (over course the overhead becomes insignificant for
   slower predicates), the overhead is small but present, 4% when
   filter is implemented on top of filteri, and 6% in the other
   direction.

   If a lot of complex work goes into an implementation of filteri,
   reusing the above 'filter' subtleties, it definitely makes sense to
   just reuse it in filter instead of rewriting it.
*)

let test_filteri () =
  let filter_specialized p xs =
    let n = length xs in
    (* Use a bitset to store which elements will be in the final array. *)
    let bs = BatBitSet.create n in
    for i = 0 to n-1 do
      if p xs.(i) then BatBitSet.set bs i
    done;
    (* Allocate the final array and copy elements into it. *)
    let n' = BatBitSet.count bs in
    let j = ref 0 in
    let xs' = init n'
      (fun _ ->
        (* Find the next set bit in the BitSet. *)
        while not (BatBitSet.is_set bs !j) do incr j done;
        let r = xs.(!j) in
        incr j;
        r) in
    xs'
  in

  let filteri_specialized p xs =
    let n = length xs in
    (* Use a bitset to store which elements will be in the final array. *)
    let bs = BatBitSet.create n in
    for i = 0 to n-1 do
      if p i xs.(i) then BatBitSet.set bs i
    done;
    (* Allocate the final array and copy elements into it. *)
    let n' = BatBitSet.count bs in
    let j = ref 0 in
    let xs' = init n'
      (fun _ ->
        (* Find the next set bit in the BitSet. *)
        while not (BatBitSet.is_set bs !j) do incr j done;
        let r = xs.(!j) in
        incr j;
        r) in
    xs'
  in

  let filteri_reusing p xs =
    let i = ref (-1) in
    filter_specialized (fun x -> incr i; p !i x) xs
  in

  let filter_reusing p xs =
    filteri_specialized (fun _ x -> p x) xs
  in

  let length = 100_000 in
  let frac = 100 in

  let test_filteri =
    let xs = init length (fun k -> k) in
    fun filteri ->
      ignore (filteri (fun i k -> assert (i = k); k mod frac = 0) xs)
  in

  let test_filter =
    let xs = init length (fun k -> k) in
    fun filter ->
      ignore (filter (fun k -> k mod frac = 0) xs)
  in

  print_newline -| Benchmark.tabulate <| Benchmark.throughputN ~repeat:1 1
      [
        "filteri_specialized", test_filteri, filteri_specialized;
        "filteri_reusing", test_filteri, filteri_reusing
      ];
  print_newline -| Benchmark.tabulate <| Benchmark.throughputN ~repeat:1 1
      [
        "filter_specialized", test_filter, filter_specialized;
        "filter_reusing", test_filter, filter_reusing
      ];
  ()

let () =
  test_filter ();
  test_filteri ();
  ()
