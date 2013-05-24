(* cd .. && ocamlbuild benchsuite/bench_string.native && _build/benchsuite/bench_string.native *)

let replace_string_simple ~str ~sub ~by =
  (* opening BatString locally here shadows 'sub'; sometimes life sucks... *)
  try
    let i = BatString.find str sub in
    (true, (BatString.slice ~last:i str) ^ by ^
           (BatString.slice ~first:(i + String.length sub) str))
  with
    Not_found -> (false, String.copy str)

let replace_substring_simple ~str ~sub ~by =
  try
    let i = BatString.find str sub in
    (true,
     BatSubstring.concat [
       BatSubstring.unsafe_substring str 0 i;
       BatSubstring.of_string by;
       let k = i + String.length sub in
         BatSubstring.unsafe_substring str k (String.length str - k);
     ])
  with
    Not_found -> (false, String.copy str)
  
let replace_optimized ~str ~sub ~by =
   try
     let subpos = BatString.find str sub in
     let strlen = BatString.length str in
     let sublen = BatString.length sub in
     let bylen  = BatString.length by in
     let newstr = BatString.create (strlen - sublen + bylen) in
     BatString.unsafe_blit str 0 newstr 0 subpos ;
     BatString.unsafe_blit by 0 newstr subpos bylen ;
     BatString.unsafe_blit str (subpos + sublen) newstr (subpos + bylen) (strlen - subpos - sublen) ;
     (true, newstr)
   with Not_found ->  (* find failed *)
     (false, str)  

let pattern = "aa"
let replace = "foo"

let short_string = "bar aaa bar"
let long_string = String.concat " " [String.make 5_000 'b'; "aaa"; String.make 5_000 'c']


let replace_bench name input_string =
  let test_replace f niters =
    for j = 1 to niters do
      ignore (f ~str:input_string ~sub:pattern ~by:replace);
    done
  in
  Bench.bench_n [
    name ^ ":string simple", test_replace replace_string_simple;
    name ^ ":substring simple", test_replace replace_substring_simple;
    name ^ ":optimized", test_replace replace_optimized;
  ]

(* note that this function does not have the same semantics as the
   other, as it replaces from right to left instead of the
   (probably expected) left to right; this corresponds to what is
   currently specified for nreplace. *)
let nreplace_string_simple ~str ~sub ~by =
  let parts = BatString.nsplit str ~by:sub in
  String.concat by parts

(* should be BatSubstring.nsplit *)
let nsplit str pat =
  let pat_len = String.length pat in
  let rec loop pos rev_subs =
    let next_pos =
      try BatString.find_from str pos pat
      with Not_found -> -1
    in
    if next_pos = -1 then
      (BatSubstring.extract str pos None :: rev_subs)
    else
      let sub = BatSubstring.unsafe_substring str pos (next_pos - pos) in
      loop (next_pos + pat_len) (sub :: rev_subs)
  in
  List.rev (loop 0 [])

(* should be BatSubstring.concat, with a separator argument *)
let concat_optimized ~sep ssl =
  let sep_len = String.length sep in
  (* use of Obj.magic is unfortunate here, but it would not be present
     if this function was implemented inside BatSubstring. Another
     option would be to make BatSubstring.t a [private (string * int
     * int)] and use a case here, but I'm not sure it's wise to expose
     the representation publicly -- we may want to change, say, from
     (string * start_pos * len) to (string * start_pos * end_pos). *)
  let ssl : (string * int * int) list = Obj.magic (ssl : BatSubstring.t list) in
  match ssl with
    | [] -> ""
    | (s,o,len)::tl ->
      let total_len =
        let rec count acc = function
          | [] -> acc
          | (_,_,l)::tl -> count (acc + sep_len + l) tl
        in count len tl
      in
      let item = String.create total_len in
      String.unsafe_blit s o item 0 len;
      let pos = ref len in
      let rec loop = function
        | [] -> ()
        | (s,o,len)::tl ->
          String.unsafe_blit sep 0 item !pos sep_len;
          pos := !pos + sep_len;
          String.unsafe_blit s o item !pos len;
          pos := !pos + len;
          loop tl;
      in loop tl;
      item

(* should be BatSubstring.concat, with a separator argument *)
let concat_simple ~sep ssl =  
  let sep_len = String.length sep in
  (* see comment above about Obj.magic *)
  let ssl : (string * int * int) list = Obj.magic (ssl : BatSubstring.t list) in
  match ssl with
    | [] -> ""
    | (s,o,len)::tl ->
      let total_len = List.fold_left (fun acc (_,_,l) -> acc+sep_len+l) len tl in
      let item = String.create total_len in
      String.unsafe_blit s o item 0 len;
      let pos = ref len in
      let write (s,o,len) =
        String.unsafe_blit sep 0 item !pos sep_len;
        pos := !pos + sep_len;
        String.unsafe_blit s o item !pos len;
        pos := !pos + len;
      in
      List.iter write tl;
      item

let nreplace_substring_simple ~str ~sub ~by =
  concat_simple ~sep:by (nsplit str sub)

let nreplace_substring_optimized ~str ~sub ~by =
  concat_optimized ~sep:by (nsplit str sub)

let nreplace_optimized ~str ~sub ~by =
  if sub = "" then invalid_arg "nreplace: cannot replace all empty substrings" ;
  let find_sub pos =
    try BatString.find_from str pos sub
    with Not_found -> -1 in  
   (* allows loop to be tail recursive *)
   let sublen = BatString.length sub in
   let strlen = BatString.length str in
   let buffer = Buffer.create strlen in
   let rec loop curpos =
     if curpos = strlen then
       Buffer.contents buffer
     else
       let subpos = find_sub curpos in
       if subpos = -1 then
         ( Buffer.add_substring buffer str curpos (strlen - curpos) ;
           Buffer.contents buffer )
       else
         ( Buffer.add_substring buffer str curpos (subpos - curpos) ;
           Buffer.add_string buffer by ;
           loop (subpos + sublen) )
   in
   loop 0

let pattern = "aa"
let replace = "foo"

let short_string = "bar aaa bar aaa bar"
let long_string = BatString.concat "" (BatList.init 10_000 (fun _ -> short_string))

let nreplace_bench name input_string =
  let test_nreplace f niters =
    for j = 1 to niters do
      ignore (f ~str:input_string ~sub:pattern ~by:replace);
    done
  in
  Bench.bench_n [
    name ^ ":string simple", test_nreplace nreplace_string_simple;
    name ^ ":substring simple", test_nreplace nreplace_substring_simple;
    name ^ ":substring optimized", test_nreplace nreplace_substring_optimized;
    name ^ ":optimized", test_nreplace nreplace_optimized;
  ]

let () =
  List.iter (fun b -> print_newline (Bench.summarize b)) [
    replace_bench "replace:short_string" short_string;
    replace_bench "replace:long_string" long_string;
    
    nreplace_bench "nreplace:short_string" short_string;
    nreplace_bench "nreplace:long_string" long_string;
  ]

(* representative results:

replace:short_string:optimized (73.32 ns) is 41.2% faster than
replace:short_string:substring simple (124.66 ns) which is 26.9% faster than
replace:short_string:string simple (170.43 ns)

replace:long_string:optimized (23.90 us) is probably (alpha=6.40%) same speed as
replace:long_string:substring simple (24.44 us) which is 67.6% faster than
replace:long_string:string simple (75.33 us)

nreplace:short_string:substring optimized (234.95 ns) is 4.6% faster than
nreplace:short_string:substring simple (246.23 ns) which is 21.4% faster than
nreplace:short_string:string simple (313.30 ns) which is 2.3% faster than
nreplace:short_string:optimized (320.67 ns)

nreplace:long_string:optimized (2.06 ms) is 48.3% faster than
nreplace:long_string:string simple (3.97 ms) which is 14.1% faster than
nreplace:long_string:substring optimized (4.62 ms) which is probably (alpha=22.68%) same speed as
nreplace:long_string:substring simple (4.67 ms)

*)
