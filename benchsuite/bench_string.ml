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
(* let long_string = BatString.concat (BatList.init 10_000 short_string) *)
let long_string = String.concat " " [String.make 5_000 'b'; "aaa"; String.make 5_000 'c']

let replace_bench name input_string =
  let test_f f niters =
    for j = 1 to niters do
      ignore (f ~str:input_string ~sub:pattern ~by:replace);
    done in
  Bench.bench_n [
    name ^ ":string simple", test_f replace_string_simple;
    name ^ ":substring simple", test_f replace_substring_simple;
    name ^ ":optimized", test_f replace_optimized;
  ] 


let () =
  Bench.summarize (replace_bench "short_string" short_string)

let () =
  Bench.summarize (replace_bench "long_string" long_string)






