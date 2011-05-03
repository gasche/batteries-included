(*
tests.ml: oUnit test container

Copyright (C) 2007-2008  Mauricio Fernandez <mfp@acm.org>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

*)
let tests : OUnit.test list ref = ref []

let register x = tests := x :: !tests

let check name test_fun = fun () ->
  let time_begin = Unix.time () in
  test_fun ();
  let time_end = Unix.time () in
  if time_end -. time_begin > 0.5 then
    Printf.eprintf "%s taking more than half a second.\n%!"
      (match name with
        | None -> "Anonymous test"
        | Some name -> "Test '"^name^"'")

let rec check_too_long name = function
  | OUnit.TestCase test_fun ->
    OUnit.TestCase (check name test_fun)
  | OUnit.TestList tests ->
    OUnit.TestList (List.map (check_too_long name) tests)
  | OUnit.TestLabel (label, test) ->
    let name = match name with
      | None -> Some label
      | Some name -> Some (name^":"^label) in
    OUnit.TestLabel (label, check_too_long name test)

let all_tests () =
  List.map (check_too_long None) !tests

let data_dir = Filename.concat (Sys.getcwd ()) "data"
