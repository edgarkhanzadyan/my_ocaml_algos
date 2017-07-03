let rand_int = 1073741823;;
let random_vect length =
  let t = Array.make length 0 in
  for pos = 0 to length - 1 do
    t.(pos) <- Random.int rand_int
  done;
  t;;

let rec random_list = function
  | 0 -> []
  | n -> (Random.int rand_int) :: (random_list (n - 1));;

let time randomize sort length =
  let data = randomize length in
  let start = Sys.time() in
  sort data;
  Sys.time () -. start;;

let time_fn name random fn counts =
  let rec aux = function
    | [] -> ()
    | count :: tail ->
      Printf.printf "%s : %d elements -> %f\n"
        name count (time random fn count);
      aux tail in
  aux counts;
  print_newline ();;
time_fn
  "merge_sort"
  random_vect
  Merge_sort.merge_sort_array
  [10; 100; 1000; 10000; 100000; 1000000];
(* merge_sort : 10 elements -> 0.000009 *)
(* merge_sort : 100 elements -> 0.000078 *)
(* merge_sort : 1000 elements -> 0.001057 *)
(* merge_sort : 10000 elements -> 0.013286 *)
(* merge_sort : 100000 elements -> 0.136587 *)
(* merge_sort : 1000000 elements -> 1.578245 *)
