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

(* time_fn *)
(*   "merge_sort_array" *)
(*   random_vect *)
(*   Merge_sort.merge_sort_array *)
(*   [10; 100; 1000; 10000; 100000; 1000000]; *)
(* merge_sort_array : 10 elements -> 0.000009 *)
(* merge_sort_array : 100 elements -> 0.000081 *)
(* merge_sort_array : 1000 elements -> 0.001066 *)
(* merge_sort_array : 10000 elements -> 0.013871 *)
(* merge_sort_array : 100000 elements -> 0.138763 *)
(* merge_sort_array : 1000000 elements -> 1.525749 *)

(* time_fn *)
(*   "merge_sort" *)
(*   random_list *)
(*   Merge_sort.merge_sort_list *)
(*   [10; 100; 1000; 10000; 100000; 1000000]; *)
(* merge_sort_list : 10 elements -> 0.000012 *)
(* merge_sort_list : 100 elements -> 0.000094 *)
(* merge_sort_list : 1000 elements -> 0.001280 *)
(* merge_sort_list : 10000 elements -> 0.015273 *)
(* merge_sort_list : 100000 elements -> 0.168302 *)
(* Fatal error: exception Stack_overflow *)

(* open Radix_sort;; *)
(* time_fn *)
(*   "radix_sort" *)
(*   random_vect *)
(*   radix_sort *)
(*   [10; 100; 1000; 10000; 100000]; *)
(* if let rand_int = 1073741;; *)
(* radix_sort : 10 elements -> 0.000044 *)
(* radix_sort : 100 elements -> 0.000314 *)
(* radix_sort : 1000 elements -> 0.017807 *)
(* radix_sort : 10000 elements -> 0.547238 *)
(* radix_sort : 100000 elements -> 88.426987 *)
(* if let rand_int = 1073741823;; *)
(* radix_sort : 10 elements -> 0.000075 *)
(* radix_sort : 100 elements -> 0.000396 *)
(* radix_sort : 1000 elements -> 0.020093 *)
(* radix_sort : 10000 elements -> 0.749763 *)
(* radix_sort : 100000 elements -> 80.812605 *)
