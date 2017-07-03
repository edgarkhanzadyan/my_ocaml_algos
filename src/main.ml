let time f x =
    let t = Sys.time() in
    let fx = f x in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fx;;

let f n =
  print_endline "";
  print_int n;;

let range fir sec =
  let rec aux a b acc =
    if a > b then aux (a-1) b (acc @ [a])
    else if a < b then aux (a+1) b (acc @ [a])
    else acc @ [a] in
  aux fir sec [];;

let ran = range 0 10000;;
let res = time Merge_sort.merge_sort ran;;
List.iter f res;;
