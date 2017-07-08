let counting_sort arr =
  let find_max arr =
    let max = ref arr.(0) in
    for i = 0 to (Array.length arr) - 1 do
      if arr.(i) > !max then max := arr.(i)
    done;
    !max in
  let counted_arr = Array.make (find_max arr) 0 in
  for i = 0 to (Array.length arr) - 1 do
    counted_arr.(arr.(i) - 1) <- counted_arr.(arr.(i) - 1) + 1
  done;
  (* Array.iter print_int counted_arr; *)
  let rec listify acc i j=
    if i = 0 && j = 0 then acc
    else if j = 0 then listify acc (i - 1) (counted_arr.(i - 1))
    else listify (i+1 :: acc) i (j - 1) in
  let len = Array.length counted_arr in
  listify [] (len - 1) (counted_arr.(len - 1));;

