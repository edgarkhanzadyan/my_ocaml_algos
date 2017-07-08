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
  let n = ref 0 in
  for i = 0 to (Array.length counted_arr) - 1 do
    while counted_arr.(i) > 0 do
      counted_arr.(i) <- counted_arr.(i) - 1;
      n := !n + 1;
      arr.(!n - 1) <- i + 1
    done;
  done;
  arr;;
