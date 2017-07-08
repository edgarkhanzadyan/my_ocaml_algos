let counting_sort arr key_f =
  let counted_arr = Array.make (Array.length arr) [||] in
  for i = 0 to (Array.length arr) - 1 do
    let sub_arr = counted_arr.(key_f arr.(i)) in
    let sub_arr_len = Array.length sub_arr in
    let new_a () =
      let a = Array.make (sub_arr_len + 1) 0 in
      Array.blit sub_arr 0 a 0 sub_arr_len;
      a.(Array.length a - 1) <- arr.(i);
      a in
    counted_arr.( key_f arr.(i) ) <- (new_a ());
  done;
  let n_r = ref [||] in
  for i = 0 to Array.length counted_arr - 1 do
    n_r := Array.append !n_r counted_arr.(i)
  done;
  !n_r;;

let radix_sort arr =
  let len = Array.length arr in
  let rec power base exp =
    if exp <= 0 then 1
    else if exp = 1 then base
    else base * power base (exp - 1) in
  let find_max arr =
    let max = ref arr.(0) in
    for i = 0 to (Array.length arr) - 1 do
      if arr.(i) > !max then max := arr.(i)
    done;
    !max in
  let num_of_dig n =
    let k = ref 0 in
    while !n > 0 do
      n := !n / len;
      k := !k + 1;
    done;
    !k in
  let m = find_max arr in
  let rec sort_it from t n_arr =
    if from = t then n_arr
    else (
      let key_f number =
        (number mod (power len from)) / (power len (from-1))  in
      let new_arr = counting_sort n_arr key_f in
      sort_it (from + 1) t new_arr
    )in
  sort_it 1 (num_of_dig (ref m)+1) arr
  
    
