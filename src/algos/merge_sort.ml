let merge_sort arr =
  let rec merge arr1 arr2 acc =
    match arr1, arr2 with
    | (h1 :: t1 as a1), (h2 :: t2 as a2) ->
      if h1 >= h2 then merge t1 a2 (h1 :: acc)
      else merge a1 t2 (h2 :: acc)
    | [], h2 :: t2 -> merge [] t2 (h2 :: acc)
    | h1 :: t1, [] -> merge t1 [] (h1 :: acc)
    | [], [] -> List.rev acc in
  let rec divide k acc = function
    | hd :: tl ->
      if k <= 1 then (hd :: acc, tl)
      else divide (k-1) (hd :: acc) tl
    | [] -> (acc, []) in
  let rec aux l =
    let len = List.length l in
    let fir, sec = divide (len / 2) [] l in
    if len >= 2 then
      (merge (aux (fir)) (aux (sec)) [])
    else
      l in
  aux arr

let merge_sort_array arr =
  let rec merge arr1 arr2 acc k1 k2 k =
    if k >= Array.length acc then acc
    else (
      let l1 = Array.length arr1 in
      let l2 = Array.length arr2 in
      if k1 < l1 && k2 < l2 then (
        if arr1.(k1) > arr2.(k2) then (
          acc.(k) <- arr1.(k1);
          merge arr1 arr2 acc (k1 + 1) k2 (k + 1) 
        )
        else (
          acc.(k) <- arr2.(k2);
          merge arr1 arr2 acc k1 (k2 + 1) (k + 1)
        )
      )
      else if k2 >= l2 then (
        acc.(k) <- arr1.(k1);
        merge arr1 arr2 acc (k1 + 1) k2 (k + 1)
      )
      else if k1 >= l1 then (
        acc.(k) <- arr2.(k2);
        merge arr1 arr2 acc k1 (k2 + 1) (k + 1)
      )
      else acc
    ) in
  let rec main arr =
    let len = Array.length arr in
    if len > 1 then (
      let a1 = Array.sub arr 0 (len / 2) in
      let a2 = Array.sub arr (len / 2) (if len mod 2 = 1
                                        then len / 2 + 1
                                        else len / 2) in
      let new_arr = Array.make len 0 in
      merge (main a1) (main a2) new_arr 0 0 0
    )
    else arr in
  main arr
