let merge_sort arr =
  let rec merge arr1 arr2 acc =
    match arr1, arr2 with
    | (h1 :: t1 as a1), (h2 :: t2 as a2) ->
      if h1 >= h2 then merge t1 a2 (acc@[h1])
      else merge a1 t2 (acc@[h2])
    | [], h2 :: t2 -> merge [] t2 (acc@[h2])
    | h1 :: t1, [] -> merge t1 [] (acc@[h1])
    | [], [] -> acc in
  let rec divide k acc = function
    | hd :: tl ->
      if k <= 1 then (acc@[hd], tl)
      else divide (k-1) (acc@[hd]) tl
    | [] -> (acc, []) in
  let rec aux l =
    let len = List.length l in
    let fir, sec = divide (len / 2) [] l in
    if len >= 2 then
      (merge (aux (fir)) (aux (sec)) [])
    else
      l in
  aux arr
