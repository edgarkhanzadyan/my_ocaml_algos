type 'a binary_tree_avl =
  | Empty
  | Node of
      'a                        (* item *)
      * int                     (* height of tree *)
      * 'a binary_tree_avl      (* left child *)
      * 'a binary_tree_avl      (* right child *)

let print_tree tree=
  let rec aux str = function
    | Node (n, h, left, right) ->
      print_string (str ^ ": ");
      print_int n;
      print_string " h: ";
      print_int h;
      print_newline ();
      aux (str ^ "L") left;
      aux (str ^ "R") right;
    | Empty -> () in
  aux "" tree;;
        

let predecessor item tree =
  let rec go_to_parent = function
    | [] -> failwith "no predeseccor"
    | hd :: tl ->
      let tr, direct = hd in
      match tr with
      | Node (num, _, _, _) ->
        if direct then num
        else go_to_parent tl
      | Empty -> failwith "never happends predecessor" in
  let rec aux acc = function 
    | Node (num, _, left, right) as tree ->
      if item > num then aux ((tree, true) :: acc) right
      else if item < num then aux ((tree, false) :: acc) left
      else (
        match left with
        | Node (n, _, _, _) -> n
        | Empty -> go_to_parent acc
      )
    | Empty -> failwith "shit" in
  aux [] tree
    
let successor item tree =
  let rec go_to_parent = function
    | [] -> failwith "no successor"
    | hd :: tl ->
      let tr, direct = hd in
      match tr with
      | Node (num, _, _, _) ->
        if not direct then num
        else go_to_parent tl
      | Empty -> failwith "never happends successor" in
  let rec aux acc = function 
    | Node (num, _, left, right) as tree ->
      if item > num then aux ((tree, true) :: acc) right
      else if item < num then aux ((tree, false) :: acc) left
      else (
        match right with
        | Node (n, _, _, _) -> n
        | Empty -> go_to_parent acc
      )
    | Empty -> failwith "shit" in
  aux [] tree


let rec left_rotation item = function
  | Node (num, height, left, right) ->
    if item > num then Node(num, height, left, left_rotation item right)
    else if item < num then Node(num, height, left_rotation item left, right)
    else (
      match left, right with
      | Node (_, h1, _, _), Node (n2, h2, l2, r2) ->
        if abs (h2 - h1) > 1 then
          Node(n2, h2, Node(num, height-2, left, l2), r2)
        else Node(n2, h2+1, Node(num, height-1, left, l2), r2)
      | Empty, Node (n2, h2, l2, r2) ->
        if h2 > 0 then
          Node(n2, h2, Node(num, height-2, left, l2), r2)
        else Node(n2, h2+1, Node(num, height-1, left, l2), r2)
      | Node _, Empty | Empty, Empty ->
        failwith "this node doesn't have right side,so no left_rotation possible"
    )
  | Empty -> failwith "no such node";;

let rec right_rotation item = function
  | Node (num, height, left, right) ->
    if item > num then Node(num, height, left, right_rotation item right)
    else if item < num then Node(num, height, right_rotation item left, right)
    else (
      match left, right with
      | Node (n1, h1, l1, r1), Node (_, h2, _, _) ->
        if abs (h2 - h1) > 1 then
          Node(n1, h1, l1, Node(num, height-2, r1, right))
        else Node(n1, h1+1, l1, Node(num, height-1, r1, right))
      | Node (n1, h1, l1, r1), Empty ->
        if h1 > 0 then
          Node(n1, h1, l1, Node(num, height-2, r1, right))
        else Node(n1, h1+1, l1, Node(num, height-1, r1, right))
      | Empty, Empty | Empty, Node _ ->
        failwith "this node doesn't have left side,so no right_rotation possible"
    )
  | Empty ->
    failwith "no such node";;

let rebalance = function
  | Node(num, _, left, right) as tree ->
    (
      match left, right with
      | Node (n1, h1, l1, r1), Node(n2, h2, l2, r2) ->
        if h2 - h1 > 1 then (
          match l2, r2 with
          | Node (_, he1, _, _), Node (_, he2, _, _) ->
            if he1 - he2 = 1 then left_rotation num (right_rotation n2 tree)
            else left_rotation num tree
          | Node _, Empty ->
            left_rotation num (right_rotation n2 tree)
          | Empty, Node _ | Empty, Empty ->
            left_rotation num tree
        )
        else if h1 - h2 > 1 then (
          match l1, r1 with
          | Node (_, he1, _, _), Node (_, he2, _, _) ->
            if he2 - he1 = 1 then right_rotation num (left_rotation n1 tree)
            else right_rotation num tree
          | Empty, Node _ ->
            right_rotation num (left_rotation n1 tree)
          | Node _, Empty | Empty, Empty ->
            right_rotation num tree
        )
        else tree
      | Node (n1, h1, l1, r1), Empty ->
        let h2 = -1 in
        if h1 - h2 > 1 then (
          match l1, r1 with
          | Node (_, he1, _, _), Node (_, he2, _, _) ->
            if he2 - he1 = 1 then
              right_rotation num (left_rotation n1 tree)
            else right_rotation num tree
          | Empty, Node _ ->
            right_rotation num (left_rotation n1 tree)
          | Node _, Empty | Empty, Empty ->
            right_rotation num tree
        )
        else tree
      | Empty, Node(n2, h2, l2, r2) ->
        let h1 = -1 in
        if h2 - h1 > 1 then (
          match l2, r2 with
          | Node (_, he1, _, _), Node (_, he2, _, _) ->
            if he1 - he2 = 1 then
              left_rotation num (right_rotation n2 tree)
            else left_rotation num tree
          | Node _, Empty ->
            left_rotation num (right_rotation n2 tree)
          | Empty, Node _ | Empty, Empty ->
            left_rotation num tree
        )
        else tree
    | Empty, Empty ->
      tree
  )
  | Empty -> failwith "wtf it's empty";;

let insert item tree =
  let rec rebalance_all acc = function
    | [] -> acc
    | hd :: tl ->
      match hd with
      | Node (n, h, l, r), direct ->
        if direct then
          let reb = rebalance (Node (n, h, l, acc)) in
          rebalance_all reb tl
        else
          let reb = rebalance (Node (n, h, acc, r)) in
          rebalance_all reb tl
      | Empty, _ -> failwith "never happens" in
  let rec add_height acc = function
    | [] -> List.rev acc
    | hd :: tl ->
      match hd with
      | Node (n, h, l, r), direct ->
        add_height ((Node (n, h+1, l, r), direct) :: acc) tl
      | Empty, _ -> failwith "never happerns" in
  let is_node_there = function
    | Node _ -> true
    | Empty -> false in
  let rec aux acc = function
    | Node (num, _, left, right) as tree ->
      if item >= num then
        if is_node_there right then 
          aux ((tree, true) :: acc) right
        else aux ((left, false) :: (tree, true) :: acc) right
      else
        if is_node_there left then
          aux ((tree, false) :: acc) left
        else aux ((right, false) :: (tree, false) :: acc) left
    | Empty ->
      match acc with
      | [] -> Empty
      | hd :: tl ->
        match hd with
        | Node _, _ ->
          let new_node = Node (item, 0, Empty, Empty) in
          rebalance_all Empty ((new_node, true) :: tl)
        | Empty, _ ->
          let new_node = Node (item, -1, Empty, Empty) in
          let added_height = add_height [] ((new_node, true) :: tl) in
          rebalance_all Empty added_height in
  aux [] tree


