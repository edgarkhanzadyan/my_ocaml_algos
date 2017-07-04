type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let print_tree tree =
  let rec aux str = function
    | Node (n, left, right) ->
      print_string (str ^ ": ");
      print_int n;
      print_newline ();
      aux (str ^ "L") left;
      aux (str ^ "R") right;
    | Empty -> () in
  aux "" tree;;

let find_min tree = 
  let rec aux acc count = function
    | Node (n, left, _) ->
      aux n (count+1) left
    | Empty ->
      if count = 0 then failwith "Empty tree"
      else acc in
  aux 0 0 tree;;

let find_max tree =
  let rec aux acc count = function
    | Node (n, _, right) ->
      aux n (count + 1) right
    | Empty ->
      if count = 0 then failwith "Empty tree"
      else acc in
  aux 0 0 tree;;

let rec insert_in_tree item tree =
  match tree with
  | Node (n, left, right) ->
    if item >= n then Node (n, left, insert_in_tree item right)
    else Node (n, insert_in_tree item left, right)
  | Empty -> Node(item, Empty, Empty);;

let rec insert_in_tree_list tree = function
  | [] -> tree
  | hd :: tl ->
    insert_in_tree_list (insert_in_tree hd tree) tl;;

let binary_tree_of_list list =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl ->
      aux (insert_in_tree hd acc) tl in
  aux Empty list

let list_of_binary_tree tree =
  let rec aux acc = function
    | Node (n, left, right) ->
      aux (n :: (aux acc left)) right
    | Empty -> acc in
  List.rev (aux [] tree)

let rec delete_from_tree item tree =
  match tree with
  | Node (n, left, right) ->
    if item > n then Node (n, left, delete_from_tree item right)
    else if item < n then Node (n, delete_from_tree item left, right)
    else (
      match left, right with
      | Empty, Empty ->
        Empty
      | Empty, Node (num, l, r) ->
        Node (num, l, r)
      | Node (num, l, r), Empty ->
        Node (num, l, r)
      | Node(_, _, _), Node(_, _, _) ->
        let min_right = find_min right in
        Node(min_right, left, delete_from_tree min_right right)
    )
  | Empty -> failwith "Item not found";;
