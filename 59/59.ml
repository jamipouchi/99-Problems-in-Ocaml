type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

let combine_all l_tree_list r_tree_list =
  List.concat_map (fun l_tree -> 
    List.map (fun r_tree -> Node ('X', l_tree, r_tree)) r_tree_list
    ) (l_tree_list)

let rec hbal_tree = function
  | 0 -> [Empty]
  | 1 -> [Node('X',Empty,Empty)]
  | num ->
    let height_1_tree = hbal_tree (num - 1) in
    let height_2_tree = hbal_tree (num - 2) in
    (combine_all height_1_tree height_2_tree) 
    @ (combine_all height_2_tree height_1_tree) 
    @ (combine_all height_1_tree height_1_tree)

let x = 'X'
let test_hbal_tree () =
  assert(
    List.mem 
    (Node (x, Node (x, Node (x, Empty, Empty), Node (x, Empty, Empty)),Node (x, Node (x, Empty, Empty), Node (x, Empty, Empty)))) 
    (hbal_tree 3));
  assert(List.mem (Node (x, Node (x, Node (x, Empty, Empty), Node (x, Empty, Empty)),
  Node (x, Node (x, Empty, Empty), Empty))) (hbal_tree 3));
  assert (List.length (hbal_tree 3) = 15)