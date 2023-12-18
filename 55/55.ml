type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

let combine_all l_tree_list r_tree_list =
  List.concat_map (fun l_tree -> 
    List.map (fun r_tree -> Node ('X', l_tree, r_tree)) r_tree_list
    ) (l_tree_list)

let rec cbal_tree = function
  0 -> [Empty]
  | n when n mod 2 = 0 -> 
    let half_subtrees = cbal_tree (n/2) in
    let half_1_subtrees = cbal_tree (n/2 - 1) in
    (combine_all half_subtrees half_1_subtrees) @ (combine_all half_1_subtrees half_subtrees)
  | n ->
    let half_subtrees =  cbal_tree (n/2) in
    combine_all half_subtrees half_subtrees
