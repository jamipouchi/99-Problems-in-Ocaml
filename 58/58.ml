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

let rec is_mirror l r = match l,r with
  | Empty,Empty -> true
  | Node(_,ll,lr),Node(_,rl,rr) -> is_mirror ll rr && is_mirror lr rl
  | (_,_) -> false

let is_symmetric = function
  Empty -> true
  | Node(_,l,r) -> is_mirror l r

let sym_cbal_trees n = List.filter is_symmetric @@ cbal_tree n

let test_sym_cbal_trees () =
  assert (sym_cbal_trees 5 = 
  [
    Node ('X', Node ('X', Node ('X', Empty, Empty), Empty),
    Node ('X', Empty, Node ('X', Empty, Empty)));
    Node ('X', Node ('X', Empty, Node ('X', Empty, Empty)),
    Node ('X', Node ('X', Empty, Empty), Empty))
  ]);
