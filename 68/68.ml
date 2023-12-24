type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let rec preorder = function
  | Empty -> []
  | Node(v,Empty,Empty) -> [v]
  | Node(v,l,r) -> v :: preorder l @ preorder r

let rec inorder = function
  | Empty -> []
  | Node(v,Empty,Empty) -> [v]
  | Node(v,l,r) ->  preorder l @ [v] @ preorder r

(*
  It's not possible to construct a tree from a preorder tree. There's mulitple possibilities.
  Ex. 1  is Equal to 1
    2               2 3
  3
*)

(*
   We can do it from both preorder and inorder traversals though.

  We can use the inorder list to `split` the trees.

  Given the first value v in the preorder list, we split the inorder list at that value, 
  we repeat giving the first part of the list to build the left subtree and the second to build the second tree until the list is empty
*)

let split_at v l =
  let rec aux acc = function
    | [] -> raise Exit
    | x :: xs -> 
      if x = v then (List.rev acc), xs
      else aux (x :: acc) xs in
  aux [] l


let rec pre_in_tree_aux p i = match p, i with
  | p, [] -> p, Empty
  | v :: t, i ->
    let left_inorder_nodes, right_inorder_nodes = split_at v i in
    let right_preorder_nodes, left_tree = pre_in_tree_aux p left_inorder_nodes in
    let _, right_tree = pre_in_tree_aux right_preorder_nodes right_inorder_nodes in
    [],Node(v,left_tree, right_tree)
  | _,_ -> raise Exit

let pre_in_tree p i = snd @@ pre_in_tree_aux p i

