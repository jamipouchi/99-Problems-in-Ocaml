type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec get_left_most_height = function
  | Empty -> 0
  | Node(_,l,_) -> 1 + get_left_most_height l

let rec get_height = function
  | Empty -> 0
  | Node(_,l,r) -> 1 + max (get_height l) (get_height r)

let layout_binary_tree_2 t =
  let height = get_height t in
  let left_most_height = get_left_most_height t in
  let start_position = 1 lsl left_most_height - 1 in
let rec layout_from_pos i j = function
    | Empty -> Empty
    | Node(v,Empty,Empty) -> Node ((v,i,j),Empty,Empty)
    | Node(v,l,r) -> 
      let separation = 1 lsl (height-j-1) in
      let left_subtree = layout_from_pos (i-separation) (j+1) l in
      let right_subtree = layout_from_pos (i+separation) (j+1) r in
      Node ((v,i,j), left_subtree, right_subtree) in
  layout_from_pos start_position 1 t

(*
  Let's define the 'inverse height' of the tree such that the nodes at the lowest height have 'inverse height' = 0, 
  and each level we go up, the 'inverse height' goes up by 1.   
  We construct such tree by placing the first node in a preorder traversal in column 1, and the separation between the two siblings of a node is its 2**'inverse height' or 1 lsl inverse_height

  The 'inverse height' for a node n can be calculated as `tree_height - height(n)`, where height(root) = 1
*)

let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
  Node ('n', Node ('k', Node ('c', leaf 'a',
                           Node ('e', leaf 'd', leaf 'g')),
                 leaf 'm'),
       Node ('u', Node ('p', Empty, leaf 'q'), Empty));;

let test_layout_binary_tree_2 () =
  assert (layout_binary_tree_2 example_layout_tree =Node (('n', 15, 1),
  Node (('k', 7, 2),
   Node (('c', 3, 3), Node (('a', 1, 4), Empty, Empty),
    Node (('e', 5, 4), Node (('d', 4, 5), Empty, Empty),
     Node (('g', 6, 5), Empty, Empty))),
   Node (('m', 11, 3), Empty, Empty)),
  Node (('u', 23, 2),
   Node (('p', 19, 3), Empty, Node (('q', 21, 4), Empty, Empty)), Empty)))