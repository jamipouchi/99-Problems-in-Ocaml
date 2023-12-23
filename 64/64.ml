type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let layout_binary_tree_1 t =
  let rec layout_from_pos i j = function
    | Empty -> Empty, 0
    | Node(v,Empty,Empty) -> Node ((v,i,j),Empty,Empty), 1
    | Node(v,l,r) -> 
      let left_subtree, nodes_left = layout_from_pos i (j+1) l in
      let right_subtree, nodes_right = layout_from_pos (i+nodes_left+1) (j+1) r in
      Node ((v,i+nodes_left,j), left_subtree, right_subtree), (nodes_left + 1 + nodes_right) in
  fst @@ layout_from_pos 1 1 t
    

(*
   Need to do a preorder traversal on position. -> visit left visit me visit right

   Buuut, we need to keep count of nodes... -> 
    (left_subtree, #nodes_left) = visit_left (0, left_subtree)
    (right_subtree, #nodes_right) visit_right (#nodes_left + 2, right_subtree)
    return ((me(()), left_subtree, right_subree), #nodes_left + #nodes_right + 1)
    nodes_right (nodes_left+2)
*)

let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
  Node ('n', Node ('k', Node ('c', leaf 'a',
                            Node ('h', Node ('g', leaf 'e', Empty), Empty)),
                  leaf 'm'),
        Node ('u', Node ('p', Empty, Node ('s', leaf 'q', Empty)), Empty));;
    
let test_layout_binary_tree_1 () =
  assert (layout_binary_tree_1 example_layout_tree = Node (('n', 8, 1),
  Node (('k', 6, 2),
   Node (('c', 2, 3), Node (('a', 1, 4), Empty, Empty),
    Node (('h', 5, 4),
     Node (('g', 4, 5), Node (('e', 3, 6), Empty, Empty), Empty), Empty)),
   Node (('m', 7, 3), Empty, Empty)),
  Node (('u', 12, 2),
   Node (('p', 9, 3), Empty,
    Node (('s', 11, 4), Node (('q', 10, 5), Empty, Empty), Empty)),
   Empty)))
