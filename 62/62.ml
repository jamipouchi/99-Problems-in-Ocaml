type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec internals = function
  | Empty -> []
  | Node (_,Empty,Empty) -> []
  | Node (v,l,r) -> internals l @ [v] @ internals r (* Used preorder just to pass the test*)

let test_count_leaves () =
  assert (internals @@ Node('x',Empty,Empty)= []);
  assert (internals @@ Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
  Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty))) = ['b';'a';'c';'f'])
