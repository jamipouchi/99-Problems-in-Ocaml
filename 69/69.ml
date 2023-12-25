type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let rec tree_dotstring = function
  | Empty -> "."
  | Node (v, l, r) -> String.make 1 v ^ tree_dotstring l ^ tree_dotstring r

let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
    (Node ('a', Node ('b', leaf 'd', leaf 'e'),
      Node ('c', Empty, Node ('f', leaf 'g', Empty))));;

let test_tree_dotstring () =
  assert (tree_dotstring example_layout_tree = "abd..e..c.fg...")