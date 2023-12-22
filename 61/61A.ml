type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec leaves = function
  | Empty -> []
  | Node (v,Empty,Empty) -> [v]
  | Node (_,l,r) -> leaves l @ leaves r

  let test_count_leaves () =
    assert (leaves Empty = []);
    assert (leaves @@ Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
    Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty))) = ['d';'e';'g'])
