type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

let rec insert tree v = match tree with
  | Empty -> Node (v,Empty,Empty)
  | Node(x,l,r) -> 
    if v = x then tree
    else if v > x then Node(x, l, insert r v)
    else Node(x,insert l v, r)

let construct = function 
  [] -> Empty
  | xs -> List.fold_left insert Empty xs

let test_construct () =
  assert (construct [3; 2; 5; 7; 1] = 
  Node (3, Node (2, Node (1, Empty, Empty), Empty),Node (5, Empty, Node (7, Empty, Empty))));;