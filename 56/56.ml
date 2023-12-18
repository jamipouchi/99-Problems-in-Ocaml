type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

let rec is_mirror l r = match (l,r) with
  (Empty,Empty) -> true
  | (Node(_,ll,lr),Node(_,rl,rr)) -> is_mirror ll rr && is_mirror lr rl
  | (_,_) -> false

let is_symmetric = function
  Empty -> true
  | Node(_,l,r) -> is_mirror l r

(* Required for next problem, used for testing :)*)
let rec insert tree v = match tree with
  | Empty -> Node (v,Empty,Empty)
  | Node(x,l,r) -> 
    if v = x then tree
    else if v > x then Node(x, l, insert r v)
    else Node(x,insert l v, r)

let construct = function 
  [] -> Empty
  | xs -> List.fold_left insert Empty xs

let test_symmetric () =
  assert (is_symmetric @@ construct [5; 3; 18; 1; 4; 12; 21] = true); 
  assert (not @@ is_symmetric @@ construct [3; 2; 5; 7; 4] = true)