type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let at_level t n =
  let rec at_level ts = function
  | x when x < 1 -> raise Exit
  | 1 -> List.map (function (Node (v,_,_)) -> v | Empty -> raise Exit) ts
  | n -> 
    at_level (List.concat_map (function
      | Empty -> raise Exit
      | (Node(_, l, t)) -> match l,t with
        | Empty, Empty -> []
        | l, Empty -> [l]
        | Empty, r -> [r]
        | l, r -> [l;r]
    ) ts) (n-1) in
  match t with
  | Empty -> []
  | t -> at_level [t] n

let show_test_at_level () =
  at_level (Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
              Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)))) 2

let test_at_level () =
  assert (
    at_level (Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
              Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)))) 2 = ['b'; 'c']);
  assert (
    at_level (Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
              Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)))) 5 = []);
