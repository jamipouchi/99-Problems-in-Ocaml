type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let build_tree_from_node_list l =
  let rec aux l i k =
    match l, i with
    | [], _ -> Empty
    | v :: t, i when i = k -> Node(v,aux t (2*i) (k+1),aux t (2*i + 1) (k+1))
    | _ :: t, i -> aux t i (k+1)
  in
  aux l 1 1

let complete_binary_tree = function
  | [] -> Empty
  | l -> build_tree_from_node_list l

  let test_complete_binary_tree () =
    let l = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k"] in
    let t = complete_binary_tree l in
    assert (t = Node ("a", Node ("b", Node ("d", Node ("h", Empty, Empty), Node ("i", Empty, Empty)), Node ("e", Node ("j", Empty, Empty), Node ("k", Empty, Empty))), Node ("c", Node ("f", Empty, Empty), Node ("g", Empty, Empty))))