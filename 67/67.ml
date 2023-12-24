type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let rec string_of_tree = function
  | Empty -> ""
  | Node(v,Empty,Empty) -> String.make 1 v
  | Node(v,l,r) -> String.make 1 v ^ "(" ^ string_of_tree l ^ "," ^ string_of_tree r ^ ")"
