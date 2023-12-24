(*
   I believe it's made such that given a node:
   - The right most node of the left subtree is 1 to the left
   - The left most node of the right subtree is 1 to the right

   The recursion is broken in the middle...
*)
type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let layout_binary_tree_3 t =
   TODO() prob never will do it :(
(*
   For a given node n, we consider the sublevel with the largest number of nodes. We then put as much space as the next (power/multiple)? of 2
*)