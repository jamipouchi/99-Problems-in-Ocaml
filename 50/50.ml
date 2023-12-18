(*
  `DumbPriorityQueue` supports:
    -> add: O(n*log(n)) // Assuming that List.sort is O(n*log(n))
    -> pop: O(1)
  Note that addition can be greatly improved. 
  First improvement is inserting in an ordered list -> O(n)
  Next improvement, instead of using a list, and sorting on every insertion...
  A heap should be used, in the form of
      1. Array
      2. BTree
  Thus resulting in O(log(n)) for addition, and no modification for popping.
  The problem is not about that, so I'll leave it as is.
*)
module DumbPriorityQueue = struct
  type 'a t = {mutable data: 'a list; cmp: 'a -> 'a -> int}
  let make cmp = {data = []; cmp}
  let add v pq = pq.data <- List.sort pq.cmp (v :: pq.data)
  let pop pq = match pq.data with
  [] -> raise Exit
  | h :: t -> 
    pq.data <- t;
    h
end

type tree =
  | Leaf of int * string
  | Node of int * tree * tree

let code_of_tree root =
  let rec code_of_subtree node code = match node with
  | Leaf (_,str) -> [(str, code)]
  | Node (_,l,r) -> (code_of_subtree l (code ^ "0")) @ (code_of_subtree r (code ^ "1")) in
  code_of_subtree root ""

let freq_of_node node = match node with
  | Leaf (freq,_) -> freq
  | Node (freq,_,_) -> freq;;

let rec tree_of_frequencies_pq (pq : tree DumbPriorityQueue.t) = match pq with
  {data=[node]} -> node
  | pq ->
    let l = DumbPriorityQueue.pop pq in
    let r = DumbPriorityQueue.pop pq in
    DumbPriorityQueue.add (Node ((freq_of_node l + freq_of_node r),l,r)) pq;
    tree_of_frequencies_pq pq


let tree_of_frequencies fs =
  let pq = DumbPriorityQueue.make (fun l r -> freq_of_node l - freq_of_node r) in
  List.map (fun (str,f) -> Leaf (f,str)) fs |> List.iter (fun leaf -> DumbPriorityQueue.add leaf pq);
  tree_of_frequencies_pq pq

let huffman fs =
  let tree = tree_of_frequencies fs in
  code_of_tree tree

let test_huffman () =
  assert (huffman [("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5)] =
    [("a", "0"); ("c", "100"); ("b", "101"); ("f", "1100"); ("e", "1101"); ("d", "111")]);
  assert (huffman [("a", 10); ("b", 15); ("c", 30); ("d", 16); ("e", 29)] = 
    [("d", "00"); ("a", "010"); ("b", "011"); ("e", "10"); ("c", "11")]);;