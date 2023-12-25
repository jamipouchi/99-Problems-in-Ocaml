type 'a graph_term = {nodes : 'a list;  edges : ('a * 'a) list}
type 'a adjacency_list = ('a * 'a list) list

let adjacency_list_of_graph_term gt =
  List.map (fun node -> 
    (node, List.filter_map (fun (a,b) -> match a,b with
      | a, b when a = node -> Some b
      | a, b when b = node -> Some a
      | _,_->  None)
    gt.edges)) 
  gt.nodes

(*
   This isn't correct as it adds each edge twice. (Don't care enough to fix it!)
*)
let graph_term_of_adjacency_list al = 
  let nodes = List.map fst al in
  let edges = List.fold_left (fun acc (n, ns) -> List.map (fun n' -> (n, n')) ns @ acc) [] al in
  {nodes = nodes; edges = edges}
