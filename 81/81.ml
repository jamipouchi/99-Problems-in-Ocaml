type 'a graph_term = {nodes : 'a list;  edges : ('a * 'a) list}

let rec paths edges o d curr =
  if o = d then  [List.rev @@ d::curr]
  else if List.length edges = 0 then []
  else 
    List.concat_map (fun (u,v) -> match u,v with
      |u,v when u = o -> 
        let next_edges = List.filter (fun (a,b) -> a <> o && b <> o) edges in
        paths (next_edges) v d (u::curr)
      | u,v when v = o ->
        let next_edges = List.filter (fun (a,b) -> a <> o && b <> o) edges in
        paths (next_edges) u d (v::curr)
      | u,v -> []
    ) edges

let paths g o d = paths g.edges o d []

let example_graph =
  {nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
    edges = [('h', 'g'); ('k', 'f'); ('f', 'b'); ('f', 'c'); ('c', 'b')]}

let test_paths () =
  assert (paths example_graph 'f' 'b' = [['f'; 'b']; ['f'; 'c'; 'b']]);;