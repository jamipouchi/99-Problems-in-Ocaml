type 'a graph_term = {nodes : 'a list;  edges : ('a * 'a) list}

let rec paths edges o d curr =
  if o = d && List.length curr >= 1 then  [List.rev @@ d::curr]
  else if List.length edges = 0 then []
  else 
    List.concat_map (fun (u,v) -> match u,v with
      |u,v when u = o -> 
        let next_edges = List.filter ((<>) (u,v)) edges in
        paths (next_edges) v d (u::curr)
      | u,v when v = o ->
        let next_edges = List.filter ((<>) (u,v)) edges in
        paths (next_edges) u d (v::curr)
      | u,v -> []
    ) edges

let cycles g n = paths g.edges n n []

(*
   I know my solution does not 'pass' the test. But I don't agree with it. a cycle: u - v - u can't be valid if the cycle u-v-x-v-u isn't valid.
*)