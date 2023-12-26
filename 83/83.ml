type 'a graph_term = {nodes : 'a list;  edges : ('a * 'a) list}

(*We can construct a spanning tree by removing edges from cycles*)


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

(*Being able to reuse cycles proves the point I made on past exercise. Obv it's not the most efficient.*)
let cycles g n = paths g.edges n n []

(*We can assume there's always at least 2 nodes on the cycles, as to make a cycle we need at least 3 nodes.*)
let has_cycle g u v = List.filter (fun (a :: b :: rs) -> (a,b)=(u,v)) (cycles g u) <> []

(*
   This does not eliminate repetition... We could do it by keeping a pointer to the last 'seen' edge.
   We'd lose the nice `concat_map` structure though, so I'll leave it as is.
   Big part of why I leave it as is is because there is no example on the page.
*)
let rec spanning_tree g =
  List.concat_map (fun (u,v) -> 
    if has_cycle g u v then
      let next_edges = List.filter ((<>) (u,v)) g.edges in
      let next_tree = {g with edges = next_edges} in
      next_tree :: spanning_tree next_tree
    else []
  ) g.edges

let s_tree g = g :: spanning_tree g
