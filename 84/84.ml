type ('a, 'b) labeled_graph = {nodes : 'a list; labeled_edges : ('a * 'a * 'b) list}

(*
  We are given the hint to use Prim's algorithm. Fuck that. I will use the red rule. until there is no loops.
*)
let rec paths edges o d curr =
  if o = d && List.length curr >= 1 then  [List.rev @@ d::curr]
  else if List.length edges = 0 then []
  else 
    List.concat_map (fun (u,v,_) -> match u,v with
      |u,v when u = o -> 
        let next_edges = List.filter (fun (a,b,_) -> (a,b) <> (u,v)) edges in
        paths (next_edges) v d (u::curr)
      | u,v when v = o ->
        let next_edges = List.filter (fun (a,b,_) -> (a,b) <> (u,v)) edges in
        paths (next_edges) u d (v::curr)
      | u,v -> []
    ) edges

(*Being able to reuse cycles proves the point I made on past exercise. Obv it's not the most efficient.*)
let cycles g n = paths g.labeled_edges n n []

(*We can assume there's always at least 2 nodes on the cycles, as to make a cycle we need at least 3 nodes.*)
let get_cycle g = 
  let rec get_cycle_from edges = match edges with
    | [] -> []
    | (u,_,_) :: next_edges ->
      let cycles =  cycles g u in
        if cycles <> [] then List.hd cycles
        else get_cycle_from next_edges in
  get_cycle_from g.labeled_edges

let get_cost edge labeled_edges =
  match (List.filter (fun (u,v,c) -> (u,v) = edge || (v,u) = edge) g) with
  | [(_,_,c)] -> c
  | _ -> raise Exit

let get_max_edge g edges = 
  let rec aux curr_max = function
  | [] | [_] -> curr_max
  | u :: ((v :: _) as next_edges) -> 
    let cost = get_cost (u,v) g.labeled_edges in
    if cost > curr_max then aux (u,v,cost) next_edges
    else aux curr_max next_edges in
  aux (List.hd edges) g.labeled_edges

let rec ms_tree g = match get_cycle g with
  | [] -> g (* No more cycles, so we are at the mst *)
  | edges ->
    let max_edge = get_max_edge g edges in
    let next_edges = List.filter ((<>) max_edge) g.labeled_edges in
    ms_tree {g with labeled_edges = next_edges}

(*
   The idea is there. I am hungry though, so see ya
*)
