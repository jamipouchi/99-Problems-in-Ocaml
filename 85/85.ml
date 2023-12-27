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

let rec check_frequencies l1 l2 = match l1,l2 with
  | [],[] -> true
  | (_,ll1)::r1, (_,ll2)::r2 -> 
    if List.length ll1 <> List.length ll2 then false 
    else check_frequencies r1 r2
  | _ -> raise Exit

(*Implementation of a pure functional map. I think it's beautiful.*)
let empty_map = fun _ -> None
let get map k = map k
let put map k v = fun key -> if key = k then Some v else get map key

let force_get map k = match get map k with
  | None -> raise Exit
  | Some v -> v

let rec check_mapping adj1 adj2 mapping = match adj1,adj2 with
  | [],[] -> true
  | (v1,ns1)::r1, (v2,ns2)::r2 -> 
     force_get mapping v1 = v2 
    && List.fold_left2 (fun acc n1 n2 -> acc && force_get mapping n1 = n2) true ns1 ns2
    && check_mapping r1 r2 mapping
  | _ -> raise Exit

(*
   1. Group by length
   2. Generate permutations for each length
   3. Combine all permutations
   4. Check each combination
*)
let add_all v ls =
  if List.is_empty ls then [[v]]
  else List.map (fun ls -> v :: ls) ls

let permutations ls =
  let rec pick acc n = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then (h, (List.rev acc) @ t) else pick (h :: acc) (n - 1) t in
  let rec generatePermutationsFrom idx ls =
    if idx = List.length ls then []
    else
      let (x, xs) = pick [] idx ls in
    (add_all x (generatePermutationsFrom 0 xs)) @ (generatePermutationsFrom (idx + 1) ls) in
   generatePermutationsFrom 0 ls

(*
  [ [ [1;2];[2;1] ] ; [ [3;4] [4;3] ] ] -> [[1;2;3;4] ; [1;2;4;3] ; [2;1;3;4] ; [2;1;4;3] ]

next_combinations = [ [3;4] [4;3] ]
first_group = [ [1;2];[2;1] ]

[1;2] [ [3;4] [4;3] ] -> [[1;2;3;4] ; [1;2;4;3]]
[2;1] [ [3;4] [4;3] ] -> [[2;1;3;4] ; [2;1;4;3]]

*)

let rec all_combinations:('a list list list -> 'a list list) = function
  | [] -> raise Exit
  | [last_group] -> last_group
  | first_group :: rest_of_groups ->
    let next_combinations = all_combinations rest_of_groups in
    List.concat_map (
      fun v -> 
        List.map (fun comb -> v @ comb) next_combinations
    ) first_group

let get_mapping adj1 adj2 = 
  let rec add_mapping l1 l2 map = match l1,l2 with
    | [],[] -> map
    | (k,_)::r1,(v,_)::r2 ->
      let next_map = put map k v in
      add_mapping r1 r2 next_map
    | _ -> raise Exit in
  add_mapping adj1 adj2 empty_map

let check_permutations permutations adj2 = match permutations with 
  | [] -> false (* We have exhausted all permutations, and none matched *)
  | adj1 :: rest ->
    let mapping = get_mapping adj1 adj2 in
    check_mapping adj1 adj2 mapping

let generate_and_check_mappings adj1 adj2 = 
  let rec group_by_sorted_neighbor_lengths adj_list acc curr = match adj_list with
    | [] -> raise Exit
    | [x] -> List.rev ((List.rev (x :: curr)) :: acc)
    | ((_,l1) as first)::((_,l2)::_ as rest) -> 
      if List.length l1 = List.length l2 
        then group_by_sorted_neighbor_lengths rest acc (first :: curr)
        else group_by_sorted_neighbor_lengths rest ((List.rev curr) :: acc) [] in
  let groups = group_by_sorted_neighbor_lengths adj1 [] [] in
  let group_permutations = List.map permutations groups in
  let permutations = all_combinations group_permutations in
  check_permutations permutations adj2

(*
   This is not an easy to tackle problem algorithmically.
   What we can do best is try all the combinations that make sense:
   1. Check that they have the same number of nodes and edges
   2. Convert each graph to adj. list representation ordered by neighbours
   3. Check that their neighbour distribution is the same
   4. Check all possible mappings between nodes with the same neighbours
*)
let iso g1 g2 = 
  if List.length g1.nodes <> List.length g2.nodes || List.length g1.edges <> List.length g2.edges then false
  else
    let adj1 = List.sort (fun (_,n1) (_,n2) -> List.length n1 - List.length n2) (adjacency_list_of_graph_term g1) in
    let adj2 = adjacency_list_of_graph_term g2 in
    if not @@ check_frequencies adj1 adj2 then false
    else
      generate_and_check_mappings adj1 adj2

(*
   I am not sure that it is correctly implemented. It does typecheck though. and iso g g = true.
   This is the end (near term) of my 99 problems in ocaml. 
   They are too much time for what I want by doing them (learning the language)
*)