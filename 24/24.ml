let pick idxs ls =
  let rec pick_from i ls idxs = match (ls, idxs) with
    | (xs, []) -> []
    | ([], _) -> raise Exit(* this should not be reachable.*)
    | (x :: xs, idx :: idxs) -> 
      if i = idx then x :: pick_from (i+1) xs idxs
      else pick_from (i + 1) xs (idx :: idxs) in
  pick_from 0 ls idxs

(* This keeps the list sorted. This is to avoid adding a duplicate element *)
let rec add_to_ordered_selection v = function
  | [] -> [v]
  | x :: xs -> 
    if x <= v then x :: add_to_ordered_selection (v + 1) xs
    else v :: x :: xs


let rec pick_random_ordered acc bound = function
  | 0 -> acc
  | c ->  pick_random_ordered (add_to_ordered_selection (Random.int bound) acc) (bound - 1) (c - 1)

(* All top methods are identical to ex. 23 *)

let lotto_select qt max =
  let zero_idx = pick_random_ordered [] (max - 1) qt in
  List.map ((+) 1) zero_idx