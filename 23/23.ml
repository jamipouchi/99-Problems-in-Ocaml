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

let rand_select ls count =
  let length = List.length ls in
  if count >= length then []
  else
    let idxs = pick_random_ordered [] length count in
    pick idxs ls
