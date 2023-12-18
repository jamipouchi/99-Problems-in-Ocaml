let add_all v ls =
  if List.is_empty ls then [[v]]
  else List.map (fun ls -> v :: ls) ls

(* As a present I also created all permutations for a list! This took me much longer than what I was asked for ^^ *)
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


let rec extract n ls = 
  match (n, ls) with
  | (0, _) -> []
  | (_, []) -> raise Exit
  | (c, xs) when c = List.length xs -> [xs]
  | (c, x :: xs) -> (add_all x (extract (c-1) xs)) @ (extract c xs)

(*
   The easiest language to do this in would be Prolog. LOL
*)