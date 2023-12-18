let add_all v ls =
  if List.is_empty ls then [[v]]
  else List.map (fun ls -> v :: ls) ls

let prep_list_to_all ls lols =
  List.map (fun lo -> [ls ; lo]) lols

let rec extract n ls = 
  match (n, ls) with
  | (0, _) -> []
  | (_, []) -> raise Exit
  | (c, xs) when c = List.length xs -> [xs]
  | (c, x :: xs) -> (add_all x (extract (c-1) xs)) @ (extract c xs)

let rec extractTo acc n ls = 
  match (n, ls) with
  | (0, rs) -> [(acc, rs)]
  | (_, []) -> raise Exit
  | (c, xs) when c = List.length xs -> [(acc @ xs, [])]
  | (c, x :: xs) ->
      let should_start_with_x = extractTo [] (n-1) xs in
      let start_with_x = List.map (fun (cmb, rs) -> (x :: cmb, rs)) should_start_with_x in
      let next_missing_x = extractTo [] n xs in
      let next_with_x = List.map (fun (cmb, rs) -> (cmb, x :: rs)) next_missing_x in
      start_with_x @ next_with_x

let group ls [n1; n2] =
  let first_extracted = extractTo [] n1 ls in
  List.map (fun (l1, rs) -> prep_list_to_all l1 (extract n2 rs)) first_extracted

(*
    Given n1 n2 ls, extract n1 from ls, which will give (ln1, rst) and then extraxt n2 from rst, which will give (ln1, ln2)
*)