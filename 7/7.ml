type 'a node =
    | One of 'a 
    | Many of 'a node list;;

let rec concat_map f l =
  let rec prep_concat_map ys f xs = match ys with
    | [] -> concat_map f xs
    | y :: ys -> y :: (prep_concat_map ys f xs) in match l with
    | [] -> []
    | x :: xs -> prep_concat_map (f x) f xs

let flatten ls = 
  let rec flatten_one = function
    | One x -> [x]
    | Many [] -> []
    | Many (xs) -> concat_map flatten_one xs in
  concat_map flatten_one ls

let test_flatten() = 
  assert ((flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]) = ["a"; "b"; "c"; "d"; "e"]);
  assert ((flatten [One "a"]) = ["a"]);