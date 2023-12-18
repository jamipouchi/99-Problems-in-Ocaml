type 'a rle =
    | One of 'a
    | Many of int * 'a

let decode ls =
  let rec prep_rle_to_list ls = function
    | One v | Many (1, v) -> v :: ls
    | Many (c, v) -> prep_rle_to_list (v :: ls) (Many (c-1, v)) in
  let rec rev_decode acc = function
    | [] -> []
    | [x] -> prep_rle_to_list acc x
    | x :: xs -> rev_decode (prep_rle_to_list acc x) xs in
  List.rev @@ rev_decode [] ls

let test_decode() = 
  assert (decode [] = []);
  assert (decode [One "a"] = ["a"]);
  assert (decode [Many (4, "a")] = ["a"; "a"; "a"; "a"]);
  assert (decode [One "a"; One "b"; One "c"] = ["a"; "b"; "c"]);
  assert (decode [Many (4, "a"); One "b"; One "c"; Many (2, "a")] = ["a"; "a"; "a"; "a"; "b"; "c"; "a"; "a"]);