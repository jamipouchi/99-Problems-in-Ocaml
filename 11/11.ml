type 'a rle =
  | One of 'a
  | Many of int * 'a;;

  let encode ls =
    let to_rle x count =
      if count = 1 then One x
      else Many (count, x) in
    let rec rev_encode prev count = function
      | [] -> []
      | [x] -> (to_rle x (count + 1)) :: prev 
      | x1 :: x2 :: xs -> 
        if x1 = x2 then rev_encode prev (count + 1) (x2 :: xs)
        else rev_encode ((to_rle x1 (count + 1)) :: prev) 0 (x2 :: xs) in
    List.rev @@ rev_encode [] 0 ls

let test_encode () =
  assert (encode [] = []);
  assert (encode [1] = [One 1]);
  assert (encode [1; 1] = [Many (2, 1)]);
  assert (encode [1; 1; 2] = [Many (2, 1); One 2]);
  assert (encode 
  ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] =
  [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
)
