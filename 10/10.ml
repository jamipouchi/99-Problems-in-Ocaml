let encode ls =
  let rec rev_encode prev count = function
    | [] -> []
    | [x] -> (x, count + 1) :: prev 
    | x1 :: x2 :: xs -> 
      if x1 = x2 then rev_encode prev (count + 1) (x2 :: xs)
      else rev_encode ((x1, count + 1) :: prev) 0 (x2 :: xs) in
    List.rev @@ rev_encode [] 0 ls

let test_encode() =
  assert (encode [] = []);
  assert (encode [1] = [(1, 1)]);
  assert (encode [1; 1] = [(1, 2)]);
  assert (encode [1; 2] = [(1, 1); (2, 1)]);
  assert (encode [1; 1; 2] = [(1, 2); (2, 1)]);
  assert (encode 
  ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = 
  [("a", 4); ("b", 1); ("c", 2); ("a", 2); ("d", 1); ("e", 4)]
)