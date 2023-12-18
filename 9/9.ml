let rec pack ls = 
  let rec packTo prev curr = function
    | [] -> [] (** Only reachable from initially empty list *)
    | [x] -> (x :: curr) :: prev
    | x1 :: x2 :: xs -> 
      if x1 = x2 then packTo prev (x1 :: curr) (x2 :: xs) 
      else packTo ((x1 :: curr) :: prev) [] (x2 :: xs) in
  List.rev @@ packTo [] [] ls

let test_pack() =
  assert (pack [] = []);
  assert (pack [1] = [[1]]);
  assert (pack [1; 1] = [[1; 1]]);
  assert (pack [1; 2] = [[1]; [2]]);
  assert (pack [1; 1; 2] = [[1; 1]; [2]]);