let rec compress = function
  | x1 :: x2 :: xs -> if x1 = x2 then compress @@ x1 :: xs else x1 :: (compress @@ x2 :: xs)
  | base -> base

let test_compress() =
  assert (compress [1;1;2;2;3;3;3;4;4;4;4] = [1;2;3;4]);
  assert (compress [1;2;3;4] = [1;2;3;4]);
  assert (compress [1;1;1;1;1;1;1;1;1;1;1;1;1;1;1] = [1])