let rec length = function
  | [] -> 0
  | x :: xs -> 1 + length xs

let test_length () =
  assert (length [] = 0);
  assert (length [1] = 1);
  assert (length [1; 2] = 2);
  assert (length [1; 2; 3] = 3)