let rec duplicate = function
  | [] -> []
  | x :: xs -> x :: x :: duplicate xs

let test_duplicate() =
  assert (duplicate [] = []);
  assert (duplicate [1] = [1; 1]);
  assert (duplicate [1; 2] = [1; 1; 2; 2]);
  assert (duplicate [1; 2; 3] = [1; 1; 2; 2; 3; 3]);