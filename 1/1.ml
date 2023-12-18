let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: xs -> last xs

let test_last () =
  assert (last [] = None);
  assert (last [1] = Some 1);
  assert (last [1; 2] = Some 2);
  assert (last [1; 2; 3] = Some 3);
  assert (last [1; 2; 3; 4] = Some 4);
  assert (last [1; 2; 3; 4; 5] = Some 5)
