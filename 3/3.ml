let rec at idx = function
  | [] -> None
  | x :: xs -> if idx = 0 then Some x else at (idx - 1) xs

let test_at () =
  let xs = [1; 2; 3; 4; 5] in
  assert (at 0 xs = Some 1);
  assert (at 1 xs = Some 2);
  assert (at 2 xs = Some 3);
  assert (at 3 xs = Some 4);
  assert (at 4 xs = Some 5);
  assert (at 5 xs = None);
  assert (at 6 xs = None)