let drop ls at =
  let rec drop_count count ls = match (count, ls) with
    | (_, []) -> []
    | (0, x :: xs) -> drop_count (at - 1) xs
    | (c, x :: xs) -> x :: drop_count (c - 1) xs in
  drop_count (at - 1) ls

let test_drop() =
  assert (drop [1; 2; 3; 4; 5; 6; 7; 8; 9] 3 = [1; 2; 4; 5; 7; 8]);
  assert (drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 = ["a"; "b"; "d"; "e"; "g"; "h"; "j"])