let rec remove_at i = function
  | [] -> []
  | x :: xs -> if i = 0 then xs else x :: remove_at (i - 1) xs

let test_rec() =
  assert (remove_at 1 ["a";"b";"c";"d"] = ["a"; "c"; "d"]);