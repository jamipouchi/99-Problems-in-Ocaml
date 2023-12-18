let rec insert_at v i = function
  | [] -> [v]
  | x :: xs -> 
    if i = 0 then v :: x :: xs
    else x :: insert_at v (i - 1) xs

let test_insert_at() = 
  assert (insert_at "alfa" 1 ["a";"b";"c";"d"] = ["a"; "alfa"; "b"; "c"; "d"]);
  assert (insert_at "alfa" 3 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "alfa"; "d"]);
  assert (insert_at "alfa" 4 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "d"; "alfa"]);
  assert (insert_at "alfa" 0 ["a";"b";"c";"d"] = ["alfa"; "a"; "b"; "c"; "d"]);
  assert (insert_at "alfa" 5 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "d"; "alfa"]);

