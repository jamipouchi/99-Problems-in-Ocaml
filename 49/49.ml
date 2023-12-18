let add_zero_from_left = List.map (fun bin_str -> "0" ^ bin_str)
let add_one_from_right l = List.rev @@ List.map (fun bin_str -> "1" ^ bin_str) l

let rec gray = function
  n when n <= 0 -> raise Exit
  | 1 -> ["0"; "1"]
  | n -> let gray_n_1 = gray (n-1) in
    (add_zero_from_left gray_n_1) @ (add_one_from_right gray_n_1)

let test_gray () =
  assert (gray 1 = ["0"; "1"]);
  assert (gray 2 = ["00"; "01"; "11"; "10"]);
  assert (gray 3 = ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"]);;