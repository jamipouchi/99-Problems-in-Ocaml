let rev list = 
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs in
    aux [] list

let test_rev () =
  let list = [1; 2; 3; 4; 5] in
  let rev_list = [5; 4; 3; 2; 1] in
  assert (rev list = rev_list)