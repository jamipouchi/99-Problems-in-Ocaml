let length_sort = List.sort (fun ll lr -> List.length ll - List.length lr)

let encode ls =
  let rec rev_encode prev count = function
    | [] -> []
    | [x] -> (x, count + 1) :: prev 
    | x1 :: x2 :: xs -> 
      if x1 = x2 then rev_encode prev (count + 1) (x2 :: xs)
      else rev_encode ((x1, count + 1) :: prev) 0 (x2 :: xs) in
    List.rev @@ rev_encode [] 0 ls

let decode = List.map (fun (l, f) -> l)

let by_freq (l1, f1) (l2, f2) = f1 - f2

let frequency_sort ls = decode @@ List.sort by_freq @@ encode ls