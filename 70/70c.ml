type 'a mult_tree = T of 'a * 'a mult_tree list

let rec count_nodes (T (_, l)) = 
  List.fold_left (fun acc t -> acc + count_nodes t) 1 l

let test_count_nodes () =
  let t = T ('a', [T ('f', [])]) in
  assert (count_nodes t = 2)
