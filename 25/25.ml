let rec extract acc n = function
      | [] -> raise Not_found
      | h :: t -> if n = 0 then (h, acc @ t) else extract (h :: acc) (n - 1) t

let pick_one ls =
  let length = List.length ls in
  let idx = Random.int length in
  extract [] idx ls

let permutation ls =
  let pass_one l r = 
    let (x, xs) = pick_one l in
    (xs, x :: r) in
    let rec permuteTo acc = function
      | [] -> acc
      | xs -> 
        let (new_xs, new_acc) = pass_one xs acc in
          permuteTo new_acc new_xs in
    permuteTo [] ls