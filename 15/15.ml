let rec repeat times x = match times with
    | 0 -> []
    | c -> x :: repeat (times - 1) x

let rec concat l1 l2 = match l1 with
    | [] -> l2
    | x :: xs -> x :: concat xs l2

let rec replicate ls times = match ls with
    | [] -> []
    | x :: xs -> concat (repeat times x) (replicate xs times)

let test_replicate() =
  assert (replicate ["a";"b";"c"] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"])