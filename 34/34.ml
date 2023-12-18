let rec gcd l r =
  if l < r then gcd r l
  else if r = 0 then l
  else gcd r (l mod r)

let coprime a b = gcd a b = 1

let phi = function
  1 -> 1
  | m -> List.init (m-1) (fun x -> x+1) |> List.filter (coprime m) |> List.length
