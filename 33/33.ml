let rec gcd l r =
  if l < r then gcd r l
  else if r = 0 then l
  else gcd r (l mod r)

let coprime a b = gcd a b = 1
