let rec is_prime_from x p =
  if x * x > p then true
  else if p mod x = 0 then false
  else is_prime_from (x+1) p

let is_prime x =
  if x < 2 then false 
  else is_prime_from 2 x