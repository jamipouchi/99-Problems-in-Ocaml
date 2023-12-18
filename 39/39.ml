let is_prime x =
 let rec is_prime_from x p =
  if x * x > p then true
  else if p mod x = 0 then false
  else is_prime_from (x+1) p in
  if x < 2 then false 
  else is_prime_from 2 x

let rec all_primes l r =
  if l > r then all_primes r l
  else if l = r then []
  else if is_prime l then l :: all_primes (l+1) r
  else all_primes (l+1) r

let test_all_primes () =
  assert (List.length (all_primes 2 7920) = 1000);;