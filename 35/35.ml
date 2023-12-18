let is_prime x =
 let rec is_prime_from x p =
  if x * x > p then true
  else if p mod x = 0 then false
  else is_prime_from (x+1) p in
  if x < 2 then false 
  else is_prime_from 2 x

let factors n =
  let rec factorsFrom i n =
    if i*i > n then [n]
    else if n mod i = 0 then i :: factorsFrom i (n/i)
    else factorsFrom (i + 1) n in
  if n = 1 then [] 
  else factorsFrom 2 n

let test_factors () =
  assert (factors 1 = []);
  assert (factors 2 = [2]);
  assert (factors 315 = [3; 3; 5; 7]);;