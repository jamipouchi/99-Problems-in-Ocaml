let is_prime x =
 let rec is_prime_from x p =
  if x * x > p then true
  else if p mod x = 0 then false
  else is_prime_from (x+1) p in
  if x < 2 then false 
  else is_prime_from 2 x

let goldbach n = 
  let rec is_goldbach_of_n i = 
    if is_prime i && is_prime (n - i) then (i, n - i)
    else is_goldbach_of_n (i + 1)
  in is_goldbach_of_n 2;;

let test_goldbach () =
  assert (goldbach 4 = (2, 2));
  assert (goldbach 8 = (3, 5));;