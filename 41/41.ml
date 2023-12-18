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

let rec goldbach_list l r =
  if l > r then goldbach_list r l
  else List.init (r-l+1) (fun idx -> idx+l)
    |> List.filter (fun x -> x mod 2 = 0) 
    |> List.map (fun x -> (x, goldbach x))

let test_goldbach_list () = 
  assert (goldbach_list 9 20 = [(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13)); (20, (3, 17))]);
