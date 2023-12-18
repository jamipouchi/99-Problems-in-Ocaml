let is_prime x =
  let rec is_prime_from x p =
   if x * x > p then true
   else if p mod x = 0 then false
   else is_prime_from (x+1) p in
   if x < 2 then false 
   else is_prime_from 2 x

let encode ls =
  let rec rev_encode prev count = function
    | [] -> []
    | [x] -> (x, count + 1) :: prev 
    | x1 :: x2 :: xs -> 
      if x1 = x2 then rev_encode prev (count + 1) (x2 :: xs)
      else rev_encode ((x1, count + 1) :: prev) 0 (x2 :: xs) in
    List.rev @@ rev_encode [] 0 ls

 
 let factors n =
   let rec factorsFrom i n =
     if i*i > n then [n]
     else if n mod i = 0 then i :: factorsFrom i (n/i)
     else factorsFrom (i + 1) n in
   if n = 1 then [] 
   else encode @@ factorsFrom 2 n
  
let rec power base exp =
  if base = 1 || exp = 0 then 1
  else if exp = 1 then base
  else let half_power = power base (exp/2) in
    if exp mod 2 = 0 then half_power*half_power
    else half_power*half_power*base

let phi_improved x =
  let phi_formula l =
    List.fold_left (fun acc (p, m) -> acc * (p-1)*(power p (m-1))) 1 l in
  factors x |> phi_formula;;

let test_phi_improved () =
  assert (phi_improved 10 = 4);
  assert (phi_improved 13 = 12);