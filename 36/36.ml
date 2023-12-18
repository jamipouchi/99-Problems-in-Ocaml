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

let test_factors () =
  assert (factors 315 = [(3, 2); (5, 1); (7, 1)]);;