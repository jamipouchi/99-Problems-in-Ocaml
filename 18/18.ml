let slice ls s e =
  let rec sliceFrom i = function
    | [] -> []
    | x :: xs -> 
      if i < s then sliceFrom (i + 1) xs
      else if i <= e then x :: sliceFrom (i + 1) xs
      else [] in
  sliceFrom 0 ls

let show_slice() =
  slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6 

let test_slice() =
  assert (slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6 = ["c"; "d"; "e"; "f"; "g"])