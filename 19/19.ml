let split ls count =
  let rec splitTo acc count = function
    | [] -> List.rev acc, []
    | (x :: xs as l) -> if count = 0 then (List.rev acc, l) else splitTo (x :: acc) (count - 1) xs in
  splitTo [] count ls

let rotate ls i =
  if i < 0 then
    let (l, r) = split ls (List.length ls + i) in
  r @ l
  else
  let (l, r) = split ls i in
  r @ l

let test_rotate() =
  assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]);
  assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2) = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"])