let split ls count =
  let rec splitTo acc count = function
    | [] -> List.rev acc, []
    | (x :: xs as l) -> if count = 0 then (List.rev acc, l) else splitTo (x :: acc) (count - 1) xs in
  splitTo [] count ls

let test_split() =
  assert (split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]));
  assert (split ["a"; "b"; "c"; "d"] 5 =  (["a"; "b"; "c"; "d"], []))