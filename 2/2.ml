let rec last_two = function
  | [] | [_] -> None
  | [x; y] -> Some (x,y)
  | _ :: t -> last_two t

let test_last_two() =
  assert (last_two [ "a" ; "b" ; "c" ; "d" ] = Some ("c", "d"));
  assert (last_two [ "a" ] = None)
