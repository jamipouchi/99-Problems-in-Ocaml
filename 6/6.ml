let is_palindrome list = List.rev list = list

let test_palindrome () =
  assert (is_palindrome ["x"; "a"; "m"; "a"; "x"]);
  assert (not (is_palindrome [ "a" ; "b" ]))