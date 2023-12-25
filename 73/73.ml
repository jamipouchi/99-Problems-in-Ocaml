type 'a mult_tree = T of 'a * 'a mult_tree list

let rec lispy = function
  | T(v,[]) -> String.make 1 v
  | T(v,ts) -> "(" ^ String.make 1 v ^ List.fold_left (fun acc t -> acc ^ " " ^ lispy t) "" ts ^ ")"

let t = T ('a', [T ('f', [T ('g', [])]); T ('c', []);
  T ('b', [T ('d', []); T ('e', [])])]);; 

let test_lispy () =
  assert(lispy (T ('a', [])) = "a");
  assert(lispy (T ('a', [T ('b', [])])) = "(a b)");
  assert (lispy t = "(a (f g) c (b d e))");;