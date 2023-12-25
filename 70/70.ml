type 'a mult_tree = T of 'a * 'a mult_tree list

let rec string_of_tree = function
  | T (x, []) -> String.make 1 x ^ "^"
  | T (x, ts) -> String.make 1 x ^ List.fold_left (fun acc t -> acc ^ string_of_tree t) "" ts ^ "^"

let rec tree_of_string s =
  let rec aux s i l = 
    if i >= String.length s then List.rev l, i + 1
    else match s.[i] with
    | '^' -> List.rev l, i + 1
    | c -> 
      let ts, ni = aux s (i + 1) [] in
      aux s ni (T (c, ts) :: l)
  in
  List.hd @@ fst @@ aux s 0 []

let t = T ('a', [T ('f', [T ('g', [])]); T ('c', []);
  T ('b', [T ('d', []); T ('e', [])])]);; 

let test_string_of_tree () =
  assert (string_of_tree t = "afg^^c^bd^e^^^");
  assert (tree_of_string "afg^^c^bd^e^^^" = t)