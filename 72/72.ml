type 'a mult_tree = T of 'a * 'a mult_tree list

let rec bottom_up = function
  | T(v,[]) -> [v]
  | T(v,ts) -> List.concat_map bottom_up ts @ [v]

let t = T ('a', [T ('f', [T ('g', [])]); T ('c', []);
  T ('b', [T ('d', []); T ('e', [])])]);; 

let test_bottom_up =
  assert (bottom_up (T ('a', [T ('b', [])])) = ['b'; 'a']);
  assert (bottom_up t = ['g'; 'f'; 'c'; 'd'; 'e'; 'b'; 'a']);;
