type 'a mult_tree = T of 'a * 'a mult_tree list

let ipl t =
  let rec ipl_from h = function
    | T(_,[]) -> h
    | T(_, ts) -> List.fold_left (fun acc t -> acc + ipl_from (h+1) t) h ts in
  ipl_from 0 t

let t = T ('a', [T ('f', [T ('g', [])]); T ('c', []);
  T ('b', [T ('d', []); T ('e', [])])]);; 

let test_ipl () =
  assert (ipl t = 9)