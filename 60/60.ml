let rec min_nodes = function
  | 0 -> 0
  | 1 -> 1
  | h -> 1 + min_nodes(h-2) + min_nodes (h-1)

let min_height n = int_of_float (ceil (log (float(n + 1)) /. log 2.))

let max_height n = 
  let rec max_height_aux n h = 
    if min_nodes h > n then h - 1
    else max_height_aux n (h + 1) in
  max_height_aux n 0
