let rec range l r =
  if r < l then l :: range (l - 1) r
  else if l == r then [l]
  else l :: range (l+1) r

let test_range() =
  assert (range 4 9 = [4;5;6;7;8;9]);
  assert (range 9 4 = [9;8;7;6;5;4]);
