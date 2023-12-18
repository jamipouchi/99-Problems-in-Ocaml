type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;

module StringMap = Map.Make (String)

let rec eval (map: bool StringMap.t) = function
  Var str -> if StringMap.exists (fun key _ -> key = str) map then StringMap.find str map else raise Exit
  | Not expr -> not (eval map expr)
  | And (l,r) -> (eval map l) && (eval map r)
  | Or (l,r) -> (eval map l) || (eval map r)

let sized_bool_list_of_int n size =
  let rec fill l = function
    0 -> l
    | n -> fill (false::l) (n-1) in
  let rec sized_bool_list_to acc = function
    0 -> 
      let length = List.length acc in
        if length <= size then fill acc (size-length) else raise Exit
    | n -> sized_bool_list_to ((n mod 2 = 1)::acc) (n/2) 
  in sized_bool_list_to [] n

let table l expr =
  let length = List.length l in
  let bool_list_list = List.init (int_of_float (2. ** (float_of_int length))) (fun x -> x) 
    |> List.map (fun i -> sized_bool_list_of_int i length) in
  let map_list = 
    List.map (fun bool_list -> 
      List.fold_left2 (fun acc key value -> StringMap.add key value acc) StringMap.empty l bool_list)
       bool_list_list in
  List.map (fun map -> (StringMap.to_list map, eval map expr)) map_list