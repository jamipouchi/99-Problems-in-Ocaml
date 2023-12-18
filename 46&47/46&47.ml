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

let table2 f s expr =
  let create_maps f s = [
    StringMap.empty |> StringMap.add f true |> StringMap.add s true;
    StringMap.empty |> StringMap.add f true |> StringMap.add s false;
    StringMap.empty |> StringMap.add f false |> StringMap.add s true;
    StringMap.empty |> StringMap.add f false |> StringMap.add s false;
  ] in
  create_maps f s |> List.map (fun map -> (List.map snd @@ StringMap.to_list map, eval map expr))

let test_table2 () =
  assert (table2 "a" "b" (And (Var "a", Or (Var "a", Var "b"))) = [
    ([true; true], true);
    ([true; false], true);
    ([false; true], false);
    ([false; false], false);
  ])