(* Exercise 6 - OK *)
type btree = Empty | Node of int * btree * btree

let rec check tree =
  let rec height t =
    match t with
    Empty -> 0
    | Node (n, l, r) ->
      if height l > height r then height l + 1
      else height r + 1
  in match tree with
  Empty -> true
  | Node (n, lt, rt) -> 
    if abs (height lt - height rt) > 1 then false
    else check lt && check rt;;