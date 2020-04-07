(* Exercise 5 - OK*)
type btree = Empty | Node of int * btree * btree;;

let rec height t =
  match t with
  Empty -> 0
  | Node (n, l, r) ->
    if height l > height r then height l + 1
    else height r + 1;;