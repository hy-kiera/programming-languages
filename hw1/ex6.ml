(* Exercise 6 - OK *)
let rec notexists n t =
  match t with
  Empty -> true
  | Node (e, l, r) ->
    if n = e then false
    else notexists n l && notexists n r;;