(* Exercise 3 - OK *)
let partition p l =
  let rec part p (l1, l2) l =
    match l with
    [] -> (l1, l2)
    | h::t -> if p h then part p (l1 @ [h], l2) t
              else part p (l1, l2 @ [h]) t
    in part p ([], []) l;;