(* Exercise 9 - OK *)
let rec sigma (a, b, f) =
  match (a, b, f) with
  (a', b', f') -> if a' = b' then f' a'
                else if a' > b' then 0
                else f' a' + sigma ((a'+1), b', f');;