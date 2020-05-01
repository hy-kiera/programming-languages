(* Exercise 5 - OK *)
let count_string s x =
  let rec iter iter_str cnt =
    match (s, x) with
    (_, "") -> 0
    | (_, _) ->
      if iter_str = ((String.length s) - (String.length x)) then 
        if (String.sub s iter_str (String.length x)) = x then (cnt + 1)
        else cnt
      else
        if (String.sub s iter_str (String.length x)) = x then iter (iter_str + 1) (cnt + 1)
        else iter (iter_str + 1) cnt
    in iter 0 0;;
