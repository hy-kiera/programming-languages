(* Exercise 3 - OK *)
let to_list s =
  let rec iter_str iter =
    match s with
    "" -> []
    | _ -> if iter + 1 = String.length s then [s.[iter]]
          else s.[iter] :: iter_str (iter+1)
    in iter_str 0;;