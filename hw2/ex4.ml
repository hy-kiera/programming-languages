(* Exercise 4 -OK *)
let repeat s cnt =
  let rec iter_str iter =
    match s with
    "" -> ""
    | _ -> if cnt = 0 then ""
          else if iter + 1 = cnt then s
          else s ^ iter_str (iter+1)
    in iter_str 0;;