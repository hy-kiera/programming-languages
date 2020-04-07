(* Exercise 10 - OK *)
let rec comb a l =
  match l with
  [] -> []
  | h::t -> [(a, h)] @ comb a t;;

let rec cartesian a b =
  match a with
  [] -> []
  | h::t -> comb h b @ cartesian t b;;