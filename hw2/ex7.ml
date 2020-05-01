(* Exercise 7 - OK *)
let rec uniq l =
  let rec remove e l =
    match l with
    [] -> []
    | h::t -> if h = e then remove e t
              else h::remove e t 
  in match l with
  [] -> []
  | h::t -> h::uniq (remove h t);;