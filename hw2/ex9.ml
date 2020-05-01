(* Exercise 9 - OK *)
type relationships = (string * string) list;;

let selflove : relationships -> int = fun r ->
  (* get all uniq nodes *)
  let rec uniq l =
    let rec remove (f,s) l =
      match l with
      [] -> []
      | (x,y)::t -> if x = f then remove (f,s) t
                else (x,y)::remove (f,s) t 
    in match l with
    [] -> []
    | (x,y)::t -> x::uniq (remove (x,y) t)
  in let rec is_selflove r p h =
    let rec search r p =
      match r with
      [] -> []
      | (x,y)::t -> if x = p then [(x,y)] @ search t p
                    else search t p
    in let rec remove_list l r =
    match l with
    [] -> r
    | h::t -> let rec remove l e =
                match l with
                [] -> []
                | h::t -> if h = e then remove t e
                          else h::remove t e
              in remove_list t (remove r h)
    in let rec iter_list sl rl i =
      match sl with
      [] -> false
      | (x,y)::t -> if y = i then true || (is_selflove rl y i) || (iter_list t rl i)
                    else (is_selflove rl y i) || (iter_list t rl i)
    in iter_list (search r p) (remove_list (search r p) r) h
  in let rec count r p =
    match p with
    [] -> 0
    | h::t -> if is_selflove r h h then 1 + count r t
              else count r t
  in count r (uniq r)