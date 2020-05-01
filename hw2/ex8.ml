(* Exercise 8 - OK *)
type relationships = (string * string) list;;
type person = string;;

let likes : relationships -> person -> int = fun r p ->
  let rec likes_tmp r p =
    (* return list of elements starting with p *)
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
    in let rec iter_list sl rl =
      match sl with
      [] -> []
      | (x,y)::t -> (x,y)::(likes_tmp rl y) @ (iter_list t rl)
    in iter_list (search r p) (remove_list (search r p) r)
  in let rec count_uniq l =
    (* remove all elements having same y *)
    let rec remove_same_y y l =
      match l with
      [] -> []
      | (x',y')::t -> if y' = y then remove_same_y y t
                      else (x',y')::remove_same_y y t   
    in match l with
    [] -> 0
    | (x,y)::t -> 1 + count_uniq(remove_same_y y t)
  in count_uniq (likes_tmp r p)