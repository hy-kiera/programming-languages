(* Exercise 1 - OK*)
let rec rev_append l1 l2 =
  match l1 with
  [] -> l2
  | h::t -> rev_append t (h::l2);;

(* Exercise 2 - OK *)
let rec range lower upper =
  if lower = upper then [lower]
  else if lower > upper then []
  else [lower] @ range (lower+1) upper;;

(* Exercise 3 - OK *)
let partition p l =
  let rec part p (l1, l2) l =
    match l with
    [] -> (l1, l2)
    | h::t -> if p h then part p (l1 @ [h], l2) t
              else part p (l1, l2 @ [h]) t
    in part p ([], []) l;;

(* Solution *)
let rec partition p l =
  match l with
  [] -> ([], [])
  | h::t -> let (ts, fs) = partition p t in
            if p h then (h::ts, fs)
            else (ts, h::fs)


(* Exercise 4 - OK *)
type formula = TRUE | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr;;

let rec compute e =
  match e with
  NUM e -> e
  | PLUS (le, re) -> compute le + compute re
  | MINUS (le, re) -> compute le - compute re;;

let rec eval f =
  match f with
  TRUE -> true
  | FALSE -> false
  | NOT f' ->
    if eval f' = true then false
    else true
  | ANDALSO (lf', rf') -> eval lf' && eval rf'
  | ORELSE (lf', rf') -> eval lf' || eval rf'
  | IMPLY (lf', rf') ->
      if eval lf' = true && eval rf' = false then false
      else  true
  | LESS (le, re) ->
      if compute le < compute re then true
      else false;;

(* Exercise 5 - OK*)
type btree = Empty | Node of int * btree * btree;;

let rec height t =
  match t with
  Empty -> 0
  | Node (n, l, r) ->
    if height l > height r then height l + 1
    else height r + 1;;

(* Exercise 6 - OK *)
let rec notexists n t =
  match t with
  Empty -> true
  | Node (e, l, r) ->
    if n = e then false
    else notexists n l && notexists n r;;

(* Exercise 7 - OK *)
let rec fold3 f a l1 l2 l3 =
  match (l1, l2, l3) with
  ([], [], []) -> a
  | (h1::t1, h2::t2, h3::t3) -> fold3 f (f a h1 h2 h3) t1 t2 t3;;

(* triple match *)
let rec fold3 f a l1 l2 l3 =
  match l1 with
  [] -> a
  | h1::t1 -> (match l2 with
            [] -> a
            | h2::t2 -> (match l3 with
                      [] -> a
                      | h3::t3 -> fold3 f (f a h1 h2 h3) t1 t2 t3
            )
  )

(* Exercise 8 - OK *)
let rec iter n f x =
  if n = 0 then x
  else iter (n-1) f (f x);;

(* Exercise 9 - OK *)
let rec sigma (a, b, f) =
  match (a, b, f) with
  (a', b', f') -> if a' = b' then f' a'
                else if a' > b' then 0
                else f' a' + sigma ((a'+1), b', f');;

(* Exercise 10 - OK *)
let rec comb a l =
  match l with
  [] -> []
  | h::t -> [(a, h)] @ comb a t;;

let rec cartesian a b =
  match a with
  [] -> []
  | h::t -> comb h b @ cartesian t b;;

(* Solution *)
let rec map f l =
  match l with
  [] -> []
  | h::t -> (f h)::(map f t)

let rec cartesian a b =
  match a with
  [] -> []
  (* Single list *)
  | h::[] -> map (func x -> (h, x)) b
  | h::t -> (cartesian [h] b) @ (cartesian t b)