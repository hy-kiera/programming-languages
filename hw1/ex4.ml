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