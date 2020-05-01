(* Exercise 1 -OK *)
type exp = X | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp;;

module Env = struct
  type t = (string * float) list
  let empty = []
  let rec get x e =
    match e with
    | [] -> raise (Failure ("FreeVariable"))
    | (y,v)::t -> if y = x then v
                  else get x t
  let update x v e = (x,v)::e
end

let rec calculate x =
  let rec subcal f env =
    match f with
    X -> (Env.get "X" env)
    | INT i -> float_of_int i
    | REAL f -> f
    | ADD (e1, e2) -> subcal e1 env +. subcal e2 env
    | SUB (e1, e2) -> subcal e1 env -. subcal e2 env
    | MUL (e1, e2) -> subcal e1 env *. subcal e2 env
    | DIV (e1, e2) -> subcal e1 env /. subcal e2 env
    | SIGMA (s, e, f) -> if subcal s env > subcal e env then 0.0
                          else subcal f (Env.update "X" (subcal s env) env) +. subcal (SIGMA (ADD (s, INT 1), e, f)) env
    | INTEGRAL (s, e, f) -> if subcal s env > subcal e env then 0.0
                          else if subcal s env >= subcal (SUB (e, REAL 0.1)) env then 0.0
                          else (0.1 *. subcal f (Env.update "X" (subcal s env) env)) +. subcal (INTEGRAL (ADD (s, REAL 0.1), e, f)) env
  in subcal x Env.empty
