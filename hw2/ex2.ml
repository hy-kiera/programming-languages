(* Exercise 2 - OK *)
type ae = CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list;;

let rec isexist l x =
  match l with
  [] -> false
  | h::t -> let rec iter f x =
              match f with
              CONST c -> false
              | VAR v -> if v = x then true else false
              | POWER (d, e) -> if d = x then true else false
              | TIMES l -> isexist l x
              | SUM l -> isexist l x
            in iter h x || isexist t x;;

let rec remove x l =
  match l with
  [] -> []
  | h::t -> match h with
            CONST c -> h::remove x t
            | VAR v -> if x = v then remove x t else h::remove x t
            | POWER (d, e) -> if x = d then remove x t else h::remove x t
            | TIMES t -> h::remove x t
            | SUM s -> h::remove x t

let change x cnt =
  match x with
  CONST c -> x
  | VAR v -> POWER (v, cnt)
  | POWER (d, e) -> POWER (d, cnt)
  | TIMES t -> x
  | SUM s -> x

let rec count x l =
  match l with
  [] -> 0
  | h::t -> match h with
            CONST c -> count x t 
            | VAR v -> if v = x then 1 + count x t else count x t
            | POWER (d,e) -> if d = x then e + count x t else count x t
            | TIMES t -> count x t
            | SUM t -> count x t

let rec multiple l =
  match l with
  [] -> []
  | h::t -> match h with
            CONST c -> h::multiple t
            | VAR v -> (change (VAR v) (count v l)) :: multiple (remove v t)
            | POWER (d, e) -> (change (POWER (d, e)) (count d l)) :: multiple (remove d t)
            | TIMES t -> h::multiple t
            | SUM t -> h::multiple t

let diff (f, x) =
  let rec do_diff (f, x) ae =
    match f with
    CONST c -> CONST 0
    | VAR v -> if v = x then CONST 1
                else CONST 0
    | POWER (d, e) -> if d = x then TIMES [CONST e; POWER (d, e-1)]
                      else CONST 0 
    | TIMES l -> if isexist l x then 
                    let rec times l' x ae =
                      match l' with
                      [] -> TIMES ae
                      | h::t -> let rec iter f x =
                                  match f with
                                  CONST c -> CONST c
                                  | VAR v -> if v = x then CONST 1
                                              else VAR v
                                  | POWER (d, e) -> if d = x then if e - 1 = 0 then CONST 1
                                                                  else TIMES [CONST e; POWER (d, e-1)]
                                              else POWER (d, e)
                                  | TIMES _ -> do_diff (f, x) []
                                  | SUM _ -> do_diff (f, x) []
                                in times t x ((iter h x)::ae)
                      in times (multiple l) x []
                  else CONST 0
    | SUM l -> if isexist l x then
                  let rec sum l x ae = 
                    match l with
                    [] -> SUM ae
                    | h::t -> sum t x ((do_diff (h, x) ae)::ae)
                  in sum l x ae
                else CONST 0
  in do_diff (f, x) [];;