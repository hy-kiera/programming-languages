open M

(* exp environment : var -> exp *)
module EEnv = struct
  type t = var -> exp
  (* let empty = fun _ -> raise (Failure "Exp Env is empty") *)
  let empty = fun _ -> FALSE
  let extend (x,t) eenv = fun y -> if x = y then t else (eenv y)
  let find eenv x = eenv x
end

(* my exception *)
exception NotImplemented

let expand: exp -> exp 
= fun exp ->
  let rec isx exp x =
    match exp with
    TRUE | FALSE -> false
    | CONST c -> false
    | VAR x' -> if x' = x then true
                else false
    | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) -> (isx e1 x) || (isx e2 x)
    | ISZERO e -> isx e x
    | READ -> false
    | IF (e1, e2, e3) -> (isx e1 x) || (isx e2 x) || (isx e3 x)
    | LET (x', e1, e2)-> if x' = x then true
                          else (isx e1 x) || (isx e2 x)
    | LETREC (f, x', e1, e2) -> if x' = x then true
                                else (isx e1 x) || (isx e2 x)
    | PROC (x', e) -> if x' = x then true
                        else isx e x
    | CALL (e1, e2) -> (isx e1 x) || (isx e2 x)
  in
    let rec trans exp eenv =
      match exp with
      TRUE | FALSE -> exp
      | CONST c -> exp
      | VAR x -> let isexist = EEnv.find eenv x in
                  if isexist = FALSE then exp
                  else isexist
      | ADD (e1, e2) -> ADD ((trans e1 eenv), (trans e2 eenv))
      | SUB (e1, e2) -> SUB ((trans e1 eenv), (trans e2 eenv))
      | MUL (e1, e2) -> MUL ((trans e1 eenv), (trans e2 eenv))
      | DIV (e1, e2) -> DIV ((trans e1 eenv), (trans e2 eenv))
      | ISZERO e -> ISZERO (trans e eenv)
      | READ -> exp
      | IF (e1, e2, e3) -> IF ((trans e1 eenv), (trans e2 eenv), (trans e3 eenv))
      | LET (x, e1, e2) -> let new_eenv = (EEnv.extend (x, (trans e1 eenv)) eenv) in
                            if isx e2 x then trans e2 new_eenv
                            else LET (x, (trans e1 new_eenv), (trans e2 new_eenv))
      | LETREC (f, x, e1, e2) -> exp
      | PROC (x, e) -> PROC (x, (trans e eenv))
      | CALL (e1, e2) -> CALL ((trans e1 eenv), (trans e2 eenv))
    in trans exp EEnv.empty


(* typeof: Do not modify this function *)
let typeof : exp -> typ 
=fun exp -> 
	let exp' = expand exp in 
	M.typeof exp'  
