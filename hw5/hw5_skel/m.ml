type program = exp
and exp = 
	| TRUE
	| FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | ISZERO of exp
  | READ
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
and var = string

exception TypeError

type typ = TyInt | TyBool 
	| TyFun of typ * typ (* t1 -> t2 *)
	| TyVar of tyvar
and tyvar = string
type typ_eqn = (typ * typ) list (* t1 = t2 *)

let rec string_of_type ty = 
  match ty with
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyFun (t1,t2) -> "(" ^ string_of_type t1 ^ " -> " ^ string_of_type t2 ^ ")"
  | TyVar x -> x

let print_typ_eqns eqns = 
  List.iter (fun (ty1,ty2) -> print_string (string_of_type ty1 ^ " = " ^ string_of_type ty2 ^ "\n")) eqns;
  print_endline ""

(* type environment : var -> type *)
module TEnv = struct
  type t = var -> typ
  let empty = fun _ -> raise (Failure "Type Env is empty")
  let extend (x,t) tenv = fun y -> if x = y then t else (tenv y)
  let find tenv x = tenv x
end

(* substitution *)
module Subst = struct
  type t = (tyvar * typ) list
  let empty = []
  let find x subst = List.assoc x subst

  (* walk through the type, replacing each type variable by its binding in the substitution *)
  let rec apply : typ -> t -> typ
  =fun typ subst ->
    match typ with
    | TyInt -> TyInt
    | TyBool -> TyBool 
    | TyFun (t1,t2) -> TyFun (apply t1 subst, apply t2 subst)
    | TyVar x -> 
      try find x subst
      with _ -> typ

  (* add a binding (tv,ty) to the substitution and propagate the information *)
  let extend tv ty subst = 
    (tv,ty) :: (List.map (fun (x,t) -> (x, apply t [(tv,ty)])) subst)

  let print : t -> unit
  =fun subst -> 
      List.iter (fun (x,ty) -> print_endline (x ^ " |-> " ^ string_of_type ty)) subst
end

let tyvar_num = ref 0

(* generate a fresh type variable *)
let fresh_tyvar () = (tyvar_num := !tyvar_num + 1; (TyVar ("t" ^ string_of_int !tyvar_num)))

(* my functions *)
let get_type ty = 
  match ty with
  | TyInt -> "TyInt"
  | TyBool -> "TyBool"
  | TyFun (t1,t2) -> "TyFun"
  | TyVar x -> "TyVar"

let tyfun_first x =
  match x with
  TyFun (f, _) -> f
  | _ -> raise TypeError

let tyfun_second x =
  match x with
  TyFun (_, s) -> s
  | _ -> raise TypeError

let count_string s x =
  let rec iter iter_str cnt =
    match (s, x) with
    (_, "") -> 0
    | (_, _) ->
      if iter_str = ((String.length s) - (String.length x)) then 
        if (String.sub s iter_str (String.length x)) = x then (cnt + 1)
        else cnt
      else
        if (String.sub s iter_str (String.length x)) = x then iter (iter_str + 1) (cnt + 1)
        else iter (iter_str + 1) cnt
    in iter 0 0;;

exception MyError

(* TODO *)
let rec gen_equations : TEnv.t -> exp -> typ -> typ_eqn 
=fun tenv e ty ->
  match e with
  TRUE | FALSE -> [(ty, TyBool)]
  | CONST c -> [(ty, TyInt)]
  | VAR x -> let tyX = (TEnv.find tenv x) in [(ty, tyX)]
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) -> [(ty, TyInt)] @ (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | ISZERO e -> [(ty, TyBool)] @ (gen_equations tenv e TyInt)
  | READ -> [(ty, TyInt)]
  | IF (e1, e2, e3) -> (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty)
  | LET (x, e1, e2) -> let alpha = fresh_tyvar() in
                        (gen_equations tenv e1 alpha) @ (gen_equations (TEnv.extend (x, alpha) tenv) e2 ty)
  | LETREC (f, x, e1, e2) -> let alpha1 = fresh_tyvar() in
                              let alpha2 = fresh_tyvar() in
                                let t = (gen_equations (TEnv.extend (f, TyFun (alpha2, alpha1)) tenv) e2 ty) in
                                  let new_tenv = (TEnv.extend (x, alpha2) tenv) in
                                    (gen_equations (TEnv.extend (f, TyFun (alpha2, alpha1)) new_tenv) e1 alpha1) @ t
  | PROC (x, e) -> let alpha1 = fresh_tyvar() in
                    let alpha2 = fresh_tyvar() in
                      [ty, TyFun (alpha1, alpha2)] @ (gen_equations (TEnv.extend (x, alpha1) tenv) e alpha2)
  | CALL (e1, e2) -> let alpha = fresh_tyvar() in
                      (gen_equations tenv e1 (TyFun (alpha, ty))) @ (gen_equations tenv e2 alpha)

(* TODO *)
let solve : typ_eqn -> Subst.t
=fun eqns -> let rec unify x y sub =
              if string_of_type x = string_of_type y then sub (* int = int, bool = bool, alpha = alpha *)
              else if (get_type x = "TyVar") then
                      if count_string (string_of_type y) (string_of_type x) > 0 then raise TypeError (* occurence check *)
                      else Subst.extend (string_of_type x) y sub
              else if ((get_type x = "TyInt" || get_type x = "TyBool" || get_type x = "TyFun") && get_type y = "TyVar") then unify y x sub
              else if (get_type x = "TyFun" && get_type y = "TyFun") then let s' = unify (tyfun_first x) (tyfun_first y) sub in
                                                                        let s'' = unify (Subst.apply (tyfun_second x) s') (Subst.apply (tyfun_second y) s') s' in
                                                                          s''
              else raise TypeError
            in let rec unifyall eqns sub =
                match eqns with
                [] -> sub
                | (x,y)::t -> let s' = unify (Subst.apply x sub) (Subst.apply y sub) sub in
                                unifyall t s'
              in unifyall eqns Subst.empty


(* typeof: Do not modify this function *)
let typeof : exp -> typ 
=fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations TEnv.empty exp new_tv in
  let _ = print_endline "= Equations = ";
          print_typ_eqns eqns in
  try 
    let subst = solve eqns in
    let ty = Subst.apply new_tv subst in
     print_endline "= Substitution = ";
      Subst.print subst;
      print_endline "";
      print_endline ("Type of the given program: " ^ string_of_type ty);
      print_endline "";
      ty
  with TypeError -> (print_endline "The program does not have type. Rejected."); exit (1)
