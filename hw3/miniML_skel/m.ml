type program = exp
and exp = 
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
  | NEWREF of exp 
  | DEREF of exp
  | SETREF of exp * exp
  | SEQ of exp * exp
  | BEGIN of exp
and var = string

type value = 
    Int of int 
  | Bool of bool 
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env
  | Loc of loc
and loc = int
and env = (var * value) list
and mem = (loc * value) list

(* conversion of value to string *)
let value2str v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Loc l -> "Loc "^(string_of_int l)
  | Procedure (x,e,env) -> "Procedure "
  | RecProcedure (f,x,e,env) -> "RecProcedure "^f

(* environment *)
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x = 
  match e with
  | [] -> raise (Failure (x ^ " is unbound in Env"))
  | (y,v)::tl -> if x = y then v else apply_env tl x

(* memory *)
let empty_mem = [] 
let extend_mem (l,v) m = (l,v)::m
let rec apply_mem m l = 
  match m with
  | [] -> raise (Failure ("Location " ^ string_of_int l ^ " is unbound in Mem"))
  | (y,v)::tl -> if l = y then v else apply_mem tl l

(* use the function 'new_location' to generate a fresh memory location *)
let counter = ref 0
let new_location () = counter:=!counter+1;!counter

exception NotImplemented
exception UndefinedSemantics (* whenever the semantics is undefined *)


(* my exceptions *)
exception DivisionByZero
exception TypeError

(* my functions *)
let getvalue e =
  match e with
  (v, _) -> v

let getmem e =
  match e with
  (_, m) -> m

(*****************************************************************)
(* TODO: Implement the eval function. Modify this function only. *)
(*****************************************************************)
let rec eval : exp -> env -> mem -> value * mem
= fun exp env mem ->
  match exp with
  CONST c -> (Int c, mem)
  | VAR x -> ((apply_env env x), mem)
  | ADD (e1, e2) -> let e' = eval e1 env mem
                      in let e'' = eval e2 env (getmem e')
                          in (Int (int_of_string (value2str (getvalue e')) + int_of_string (value2str (getvalue e''))), (getmem e''))
  | SUB (e1, e2) -> let e' = eval e1 env mem
                      in let e'' = eval e2 env (getmem e')
                          in (Int (int_of_string (value2str (getvalue e')) - int_of_string (value2str (getvalue e''))), (getmem e''))
  | MUL (e1, e2) -> let e' = eval e1 env mem
                      in let e'' = eval e2 env (getmem e')
                          in (Int (int_of_string (value2str (getvalue e')) * int_of_string (value2str (getvalue e''))), (getmem e''))
  | DIV (e1, e2) -> let e' = eval e1 env mem
                      in let e'' = eval e2 env (getmem e')
                          in if int_of_string (value2str (getvalue e'')) <> 0 then (Int (int_of_string (value2str (getvalue e')) / int_of_string (value2str (getvalue e''))), (getmem e''))
                              else raise DivisionByZero
  | ISZERO e -> let e' = eval e env mem
                  in if int_of_string (value2str (getvalue e')) = 0 then (Bool true, (getmem e'))
                      else (Bool false, (getmem e'))
  | READ -> let input = read_int()
              in (Int input, mem)
  | IF (e1, e2, e3) -> let cond = eval e1 env mem
                          in if value2str (getvalue cond) = "true" then eval e2 env (getmem cond)
                              else if value2str (getvalue cond) = "false" then eval e3 env (getmem cond)
                              else raise TypeError
  | LET (x, e1, e2) -> let e' = eval e1 env mem
                          in eval e2 (extend_env (x, (getvalue e')) env) (getmem e')
  | LETREC(f, x, e1, e2) -> eval e2 (extend_env (f, RecProcedure (f, x, e1, env)) env) mem
  | PROC (x, e) -> (Procedure (x, e, env), mem)
  | CALL (e1, e2) -> let e' = eval e1 env mem
                        in let e'' = eval e2 env (getmem e')
                            in let res = match getvalue e' with
                                          Procedure (x, e, env') -> eval e (extend_env (x, (getvalue e'')) env') (getmem e'')
                                          | RecProcedure (f, x, e, env') -> let newenv = extend_env (f, RecProcedure (f, x, e, env')) env'
                                                                                in eval e (extend_env (x, getvalue e'') newenv) (getmem e'')
                                          | _ -> raise TypeError
                                  in res
  | NEWREF e -> let e' = eval e env mem
                  in let l = new_location()
                      in (Loc l, (extend_mem (l, (getvalue e')) (getmem e')))
  | DEREF e -> let e' = eval e env mem
                  in let v = match getvalue e' with
                              Loc l -> apply_mem (getmem e') l
                              | _ -> raise TypeError
                      in (v, getmem e')
  | SETREF (e1, e2) -> let e' = eval e1 env mem
                          in let e'' = eval e2 env (getmem e')
                              in let m = match getvalue e' with
                                          Loc l' -> extend_mem (l', (getvalue e'')) (getmem e'')
                                          | _ -> raise TypeError
                                  in ((getvalue e''), m)
  | SEQ (e1, e2) -> let e = eval e1 env mem
                      in eval e2 env (getmem e)
  | BEGIN e -> eval e env mem

(* driver code *)
let run : program -> value
= fun pgm -> (fun (v,_) -> v) (eval pgm empty_env empty_mem) 
