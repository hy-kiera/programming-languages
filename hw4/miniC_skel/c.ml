type program = exp
and exp =
	| SKIP (* unit *)
	| TRUE
	| FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
	| LE of exp * exp
	| EQ of exp * exp
	| NOT of exp 
  | IF of exp * exp * exp
	| WHILE of exp * exp 
	| LET of var * exp * exp
	| PROC of var list * exp 
	| CALLV of exp * exp list 
	| CALLR of exp * var list
	| ASSIGN of var * exp 
	| RECORD of (var * exp) list 
	| FIELD of exp * var
	| ASSIGNF of exp * var * exp 
  | READ of var
	| PRINT of exp 
  | SEQ of exp * exp
  | BEGIN of exp
and var = string

type value = 
    Int of int
  | Bool of bool
	| Unit
  | Procedure of var list * exp * env
	| Record of record
  | Loc of loc
and loc = int 
and env = (var * loc) list
and mem = (loc * value) list
and record = (var * loc) list

(* conversion of value to string *)
let value2str v =
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unit -> "."  
	| Procedure (params,e,env) -> "Procedure "
  | Record record -> "Record "
	| Loc l -> "Loc "^(string_of_int l)

(* environment *)
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x = 
  match e with
  | [] -> raise (Failure (x ^ " is unbound in Env"))
  | (y,v)::tl -> if x = y then v else apply_env tl x

(* memory *)
let empty_mem = [] 
let extend_mem (l,v) m = (l,v)::(List.filter (fun (l',_) -> l != l') m)
let rec apply_mem m l = 
  match m with
  | [] -> raise (Failure ("Location " ^ string_of_int (l) ^ " is unbound in Mem"))
  | (y,v)::tl -> if l = y then v else apply_mem tl l

let counter = ref 0
let new_location () = counter:=!counter+1; (!counter)

(* conversion of env to string *)
let string_of_env env = 
	List.fold_left (fun str (x,l) -> Printf.sprintf "%s\n%s -> %d" str x l) "" env  
(* conversion of mem to string *)
let string_of_mem mem = 
	List.fold_left (fun str (l,v) -> Printf.sprintf "%s\n%d -> %s" str l (value2str v)) "" mem 		

exception NotImplemented
exception UndefinedSemantics

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

let rec extend_env_list xlist ylist e e' =
	match (xlist, ylist) with
	([], []) -> e'
	| (xh::xt, yh::yt) -> (xh, (apply_env e yh)) :: (extend_env_list xt yt e e')
	| _ -> raise UndefinedSemantics

let rec extend_env_mem xlist enlist env mem =
	match (xlist, enlist) with
	([], []) -> (env, mem)
	| (xh::xt, eh::et) -> let l = new_location()
													in extend_env_mem xt et (extend_env (xh, l) env) (extend_mem (l, (getvalue eh)) mem)
	| _ -> raise UndefinedSemantics

let rec apply_rec r x = 
	match r with
	| [] -> raise (Failure (x ^ " is unbound in Record"))
	| (y,l)::t -> if x = y then l else apply_rec t x

let type2str v =
	match v with
	| Int n -> "Int"
	| Bool b -> "Bool"
	| Unit -> "Unit"  
	| Procedure (params,e,env) -> "Procedure"
	| Record record -> "Record"
	| Loc l -> "Loc"

(* if the following variable is set true, gc will work (otherwise, gc simply returns a given memory). *)
let remove_garbage = ref false 

let gc: env * mem -> mem
= fun (env, mem) ->
	if (not !remove_garbage) then mem 
	else 
		(* TODO *)
		let reach env mem =
			let reach_env = List.map (fun (x,l) -> (l, apply_mem mem l)) env
				in let rec reach_mem reach_env res =
						match reach_env with
						[] -> res
						| (l,v)::t -> match v with
													Loc l' -> reach_mem (extend_mem (l', apply_mem mem l') t) (extend_mem (l', apply_mem mem l') res)
													| Record r -> let rec iter r (left, tmp) =
																					match r with
																					[] -> (t, tmp)
																					| (x,l)::t -> if List.length (List.filter (fun (l',_) -> l = l') tmp) <> 1 then iter t ((extend_mem (l, apply_mem mem l) left), (extend_mem (l, apply_mem mem l) tmp))
																												else iter t (left, (extend_mem (l, apply_mem mem l) tmp))
																				in let (left, res) = iter r (t, res)
																						in reach_mem left res
													| Procedure (x, e, env') -> let rec iter r (left, tmp) =
																												match r with
																												[] -> (t, tmp)
																												| (x,l)::t -> if List.length (List.filter (fun (l',_) -> l = l') tmp) <> 1 then iter t ((extend_mem (l, apply_mem mem l) left), (extend_mem (l, apply_mem mem l) tmp))
																																			else iter t (left, (extend_mem (l, apply_mem mem l) tmp))
																											in let (left, res) = iter env' (t, res)
																													in reach_mem left res
													| _ -> reach_mem t res
						in reach_mem reach_env reach_env
			in reach env mem
			

let rec eval : program -> env -> mem -> (value * mem)
=fun pgm env mem ->  
  match pgm with
  | READ x -> (Unit, extend_mem (apply_env env x, Int (read_int())) mem) (* Do not modify *)
	| PRINT e ->
		let v, mem' = eval e env mem in
		let _ = print_endline (value2str v) in
		(v, gc(env,mem')) (* Do not modify *)
	(* Constants and Variables *)
	| SKIP -> (Unit, mem)
	| TRUE -> (Bool true, mem)
	| FALSE -> (Bool false, mem)
	| CONST c -> (Int c, mem)
	| VAR x -> ((apply_mem mem (apply_env env x)), mem)
	| PROC (xlist, e) -> (Procedure (xlist, e, env), mem)
	(* Unary and Binary Operations *)
	| ADD (e1, e2) -> let e' = eval e1 env mem
											in let e'' = eval e2 env (getmem e')
													in if type2str (getvalue e') = "Int" && type2str (getvalue e'') = "Int" then
																(Int (int_of_string (value2str (getvalue e')) + int_of_string (value2str (getvalue e''))), (getmem e''))
															else raise UndefinedSemantics
	| SUB (e1, e2) -> let e' = eval e1 env mem
											in let e'' = eval e2 env (getmem e')
													in if type2str (getvalue e') = "Int" && type2str (getvalue e'') = "Int" then
																(Int (int_of_string (value2str (getvalue e')) - int_of_string (value2str (getvalue e''))), (getmem e''))
															else raise UndefinedSemantics
	| MUL (e1, e2) -> let e' = eval e1 env mem
											in let e'' = eval e2 env (getmem e')
													in if type2str (getvalue e') = "Int" && type2str (getvalue e'') = "Int" then
																(Int (int_of_string (value2str (getvalue e')) * int_of_string (value2str (getvalue e''))), (getmem e''))
															else raise UndefinedSemantics
	| DIV (e1, e2) -> let e' = eval e1 env mem
											in let e'' = eval e2 env (getmem e')
													in if type2str (getvalue e') = "Int" && type2str (getvalue e'') = "Int" then
																if int_of_string (value2str (getvalue e'')) <> 0 then (Int (int_of_string (value2str (getvalue e')) / int_of_string (value2str (getvalue e''))), (getmem e''))
																else raise DivisionByZero
															else raise UndefinedSemantics
	| LE (e1, e2) -> let e' = eval e1 env mem
											in let e'' = eval e2 env (getmem e')
													in if type2str (getvalue e') = "Int" && type2str (getvalue e'') = "Int" then
																if int_of_string (value2str (getvalue e')) <= int_of_string (value2str (getvalue e'')) then (Bool true, (getmem e''))
																else (Bool false, (getmem e''))
															else raise UndefinedSemantics
	| EQ (e1, e2) -> let (v', m') = eval e1 env mem
										in let (v'', m'') = eval e2 env m'
											in if value2str v' = value2str v'' then (Bool true, m'')
													else (Bool false, m'')
	| NOT e -> let e' = eval e env mem
							in if value2str (getvalue e') = "true" then (Bool false, (getmem e'))
									else if value2str (getvalue e') = "false" then (Bool true, (getmem e'))
									else raise TypeError
	(* Flow Controls *)
	| IF (e1, e2, e3) -> let cond = eval e1 env mem
													in if value2str (getvalue cond) = "true" then eval e2 env (getmem cond)
															else if value2str (getvalue cond) = "false" then eval e3 env (getmem cond)
															else raise TypeError
	| WHILE (e1, e2) -> let (b, m') = eval e1 env mem
												in if value2str b = "false" then (Unit, m')
														else let (v, m'') = eval e2 env m'
																	in let res = eval (WHILE (e1, e2)) env m''
																			in res
	| SEQ (e1, e2) -> let e = eval e1 env mem
											in eval e2 env (getmem e)
	(* Records *)
	| RECORD xelist -> let res = match xelist with
																[] -> (Unit, mem)
																| (x,e)::t -> let (v, m) = eval e env mem
																								in let l = new_location()
																									in let rec subeval xelist (r', m') =
																												match xelist with
																												[] -> (r', m')
																												| (x,e)::t -> let (v, m) = (eval e env m')
																																				in let l = new_location()
																																						in subeval t ((r'@[(x,l)]), (extend_mem (l, v) m))
																												in let (record, memory) = subeval t ([(x,l)], (extend_mem (l, v) m))
																														in (Record record, memory)
											in res
	| FIELD (e, x) -> let (r, m) = eval e env mem
											in let res = match r with
																		Record r' -> let v = apply_mem m (apply_rec r' x)
																									in (v, m)
																		| _ -> raise TypeError
													in res
	| ASSIGNF (e1, x, e2) -> let (r, m') = eval e1 env mem
														in let (v, m'') = eval e2 env m'
																in let res = match r with
																							Record r' -> let m = extend_mem ((apply_rec r' x), v) m''
																														in (v, m)
																							| _ -> raise TypeError
																		in res
	(* Assignments *)
	| ASSIGN (x, e) -> let e' = eval e env mem
											in ((getvalue e'), (extend_mem ((apply_env env x), (getvalue e')) (getmem e')))
	| LET (x, e1, e2) -> let e' = eval e1 env mem
												in let l = new_location()
														in eval e2 (extend_env (x, l) env) (extend_mem (l, (getvalue e')) (getmem e'))
	(* Function Calls *)
	| CALLV (e, elist) -> let (p, m) = eval e env mem
													in let res = match p with
																Procedure (xlist, e, env') -> let rec subeval elist xlist (e', m') =
																																	match (elist, xlist) with
																																	([], []) -> (e', m')
																																	| (eh::et, xh::xt) -> let (v, m) = (eval eh env m')
																																													in let l = new_location()
																																															in subeval et xt ((extend_env (xh, l) e'), (extend_mem (l, v) m))
																																	| _ -> raise UndefinedSemantics
																																	in let (envn, memn) = subeval elist xlist (env', m)
																																			in eval e envn memn
																| _ -> raise TypeError
															in res
	| CALLR (e, ylist) -> let e' = eval e env mem
													in let res = match (getvalue e') with
																			Procedure (xlist, e, env') -> eval e (extend_env_list xlist ylist env env') (getmem e')
																			| _ -> raise TypeError
															in res
	| BEGIN e -> eval e env mem


let run : program -> bool -> bool -> unit 
= fun pgm with_gc print_mem_size ->
	let _ = remove_garbage := with_gc in 
	let mem = snd (eval pgm empty_env empty_mem) in   
	if (print_mem_size) then 
		print_endline (Printf.sprintf "Final mem size: %d" (List.length mem))
	
	
