open Syntax
open Env
open CompileHelpers

type binop_type = Op_add | Op_sub | Op_mul | Op_div | Op_lt

let entryresult = (LabelS("entry"),LabelS("result")) ;;
												
let regs : (int list) ref = ref([0]) ;;

let wSize = 4;;

let funcs = ref(0) ;;

let labels = ref(0);;

let newReg = fun() -> 
	match !regs with 
	| [] -> raise(SemanticError("Empty reg list"))
	| x::xs -> let ret = x in regs := (ret+1)::xs; ret
;;

let newLabel = fun() -> (let ret = !labels in labels := !labels + 1; ret) ;;

let newFunc = fun() -> ( let ret = !funcs in funcs := !funcs + 1; ret) ;;

let printReg =
	"STATE: \n"^"Reg level: "^string_of_int (List.length (!regs))^"\n" ;;

let setRegNumber n =
	match !regs with
	| [] -> raise(SemanticError("Empty reg list"))
	| x::xs -> regs := n::xs

let beginScope = 
	fun() ->
	match !regs with
	| [] -> raise(SemanticError("Empty reg list"))
	| x::xs -> regs := [0]@(!regs)

let endScope =
	fun() ->
	match !regs with
	| [] -> raise (SemanticError("Empty reg list"))
	| x::xs -> regs := xs
 
let resultToStr e =
	match e with
 | Num n -> string_of_int n
 | Boolean b -> string_of_bool b
 | Ref r -> "reference"
 | Closure (c, l, r) -> "Function"

let toNum e = 
	match e with
	| Num n -> n
	| Closure(c,l,r) -> raise (SemanticError("Returning closure instead of Int"))
	| _ -> raise(SemanticError("This expression should be an Int"))

let toRef e =
	match e with
	| Ref r -> r
	| _ -> raise (SemanticError("This expression should be a reference"))

let toBool e =
	match e with
	| Boolean b -> b
	| _ -> raise( SemanticError("This expression should be a boolean"))

	
let rec eval e env = 
	match e with
	| Number n -> Num (n)

	| Add(l,r) -> let left = toNum(eval l env) in 
								let right = toNum(eval r env) in
								Num (left + right)
								
	| Sub(l,r) -> let left = toNum(eval l env) in 
								let right = toNum(eval r env) in 
								Num (left - right)
								
	| Mul(l,r) -> let left = toNum(eval l env) in 
								let right = toNum(eval r env) in 
								Num (left * right)
								
	| Div(l,r) -> let right = toNum(eval r env) in 
								if(right != 0) then let left = toNum(eval l env) in Num (left / right)
								else raise (SemanticError("Division by zero"))
									
	| Id (x,t) -> find x env

	| Decl(x,l,r,t) -> let left = eval l env in 
										 let newEnv = assoc x left env in 
										 eval r newEnv
										
	| Var (x,t) -> let res = eval x env in Ref (ref res)
	| Assign(l,r,t) -> let left_val = toRef(eval l env) in 
										 left_val := eval r env; !left_val
									
	| Deref (x,t) -> let res = eval x env in !(toRef(res))
 
 | While (c,l) -> let condition = toBool(eval c env) in 
												(match condition with
													|	true -> eval (Seq(l, While(c,l),None)) env
													| false -> Boolean(false)
												) 
 | Not x -> let condition = toBool(eval x env) in Boolean(not condition)
 | Equal(x,y) -> let left = eval x env in 
								 let right = eval y env in
								 if (left = right) then Boolean(true) else Boolean(false)
										
 | Seq(x,y,t) -> eval x env; eval y env

 | If(c,t,f,t') -> let cond = toBool(eval c env) in 
									(match cond with
									  | true -> eval t env
										| false -> eval f env )

| Bool(x) -> Boolean(x)
| Gt(x,y) -> let left = toNum(eval x env) in  
						 let right = toNum(eval y env) in 
						 if (left > right) then Boolean(true) else Boolean(false)
						
| Less(x,y) -> let left = toNum(eval x env) in 
							 let right = toNum(eval y env) in 
							 if (left < right) then Boolean(true) else Boolean(false)
							
| Fun(x,t,e,t') -> Closure(x,e,env)
| Call(x,y,t) -> let func = eval x env in 
							   let arg = eval y env in 
								 (match func with
									| Closure(id,body,envir) -> eval body (assoc id arg envir)
									| _ -> raise(SemanticError("No such function exists"))
								  )
																
| And(l,r) -> let left = toBool(eval l env) in 
						  let right = toBool(eval r env)	in 
							Boolean(left&&right)

| Or(l,r) -> let left = toBool(eval l env) in 
						 let right = toBool(eval r env)	in 
						 Boolean(left||right)
										


let toInteger e =
	match e with
	| IntType -> IntType
	| _ -> raise(SemanticError("This expression should be an Int"))

let toBoolean e =
	match e with
	| BoolType -> BoolType
	| _ -> raise( SemanticError("This expression should be a boolean"))

let toRef e =
	match e with
	| RefType t -> RefType t
	| _ -> raise ( SemanticError("This expression should be a Reference"))

let typeOf e =
	match e with
	| Number n -> IntType
	| Add(_,_) -> IntType
	| Sub(_,_) -> IntType
	| Mul(_,_) -> IntType
	| Div(_,_) -> IntType
	| Id (_,t) -> t
	| Var(x,t) -> t
	| Decl(_,_,_,t) -> t
	| Equal(_,_) -> BoolType
	| Assign(_,_,t) -> t
	| Deref(_,t) -> t
	| While(c,l) -> BoolType
	| Not x -> BoolType
	| Bool x -> BoolType
	| Seq(x,y,t) -> t
	| Call(x,y,t) -> t
	| Fun(x,t,e,ty) -> ty
	| Gt(x,y) -> BoolType
	| Less(x,y) -> BoolType
	| Or(x,y) -> BoolType
	| If(x,t,f,ty) -> ty
	| And(x,y) -> BoolType

let getID e = 
	match e with
	| Id(x,t) -> x
	| _ -> ""

let rec typecheck e env =
	match e with
	| Number n -> Number n
	| Add(l,r) -> let left = typecheck l env in 
								let right = typecheck r env in 
								if(typeOf(left) = typeOf(right) && typeOf(left) = IntType) then 
								Add(left,right) 
								else raise(SemanticError("Adding non-integer values"))
								
	| Sub(l,r) -> let left = typecheck l env in 
								let right = typecheck r env in 
								if(typeOf(left) = typeOf(right) && typeOf(left) = IntType) then
								Sub(left,right) else raise(SemanticError("Subtracting non-integer values"))
								
	| Mul(l,r) -> let left = typecheck l env in 
							  let right = typecheck r env in 
								if(typeOf(left) = typeOf(right) && typeOf(left) = IntType) then
								Mul(left,right) else raise(SemanticError("Multiplying non-integer values"))
								
	| Div(l,r) -> let left = typecheck l env in let right = typecheck r env in 
								if(typeOf(left) = typeOf(right) && typeOf(left) = IntType) then 
								Div(left,right) else raise(SemanticError("Dividing non-integer values"))
								
	| Id (x,t) -> let _t = find x env in Id(x,_t)
	
	| Decl(x,l,r,t) -> let left = typecheck l env in 
										 let right = typecheck r (assoc x (typeOf left) env) in 
										 Decl(x,left,right,(typeOf right))
										
	| Var(x,t) -> let left = typecheck x env in Var(left,RefType(typeOf left))
	
	| Assign(l,r,t) -> let left = typecheck l env in 
										 let right = typecheck r env in
										 (match typeOf(left) with
											| RefType t' -> if(t' = typeOf(right)) then Assign(left,right,t') else
												raise(SemanticError("Type mismatch on assignment"))
											| _ -> raise(SemanticError("Assigning a non-reference value")))

  | Deref(x,t) -> let e = typecheck x env in 
									(match typeOf(e) with
									 | RefType t' -> Deref(e,t')
									 | _ -> raise (SemanticError("Dereferencing a non-reference value")))

  | While(c,l) -> let cond = typecheck c env in
								  let loop = typecheck l env in 
									if(typeOf(cond) = BoolType) then
									While(cond,loop) else raise(SemanticError("Not a Bool value in while condition"))
									
	| Not x -> let cond = typecheck x env in 
						 if(typeOf(cond) = BoolType) then
						 Not(cond) else raise(SemanticError("Not a Bool value in Not expression"))
						
  | Equal(x,y) -> let t = typecheck x env in  
									let t' = typecheck y env in 
									if(typeOf(t) = typeOf(t')) then Equal(t,t') 
									else 	raise(SemanticError("Comparison between values of non-equal types"))
									
	|	Seq(x,y,t) -> let x' = typecheck x env in 
								  let y' = typecheck y env in
									Seq(x',y',typeOf(y'))
									
	| If(c,t,f,ty) -> let if_cond = typecheck c env in 
										let _then = typecheck t env in 
										let _else = typecheck f env in
										(match typeOf(if_cond) with
										 | BoolType -> if(typeOf(_then) = typeOf(_else)) then If(if_cond,_then,_else,typeOf(_then)) 
											 						 else	raise(SemanticError("Different types in If-else return values"))
										| _ -> raise(SemanticError("Not a Bool value in if condition")))

  | Bool(x) -> Bool(x)
	| Gt(x,y) -> let left = typecheck x env in 
							 let right = typecheck y env in 
							 if(typeOf(left) = typeOf(right) && typeOf(left) = IntType) then
							 Gt(left,right) else raise(SemanticError("Greater comparison between non-integer values"))
							
	| Less(x,y) -> let left = typecheck x env in 
								 let right = typecheck y env in 
								 if(typeOf(left) = typeOf(right) && typeOf(left) = IntType) then
								 Less(left,right) else raise(SemanticError("Greater comparison between non-integer values"))									
  
	| Fun(x,t,e,ty) -> let ret = typecheck e (assoc x t env) in
										 Fun(x,t,ret,FunType(t,typeOf(ret)))
										
	| Call(x,y,t) -> let func = typecheck x env in 
									 let args = typecheck y env in 
									 let callType = typeOf(args) in
									 (match typeOf(func) with
										| FunType(argType,retType) -> if(callType = argType) then Call(func,args,retType) 
																								  else raise(SemanticError("Calling function "^getID(func)^" with wrong argument type -> (argType: "^typeToStr(argType)^" , Declared Type: "^typeToStr(callType)^")"))																					
									  | _ -> raise(SemanticError("Calling a non-function identifier")))

  | And(l,r) -> let left = typecheck l env in 
								let right = typecheck r env in 
								if((typeOf(left) = typeOf(right)) && typeOf(left) = BoolType) then
								And(left,right) else raise(SemanticError("And comparison between non-boolean values"))
								 
  | Or(l,r) -> let left = typecheck l env in 
							 let right = typecheck r env in if((typeOf(left) = typeOf(right)) && typeOf(left) = BoolType) then
							 Or(left,right) else raise(SemanticError("Or comparison between non-boolean values"))


let nextAlign t n =
	match t with
	| IntType -> (n,n+4)
	| FunType(_,_) -> if( (n mod wSize) = 0) then (n,n+wSize) else (n+4,n+4+wSize)
	| RefType _ -> if( (n mod wSize) = 0) then (n,n+wSize) else (n+4,n+4+wSize)
	| _ ->  print_endline("wrong alignof type"); (-1,-1)
								
let assocAlign (env, n) (x,t) =
	let (this,next) = nextAlign t n in
	let retEnv = assoc x (REnv(x, t, this)) env in
	(retEnv, next)
	
let paramDefinition (env, n) (x,t) =
	let retEnv = assoc x (Reg (3*n)) env in
	(retEnv,n+1)

let rec comp e env (start, finish) : (block list)*(funct list)*(_type list)*dest*label =
	match e with
	| Number n -> ([],[],[],I32 n,start)

	| Add(l,r) -> let m = LabelT(newLabel()) in 
	              let m1 = LabelT(newLabel()) in 
								let (bs,fs,ts,d1,m) = comp l env (start ,m) in
							  let (bs1,fs1,ts1,d2,m1) = comp r env (m, m1) in
								let d3 = Reg(newReg()) in 
								(List.flatten [bs; bs1; [IBlock(m1,[Add_i32(d3,d1,d2)],finish)]],fs@fs1,(unique ts ts1),d3,finish)
												
	| Sub(l,r) -> let m = LabelT(newLabel()) in 
	              let m1 = LabelT(newLabel()) in 
								let (bs,fs,ts,d1,m) = comp l env (start ,m) in
							  let (bs1,fs1,ts1,d2,m1) = comp r env (m, m1) in
								let d3 = Reg(newReg()) in 
								(List.flatten [bs; bs1; [IBlock(m1,[Sub_i32(d3,d1,d2)],finish)]],fs@fs1,(unique ts ts1),d3,finish)
								
	| Mul(l,r) -> let m = LabelT(newLabel()) in 
	              let m1 = LabelT(newLabel()) in 
								let (bs,fs,ts,d1,m) = comp l env (start ,m) in
							  let (bs1,fs1,ts1,d2,m1) = comp r env (m, m1) in
								let d3 = Reg(newReg()) in 
								(List.flatten [bs; bs1; [IBlock(m1,[Mul_i32(d3,d1,d2)],finish)]],fs@fs1,(unique ts ts1),d3,finish)
								
	| Div(l,r) -> let m = LabelT(newLabel()) in 
	              let m1 = LabelT(newLabel()) in 
								let (bs,fs,ts,d1,m) = comp l env (start ,m) in
							  let (bs1,fs1,ts1,d2,m1) = comp r env (m, m1) in
								let d3 = Reg(newReg()) in 
								(List.flatten [bs; bs1; [IBlock(m1,[Div_i32(d3,d1,d2)],finish)]],fs@fs1,(unique ts ts1),d3,finish)
								
	| Decl(x,l,r,t) -> let m = LabelT(newLabel()) in
									   let (bs,fs,ts,d1,m) = comp l env (start, m) in
										 let (bs1,fs1,ts1,d2,finish) =  comp r (assoc x d1 env) (m, finish) in 
										 let blocks = List.flatten [bs;bs1] in
										 let funcs = fs@fs1 in
										 let types = (unique ts ts1) in
										 (blocks,funcs,types,d2,finish)
										 
	| Id (x,t) -> let dest = find x env in 
							  ([], [], [], dest, start)
	 
	| Var (x, (RefType t)) -> let m = LabelT(newLabel()) in
														let (bs,fs,ts,d,m) = comp x env (start, m) in
														let d1 = Reg(newReg()) in 
														(List.flatten [bs; [IBlock(m, [New_Var(t,d1,d)],finish)]],fs,ts, d1,finish)
	
	| Var(x,_) -> raise(SemanticError("Compiling error: No Reference type on variable node"))
	
	| Assign(l,r,t) -> 	let m = LabelT(newLabel()) in
											let m1 = LabelT(newLabel()) in
											let (bs,fs,ts,d,m) = comp l env (start, m) in 
											let (bs1,fs1,ts1,d1,m1) = comp r env (m, m1) in 
											(List.flatten [bs;bs1; [IBlock(m1, [Set_Var(t,d,d1)],finish)]],(fs@fs1),(unique ts ts1),d1,finish)
	
	| Deref (x,t) -> let m = LabelT(newLabel()) in
														let (bs,fs,ts,d,m) = comp x env (start, m) in
														let d1 = Reg(newReg()) in 
														(List.flatten [bs; [IBlock(m, [Get_Var(t,d1,d)],finish)]],fs,ts, d1,finish)			
			
  | Not x -> let m = LabelT(newLabel()) in
						 let (bs,fs,ts,d,m) = comp x env (start, m) in
						 let d1 = Reg(newReg()) in
						 (List.flatten [bs; [IBlock(m, [Not_i1(d1,d)], finish)]],fs,ts,d1,finish)
		
  | Equal(x,y) -> let m = LabelT(newLabel()) in 
	                let m1 = LabelT(newLabel()) in 
									let (bs,fs,ts,d1,m) = comp x env (start, m) in
							  	let (bs1,fs1,ts1,d2,m1) = comp y env (m, m1) in
									let d3 = Reg(newReg()) in 
									(List.flatten [bs; bs1; [IBlock(m1,[Eq_i32(d3,d1,d2)],finish)]],(fs@fs1),(unique ts ts1),d3,finish)
									
  | Seq(x,y,t) -> let m = LabelT(newLabel()) in
									let (bs,fs,ts,d1,m) = comp x env (start, m) in
									let(bs1,fs1,ts1,d2,finish) = comp y env (m, finish) in
									(List.flatten [bs ; bs1],(fs@fs1),(unique ts ts1), d2,finish)
									
	| While (c,l) -> let l_cond = LabelT(newLabel()) in
									 let l_body = LabelT(newLabel()) in
									 let l_exit = LabelT(newLabel()) in
									 let (bs,fs,ts,d1,l_cond) = comp c env (start, l_cond) in
									 let (bs1,fs1,ts1,d2,l_exit) = comp l env (l_body ,l_exit) in
									 (List.flatten [bs;bs1; [CBlock(l_cond,[], (d1,l_body,finish))]; [IBlock(l_exit,[],start)]],(fs@fs1),(unique ts ts1),d2,finish)
	
  | If(c,t,f,t1) -> let l_cond = LabelT(newLabel()) in
										let l_then = LabelT(newLabel()) in
										let l_else = LabelT(newLabel()) in
										let then_exit = LabelT(newLabel()) in
										let else_exit = LabelT(newLabel()) in
										let l_exit = LabelT(newLabel()) in
										let (bs,fs,ts,d,l_cond) = comp c env (start,l_cond) in
										let (bs1,fs1,ts1,d1,then_exit) = comp t env (l_then,then_exit) in
										let (bs2,fs2,ts2,d2,else_exit) = comp f env (l_else,else_exit) in
										let d3 = Reg(newReg()) in
										let codeseq = List.flatten [bs;bs1;bs2; 
										[CBlock(l_cond,[],(d,l_then,l_else))];
										[IBlock(then_exit,[],l_exit)];
										[IBlock(else_exit,[],l_exit)];
										[IBlock(l_exit, [Phi (t1, d3, d1, then_exit, d2, else_exit)], finish) ]] in
										(codeseq,(fs@fs1@fs2),(unique (unique ts ts1) ts2),d3,finish)							
										
  | Bool(x) -> ([],[],[],I1 x,start)
	
  | Fun(x,_,e,t) -> let envnames = ( List.filter (fun (y,t) -> y <> x) (free e) ) in
										 let (env_free,mallocSize) = List.fold_left assocAlign (new_env(),0) envnames in
										 let fn = FunLabel(newFunc()) in

										 beginScope() ;	
										 
										 let d1 = Reg(newReg()) in
										 let fresh_env = new_env() in
										 let nenv = assoc x (Reg 0) fresh_env in
										 let (nenv_par,n) = List.fold_left paramDefinition (nenv,1) envnames in
										
																						
										 setRegNumber (1 + 3*(List.length envnames));  
										
										 let (bs,fs,ts,d3,m) = comp e nenv_par entryresult in
										 endScope();
										 
										let d = Reg(newReg()) in
										let d2 = Reg(newReg()) in
										let args = List.fold_left (fun (envir) (y,t) -> assoc y ((find y env),t) envir) (new_env()) envnames in
										let blocks = [IBlock(start,[Create_clos(t,d2,d,fn,args)],finish)] in
										let functions = (FunctionCode(fn,t,bs,d3,m,env_free,mallocSize))::fs in
										let typs = unique [t] ts in
										(blocks,functions,typs,d2,finish)
		
  | Call(x,y,t) -> let m = LabelT(newLabel()) in
									 let m1 = LabelT(newLabel()) in
									 let (bs,fs,ts,d,m) = comp x env (start,m) in
									 let (bs1,fs1,ts1,d1,m1) = comp y env (m,m1) in
									 let d2 = Reg(newReg()) in
									 (List.flatten[bs;bs1; [IBlock(m1,[Call_type((typeOf x),d2,d,d1)],finish)]], (fs@fs1), (unique ts ts1), d2,finish)
									
	| And(x,y) -> ([],[],[],I32 0,start)
	| Or(x,y) -> ([],[],[],I32 0,start)
	| Gt(x,y) -> ([],[],[],I32 0,start)
	| Less(x,y) -> ([],[],[],I32 0,start)


let exec e =
	let env = new_env() in (match eval e env with
																			  | Num n -> string_of_int n
																				| Boolean b -> string_of_bool b
																				| Ref r -> "reference"
																				| Closure (c, l, r) -> "Function")
  
let check e =
	let env = new_env() in typecheck e env
																		 

let comp c = 
	let env = new_env() in let (bs,fs,ts,d,result) = comp c env entryresult in let string = dump (typeOf c) bs fs ts d result in save "test.s" string
