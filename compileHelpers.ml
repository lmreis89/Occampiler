open Syntax
open Env


let dest2Str d =
	match d with
	| Reg d -> "%"^string_of_int d
	| I32 n -> string_of_int n
	| I1 b -> string_of_bool b


let label2Str label = 
	match label with
	| LabelT n -> "bb"^string_of_int(n)
	| LabelS s -> s

let rec type2Str t =
	match t with
	| IntType -> "_int"
	| BoolType -> "_bool"
	| RefType t1 -> "_ref" ^ type2Str t1
	| FunType(t1, t2) -> "_fun"^type2Str t1^type2Str t2
	| None -> "ERROR TYPE NONE"


let funConv fn = match fn with FunLabel x -> x

let type2LLVM t =
	match t with
	| IntType -> "i32"
	| BoolType -> "i1"
	| RefType _ -> "%struct.var_type*"
	| FunType(_,_) -> "%struct.closure_type*"
	| None -> "ERROR TYPE NONE"

let argList args = 
	let strings = List.rev (List.map (fun (y,(x,t)) -> type2LLVM t ^" "^ dest2Str x) args) in
	let s = (List.fold_right (fun s acum -> acum^s^" , ") strings "") in
	let len = String.length s in
	if(len > 3) then  String.sub s 0 (len - 3) else ""


let mapToList args =
	fold (fun key value acum -> (key,value)::acum) args []
   
let op2Str op = 
	match op with 
	| Add_i32(d,d1,d2) -> "  "^(dest2Str d)^" = add i32 "^(dest2Str d1)^", "^(dest2Str d2)
	| Sub_i32(d,d1,d2) -> "  "^(dest2Str d)^" = sub i32 "^(dest2Str d1)^", "^(dest2Str d2)
	| Mul_i32(d,d1,d2) -> "  "^(dest2Str d)^" = mul i32 "^(dest2Str d1)^", "^(dest2Str d2)
	| Div_i32(d,d1,d2) -> "  "^(dest2Str d)^" = div i32 "^(dest2Str d1)^", "^(dest2Str d2)
	| New_Var(IntType,d1,d2) -> "  "^(dest2Str d1)^" = call %struct.var_type* @int_var_create(i32 "^(dest2Str d2)^") nounwind ssp "
	| New_Var((RefType _),d1,d2) -> "  "^(dest2Str d1)^" = call %struct.var_type* @var_var_create(%struct.var_type* "^(dest2Str d2)^") nounwind ssp "
	| New_Var(d,d1,d2) -> raise(SemanticError("Trying to compile a variable of wrong type"))
	| Set_Var(IntType,d1,d2) -> "  "^"call void @int_set_var(%struct.var_type* "^(dest2Str d1)^", i32 "^(dest2Str d2)^") nounwind ssp"
	| Set_Var((RefType _),d1,d2) -> "  "^"call void @var_set_var(%struct.var_type* "^(dest2Str d1)^", %struct.var_type* "^(dest2Str d2)^") nounwind ssp"
	| Set_Var(d,d1,d2) -> raise(SemanticError("Trying to compile a variable of wrong type"))
	| Get_Var(IntType,d1,d2) -> "  "^(dest2Str d1)^" = call i32 @int_get_var(%struct.var_type* "^(dest2Str d2)^") nounwind ssp "
	| Get_Var((RefType _),d1,d2) ->  "  "^(dest2Str d1)^" = call %struct.var_type* @var_get_var(%struct.var_type* "^(dest2Str d2)^") nounwind ssp "
	| Get_Var(d,d1,d2) -> raise(SemanticError("Trying to compile a variable of wrong type"))
	| Phi (IntType, d, d1, l,d2, l1) -> "  "^dest2Str d^" = phi i32 ["^(dest2Str d1)^", %"^(label2Str l)^"], ["^(dest2Str d2)^", %"^(label2Str l1)^"]"
	| Phi (_, d, d1, l,d2, l1) -> raise(SemanticError("Trying to compile a Phi of wrong type"))
	| Not_i1 (d,d1) -> "  "^(dest2Str d)^" = xor i1 1, "^(dest2Str d1)
	| Eq_i32 (d,d1,d2) -> "  "^(dest2Str d)^" = icmp eq i32 "^(dest2Str d1)^", "^(dest2Str d2)
	| Call_type( (FunType(t1,t2)), d,d1,d2) ->"  "^ dest2Str(d) ^ " = call " ^ type2LLVM t2^ " @closure_call" ^ type2Str(FunType(t1,t2)) ^ "(%struct.closure_type*" ^
																					  " "^dest2Str(d1)^", "^type2LLVM(t1)^" "^dest2Str d2^") nounwind ssp"
	| Call_type(_,_,_,_) -> raise(SemanticError("Calling a non-function @ compileHelpers.ml!!"))
  | Create_clos( (FunType(t1,t2)), d,d1,fn,args) ->"  "^dest2Str(d1) ^" = call i8* @create_env_f"^ string_of_int(funConv(fn))^"("^
																									 argList (mapToList args) ^") nounwind ssp\n"^
																									 "  "^dest2Str(d) ^" = call %struct.closure_type* @closure_create(i8* "^dest2Str(d1)^
																									 ", void ()* bitcast ("^type2LLVM t2^" ("^type2LLVM t1^", i8*)* @f"^string_of_int(funConv(fn))^
													 												 " to void ()*)) nounwind"
	| Create_clos(x,_,_,_,_) -> raise(SemanticError("Creating a closure for non-function type: "^type2Str(x)^" @ compileHelpers.ml!!"))

let opsList ops = List.map(fun x -> (op2Str x)^"\n") ops


let block2Str block =
	match block with
	| IBlock (l,ops,l1) -> (label2Str l)^":\n"^(String.concat "" (opsList ops))^"  br label %"^(label2Str l1)
	| CBlock (l,ops,(d,l1,l2)) -> (label2Str l) ^ ":\n"^(String.concat "" (opsList ops))^"  br i1 "^(dest2Str d)^", label %"^(label2Str l1)^", label %"^(label2Str l2)

let uniq lst =
  let unique_set = Hashtbl.create (List.length lst) in
    List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
    Hashtbl.fold (fun x () xs -> x :: xs) unique_set []
    
let unique a b = let x = a@b in uniq x 

let rec free e =
    match e with 
		| Number n -> [] 
    | Id (x,t) -> [(x,t)] 
		| Add (l,r) -> unique (free l) (free r)
    | Sub(l,r) -> unique (free l) (free r)
		| Mul(l,r) -> unique (free l) (free r)
		| Div(l,r) -> unique (free l) (free r)
		| Decl(x,l,r,t) -> unique (free l) ( List.filter (fun (y,t) -> y <> x ) (free r) )
		| Var(x,t) -> free x
  	| Assign(x,y,t) -> unique (free x) (free y)
  	| Deref(x,t) -> free x 
  	| While(x,y) -> unique (free x) (free y)
  	| Not x -> free x
  	| Equal(x,y) -> unique (free x) (free y)
  	| Seq(x,y,t) -> unique (free x) (free y)
  	| If(x,y,z,t) -> unique (unique (free x) (free y)) (free z)
  	| Bool(x) -> []
  	| Less(x,y) -> unique (free x) (free y)
  	| Gt(x,y) -> unique (free x) (free y)
  	| Fun(x,t,r,t1) -> List.filter (fun (y,t) -> y <> x ) (free r)
  	| Call(x,y,t) -> unique (free x) (free y)
  	| And(x,y) -> unique (free x) (free y)
  	| Or(x,y) -> unique (free x) (free y)
		 

let blockList blocks = List.map(fun x -> (block2Str x)^"\n\n") blocks

let main_header = "define i32 @main() nounwind {\n"
               
let main_footer dest finish  = 
			 label2Str finish ^ ":\n"^
       "  call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* "^
       "@.str, i64 0, i64 0), i32 "^ (dest2Str dest) ^
       ") nounwind\n"^
       "  ret i32 0\n"^
       "}\n"^
       "\n"^
       "declare i32 @printf(i8*, ...) nounwind\n"^
       "declare noalias i8* @malloc(i64) nounwind\n"^
       "declare %struct.var_type* @int_var_create(i32)\n"^
       "declare %struct.var_type* @var_var_create(%struct.var_type*)\n"^
       "declare i32 @int_get_var(%struct.var_type*)\n"^
       "declare %struct.var_type* @var_get_var(%struct.var_type*)\n"^
       "declare void @int_set_var(%struct.var_type*, i32)\n"^
       "declare void @var_set_var(%struct.var_type*, %struct.var_type*)\n"^
       "declare void @free_var(%struct.var_type*)\n"^
       "declare %struct.closure_type* @closure_create(i8*, void ()*)\n"

let format t =
	match t with
	| IntType -> "%d"
	| FunType(_,_) -> "%p"
	| RefType _ -> "%p"
	| BoolType -> "%d"
	| None -> ""

let file_header t =
	 "@.str = private constant [4 x i8] c\""^format t^"\\0A\\00\", align 1\n"^
   "@.formatprint = private constant [3 x i8] c\"%d\\00\", align 1 \n"^
   "@.formatprintln = private constant [4 x i8] c\"%d\\0A\\00\", align 1\n"^
   "\n"^
   "%struct.var_type = type { i32, %union.anon }\n"^
   "%union.anon = type { %struct.var_type* }\n"^
   "%struct.closure_type = type { i8*, void ()* }\n"^
   "\n"

let alignOf t =
	match t with
	| IntType -> 4
	| FunType (_, _) -> 4
  | RefType _ -> 4 
	| None -> -1
	| BoolType -> -1

let funType_dump	t1 t2 =
	"define "^type2LLVM t2^
  " @closure_call"^type2Str (FunType(t1,t2))^"(%struct.closure_type* %closure, " ^ 
  type2LLVM t1^ " %x) {\n"^
  "entry:\n"^
  "  %0 = getelementptr inbounds %struct.closure_type* %closure, i64 0, i32 1\n"^
  "  %1 = load void ()** %0, align 8\n"^
  "  %2 = bitcast void ()* %1 to "^type2LLVM t2^" ("^type2LLVM t1^", i8*)*\n"^
  "  %3 = getelementptr inbounds %struct.closure_type* %closure, i64 0, i32 0\n"^
  "  %4 = load i8** %3, align 8\n"^
  "  %5 = tail call "^type2LLVM t2^" %2("^type2LLVM t1^" %x, i8* %4) nounwind\n"^
  "  ret "^type2LLVM t2^" %5\n"^
  "}\n\n"

let dump_type s tp =
	match tp with
	| FunType(t1,t2) -> s ^ funType_dump t1 t2
	| _ -> s


let dump_types ts =
	"; ---- Closure Types section -----\n"^
	List.fold_left dump_type "" ts


let storeIdEnv (reg,string) (_,REnv(x,t,n)) =
	let d  = 2*reg- 1 in
  let d1 = 2*reg in
  let s1 = " ; %0+"^string_of_int n^" = "^type2LLVM t^" %"^x^"\n"^
           " %"^string_of_int d^" = getelementptr inbounds i8* %0, i32 "^string_of_int n^"\n"^
           " %"^string_of_int d1^" = bitcast i8* %"^string_of_int d^" to "^type2LLVM t^"*\n"^
           " store "^type2LLVM t^" %"^x^", "^type2LLVM t^"* %"^string_of_int d1^", align "^string_of_int (alignOf t)^"\n"
  in (reg+1,string^s1)

let storeIds lst =
	let (fst,snd) = List.fold_left storeIdEnv (1,"") lst in
	snd

let parList lst =
	let string = List.fold_left (fun s (x,REnv(y,t,n)) -> s^type2LLVM t^" %"^x^" , ") "" lst in
	let len = String.length string in
	if(len > 3) then String.sub string 0 (len - 3) else ""
	

let createEnvFunction fn env mallocSize =
	let lst = mapToList env in 
	"define i8* @create_env_f"^string_of_int(funConv fn)^"("^
	parList lst^") {\n"^"entry:\n"^" %0 = tail call noalias i8* @malloc(i64 "^string_of_int mallocSize^") nounwind\n"^
	storeIds lst^"  ret i8* %0\n"^"}\n\n"
	
	

let getEnv y t align n =
	"; get "^y^" align "^string_of_int align^"\n"^
	"  "^dest2Str(Reg (3*n+1))^" = getelementptr inbounds i8* %env, i32 "^string_of_int align^"\n"^
	"  "^dest2Str(Reg (3*n+2))^" = bitcast i8* "^dest2Str(Reg (3*n+1))^" to "^type2LLVM t^"*\n"^
	"  "^dest2Str(Reg (3*n+3))^" = load "^type2LLVM t^"* "^dest2Str(Reg ((3*n)+2))^", align "^string_of_int(alignOf t)^"\n\n"
	

let envToLLVM env =
	let lst = mapToList env in
	let (llvm_env,mallocs) = List.fold_left (fun (s,n) (x, REnv(y,t,align)) -> (s^(getEnv y t align n),n+1)) ("",0) lst in
	llvm_env
 
let functionHeader fn ft env =
	match ft with
	| FunType(t1,t2) -> "define "^type2LLVM t2 ^ " @f"^string_of_int(funConv fn)^
											"("^type2LLVM t1^" %x, i8* %env) nounwind {\n"^
											"start:\n"^" %0 = bitcast "^type2LLVM t1^" %x to "^type2LLVM t1^"\n"^
											envToLLVM env^" br label %entry\n\n"
	| _ -> raise(SemanticError("Non-funtype on function header"))
	

let functionFooter finish ft d =
	match ft with
	| FunType(t1,t2) -> label2Str finish ^ ":\n  ret " ^ type2LLVM t2 ^ " " ^ dest2Str d ^"\n}\n\n"
	| _ -> raise(SemanticError("Non-funtype on function footer"))

let env_types env =
	let list = mapToList env in
	let strings = List.rev (List.map (fun (x,REnv(_,t,n)) -> type2LLVM t) list) in
	let s = (List.fold_right (fun s acum -> acum^s^" , ") strings "") in
	let len = String.length s in
	if(len > 3) then String.sub s 0 ((String.length s) - 3) else ""

let functionEnvironment fn env =
	 "%struct.env_f"^string_of_int (funConv fn)^" = type { "^ env_types env^ " }\n\n"
	  

let dump_function f =
	match f with
	| FunctionCode(fn,t,bs,d,finish,env,mallocSize) ->
			"; ---- begin f"^string_of_int (funConv fn)^" -----\n"^
			functionEnvironment fn env^
			createEnvFunction fn env mallocSize ^
			functionHeader fn t env ^
			(String.concat "" (blockList bs))^
			functionFooter finish t d^
			"; ---- end f"^string_of_int (funConv fn)^" -----\n"
	| _ -> raise(SemanticError("Dumping non function"))

let dump_functions fs =
	"; ---- Closures section -----\n\n"^ List.fold_right (fun fnc s -> s^dump_function fnc^"\n") fs ""


let dump t bs fs ts d result = 
	file_header t^
  dump_types ts ^
  dump_functions fs ^
	main_header ^
	(String.concat "" (blockList bs)) ^ 
	main_footer d result

let save file string =
     let channel = open_out file in
     output_string channel string;
     close_out channel
	




