
type _type = 
	| IntType
	| BoolType
	| RefType of _type
	| FunType of _type * _type
	| None

type ast = 
	| Number of int 
	| Add of ast * ast
	| Sub of ast * ast 
	| Mul of ast * ast 
	| Div of ast * ast 
	| Decl of string * ast * ast * _type
	| Id of string * _type
	| Var of ast * _type
	| Assign of ast * ast * _type
	| Deref of ast * _type
	| While of ast * ast 
	| Not of ast
	| Equal of ast*ast
	| Seq of ast*ast * _type
	| If of ast*ast*ast * _type
	| Bool of bool
	| Less of ast*ast
	| Gt of ast*ast
	| Fun of string * _type * ast *_type
	| Call of ast*ast*_type
	| And of ast*ast
	| Or of ast*ast



type label =
	| LabelT of int
	| LabelS of string


type dest =
	| Reg of int
	| I32 of int
	| I1 of bool

type extdest =
	| REnv of string*_type*int

type funLabel = 
	| FunLabel of int

type op =
	| Add_i32 of dest*dest*dest
	| Sub_i32 of dest*dest*dest
	| Mul_i32 of dest*dest*dest
	| Div_i32 of dest*dest*dest
	| New_Var of _type*dest*dest
	| Get_Var of _type*dest*dest
	| Set_Var of _type*dest*dest
	| Phi of _type*dest*dest*label*dest*label
	| Eq_i32 of dest*dest*dest
	| Not_i1 of dest*dest
	| Create_clos of _type*dest*dest*funLabel*((dest*_type) Env.EnvMap.t)
	| Call_type of _type*dest*dest*dest

type block =
	| IBlock of label*(op list)*label
	| CBlock of label*(op list)*(dest*label*label)
	

type result =
	| Num of int
	| Ref of result ref
	| Boolean of bool
	| Closure of string * ast * result Env.EnvMap.t

type funct =
	| FunctionCode of funLabel*_type*(block list)*dest*label*(extdest Env.EnvMap.t)*int
	

type binop_type = Op_add | Op_sub | Op_mul | Op_div | Op_lt

let id_counter = ref(0);;

exception SyntaxError of string * string
exception SemanticError of string

let rec unparse a =
	match a with
		| Number n -> "Num("^string_of_int n^")"
		| Add (l,r) -> "Add("^(unparse l)^","^(unparse r)^")"
		| Sub (l,r) -> "Sub("^(unparse l)^","^(unparse r)^")"
		| Mul (l,r) -> "Mul("^(unparse l)^","^(unparse r)^")"
		| Div (l,r) -> "Div("^(unparse l)^","^(unparse r)^")"
		| Id (x,t) -> x
		| Decl(x,l,r,t) -> "Decl("^x^", "^(unparse l)^", "^(unparse r)^")"
		| Var (e,t) -> "Var( "^unparse e^" )"
		| Assign(l,r,t) -> "Assign("^(unparse l)^","^(unparse r)^")"
		| Deref (e,t) -> "Deref( "^unparse e^" )"
		| While (c,l) -> "While("^(unparse c)^", "^unparse l^")"
		| Not e -> "Not("^unparse e^")"
		| Equal (x,y) -> "Equal("^unparse x^", "^unparse y^")"
		| Seq (x,y,t) -> "Seq("^unparse x^", "^unparse y^")"
		| If (c,t,f,t') -> "If("^unparse c^", "^unparse t^")/Else("^unparse f^")"
		| Bool b -> string_of_bool(b)
		| Gt(x,y) -> "Greater("^unparse x^", "^unparse y^")"
		| Less(x,y) -> "Less("^unparse x^", "^unparse y^")"
		| Fun(s,t,x,t') -> "Fun("^s^", "^unparse x^")"
		| Call(x,y,t) -> "Call("^unparse x^", "^unparse y^")"
		| And(l,r) -> "And("^(unparse l)^","^(unparse r)^")"
		| Or(l,r) -> "Or("^(unparse l)^","^(unparse r)^")"

let rec typeToStr t =
	match t with
	| IntType -> "Integer"
	| BoolType -> "Boolean"
	| RefType t -> "RefType "^typeToStr(t)
	| FunType(t,t') -> "Function(args:"^typeToStr(t)^" return:"^typeToStr(t')^")"
	| None -> "None"

let rec unparseType ast =
	match ast with
	| Number n -> "Num("^string_of_int n^")"
		| Add (l,r) -> "Add("^(unparse l)^","^(unparse r)^")"
		| Sub (l,r) -> "Sub("^(unparse l)^","^(unparse r)^")"
		| Mul (l,r) -> "Mul("^(unparse l)^","^(unparse r)^")"
		| Div (l,r) -> "Div("^(unparse l)^","^(unparse r)^")"
		| Id (x,t) -> "Id("^x^","^typeToStr(t)^")"
		| Decl(x,l,r,t) -> "Decl("^x^", "^(unparse l)^", "^(unparse r)^","^typeToStr(t)^")"
		| Var (e,t) -> "Var( "^unparse e^","^typeToStr(t)^"^)"
		| Assign(l,r,t) -> "Assign("^(unparse l)^","^(unparse r)^","^typeToStr(t)^")"
		| Deref (e,t) -> "Deref( "^unparse e^","^typeToStr(t)^" )"
		| While (c,l) -> "While("^(unparse c)^", "^unparse l^")"
		| Not e -> "Not("^unparse e^")"
		| Equal (x,y) -> "Equal("^unparse x^", "^unparse y^")"
		| Seq (x,y,t) -> "Seq("^unparse x^", "^unparse y^","^typeToStr(t)^")"
		| If (c,t,f,t') -> "If("^unparse c^", "^unparse t^")/Else("^unparse f^","^typeToStr(t')^")"
		| Bool b -> string_of_bool(b)
		| Gt(x,y) -> "Greater("^unparse x^", "^unparse y^")"
		| Less(x,y) -> "Less("^unparse x^", "^unparse y^")"
		| Fun(s,t,x,t') -> "Fun("^s^", "^unparse x^","^typeToStr(t')^")"
		| Call(x,y,t) -> "Call("^unparse x^", "^unparse y^","^typeToStr(t)^")"
		| And(l,r) -> "And("^(unparse l)^","^(unparse r)^")"
		| Or(l,r) -> "Or("^(unparse l)^","^(unparse r)^")"




let rec unparse_cmd e = 
	unparse e

