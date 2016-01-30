%{
open Syntax
open Semantics

let parse_error s = 
  print_endline s;
  flush stdout
	
let string_of_position p =
	(string_of_int p.Lexing.pos_lnum) ^":" ^ (string_of_int p.Lexing.pos_cnum)

let raiseError () = 
	let p1 = (Parsing.rhs_start_pos 1) in 
  let p2 = (Parsing.symbol_end_pos()) in
	Parsing.clear_parser ();
  raise (SyntaxError(string_of_position p1, string_of_position p2))

%}


%token EOL
%token <int> INT
%token <string> ID
%token <string> ARGS
%token PLUS MINUS MULT DIV LPAR RPAR DECL IN EQ TYPE AND OR
%token LESS GT FUN ARROW END INTEGER BOOL REF FUNC COMMA
%token VAR BANG ASSIGN WHILE IF NOT ELSE EQUALS SEP DO THEN TRUE FALSE LB RB

%start main

%type <Syntax.ast> main

%% /* Grammar rules */

main: 
	seq EOL { $1 }
;

seq:  
	def SEP seq { Seq($1,$3,None) }
| def { $1 }
;

def:
  DECL ID EQ expr IN seq { Decl($2,$4,$6,None) }
| WHILE expr DO LB seq RB { While($2,$5) }
| IF expr THEN seq ELSE seq END{ If($2,$4,$6,None) }
| disj { $1 }

disj:
 disj OR conj { Or($1,$3) }
| conj { $1 }

conj:
 conj AND expr{ And($1,$3) }
| expr{ $1 }


expr: 
	expr PLUS term { Add($1,$3) }
| expr ASSIGN expr { Assign($1, $3,None) }
| expr MINUS term { Sub($1,$3) }
| expr EQUALS expr { Equal($1,$3) }
| expr LESS expr { Less($1,$3) }
| expr GT expr { Gt($1,$3) }
| FUN ID TYPE t ARROW seq { Fun($2,$4,$6,None) }
| NOT expr { Not($2) }
| VAR expr { Var($2,None) }
| term { $1 }
| error EOL { raiseError() }
;

t:
  INTEGER { IntType }
| BOOL { BoolType }
| REF LPAR t RPAR { RefType($3) }
| FUNC LPAR t ARROW t RPAR { FunType($3,$5) }

term: 
  term MULT fact { Mul($1,$3) }
| term DIV fact { Div($1,$3) }
| term LPAR expr RPAR { Call($1,$3,None) }
| BANG term { Deref($2,None) }
| fact { $1 }
;

fact:
	INT { Number($1) }
| LPAR expr RPAR { $2 }
| ID  { Id($1,None) }
| TRUE { Bool(true) }
| FALSE { Bool(false) }

%%
