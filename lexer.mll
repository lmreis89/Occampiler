{
open Parser

exception UnknownToken of char
}

let digit = ['0'-'9']
let char = ['a'-'z' 'A'-'Z']

rule token = parse
	| [' ' '\t' '\r' ] { token lexbuf }
	| '\n' { EOL }
	| '+' { PLUS } 
	| '-' { MINUS } 
	| '*' { MULT } 
	| '/' { DIV } 
	| '(' { LPAR }
	| ')'? { RPAR }
	| '{'? {LB}
	| '}' { RB }
	| "in" { IN }
	| '=' { EQ }
	| '<' { LESS }
	| '>' { GT }
	| "decl"  { DECL }
	| ":=" { ASSIGN }
	| "var" { VAR }
	| '!' { BANG }
	| "while" { WHILE }
	| "not" { NOT }
	| "==" { EQUALS }
	| "if" { IF }
	| "else" { ELSE }
	| ';' { SEP }
	| "then" {THEN}
	| "do" {DO} 
	| "true" {TRUE}
	| "false" {FALSE}
	| "->" {ARROW}
	| "fun" { FUN }
	| "end" { END }
	| "Int" { INTEGER }
	| "Bool" {BOOL}
	| "Ref" {REF}
	| "Func" {FUNC}
	| "&&" { AND }
	| "||" { OR }
	| ':' { TYPE }
	| ',' {COMMA}
	| digit+ as num { INT (int_of_string num) }
	| char (char | digit)* as word { ID word }
	| _ as tk { raise (UnknownToken tk) }
	| eof { raise End_of_file }