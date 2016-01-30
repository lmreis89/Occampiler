open Parser
open Syntax
open Semantics
open Env

let rec prompt lexbuf =
	print_string "# " ;
	flush stdout;  
  try		
  	let s = Parser.main Lexer.token lexbuf in

			print_endline(unparse_cmd s);  
			let annotAst = check s in 
			print_endline(exec s); 
			  
			comp annotAst; 
  with 
	| SyntaxError (p1,p2) -> 
			print_string ("Parsing error:"^p1^" to "^p2^"\n");  
			Lexing.flush_input lexbuf; prompt lexbuf ; 
			
	| End_of_file -> ()
	| SemanticError(p1) -> print_string("SemanticError: "^p1)

let main () =
      let lexbuf = Lexing.from_channel (stdin) in prompt lexbuf;
			let int1 = Sys.command("llvm-as-3.1 test.s") in
			let int2 = Sys.command("llvm-ld-3.1 test.s.bc runtime.s.bc") in int2+int1
;;

main();;
