{
  open Printf

  type token =
    | CLASS
    | INTERFACE
    | THIS
    | EXTENDS
    | IMPLEMENTS
    | FOR
    | WHILE
    | IF
    | ELSE
    | RETURN
    | BREAK
    | NEW
    | BUILTIN of string
    | ID of string
    | OP of string
    | BOOL of bool
    | INT of int
    | DOUBLE of float
    | CHAR of char
    | STRING of string
    | NULL

  let keyword_table = Hashtbl.create 72
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
              [
		("class", CLASS);
		("interface", INTERFACE);
		("this", THIS);
		("extends", EXTENDS);
		("implements", IMPLEMENTS);
		("for", FOR);
		("while", WHILE);
		("if", IF);
		("else", ELSE);
		("return", RETURN);
		("break", BREAK);
		("new", NEW)
              ]
  let builtins = ["NewArray"; "Print"; "ReadInteger"; "ReadLine"]
}

let digit = ['0'-'9']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let dec_literal = digit+
let hex_literal = "0" ['X' 'x'] hex_digit+

rule decaf = parse
		| dec_literal as inum
		| hex_literal as inum
				   {
				     printf "int %s\n" inum;
				     let num = int_of_string inum in
				     INT num
				   }
		| digit+ "." digit* ( ['e' 'E'] ['+' '-']? digit+ )?
		  as inum
		       {
			 printf "float %s\n" inum;
			 let num = float_of_string inum in
			 DOUBLE num
		       }
		| '"' [^ '\n' '"']* '"' as str { printf "str %s\n" str; STRING str }
		| ("true"|"false") as ibool
					{
					  printf "bool %s\n" ibool;
					  BOOL (if ibool = "true" then true else false)
					}
		| [' ' '\n' '\t'] { (* ignore whitespace *) decaf lexbuf }
		| _ { raise Parsing.Parse_error }
		| eof { raise End_of_file }


{
  let rec parse lexbuf =
     let token = decaf lexbuf in
     (* do nothing in this example *)
     parse lexbuf

  let main () =
    let cin =
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else stdin
    in
    let lexbuf = Lexing.from_channel cin in
    try parse lexbuf
    with End_of_file -> ()

  let _ = Printexc.print main ()
}
