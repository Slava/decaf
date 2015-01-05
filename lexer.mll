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
    | TYPE of string
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
  let types = ["void"; "int"; "double"; "bool"; "string"]

  let mprint t =
    match t with
    | CLASS | INTERFACE | THIS | EXTENDS | IMPLEMENTS | FOR
    | WHILE | IF | ELSE | RETURN | BREAK | NEW
                                             -> printf "keyword\n"
    | BUILTIN (name) -> printf "builtin %s\n" name
    | ID (id) -> printf "id %s\n" id
    | OP (opr) -> printf "op %s\n" opr
    | TYPE (t) -> printf "type %s\n" t
    | BOOL (b) -> printf "bool %s\n" (if b then "t" else "f")
    | INT (num) -> printf "int %d\n" num
    | DOUBLE (num) -> printf "double %f\n" num
    | CHAR (c) -> printf "char %c\n" c
    | STRING (str) -> printf "string %s\n" str
    | NULL -> printf "null\n"
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let dec_literal = digit+
let hex_literal = "0" ['X' 'x'] hex_digit+

rule decaf = parse
    (* literals *)
    | (dec_literal|hex_literal) as inum
           {
             let num = int_of_string inum in
             INT num
           }
    | digit+ "." digit* ( ['e' 'E'] ['+' '-']? digit+ )?
      as inum
           {
             let num = float_of_string inum in
             DOUBLE num
           }
    | '"' [^ '\n' '"']* '"' as str { STRING str }
    | ("true"|"false") as ibool
          {
            BOOL (if ibool = "true" then true else false)
          }
    | "null" { NULL }

    (* operators *)
    | ("+"|"-"|"*"|"/"|"%"|"<="|">="|"<"|">"|"=="|"!="|"="|"&&"|"||"|"!"|";"|","|"."|"["|"]"|"("|")"|"{"|"}")
      as opr {
             OP opr
           }

    (* identifiers, keywords, builtins *)
    | letter (letter|digit|'_')* as id
           {
             try Hashtbl.find keyword_table id with
		 	         Not_found -> if List.mem id builtins
                            then BUILTIN id
                            else if List.mem id types
                            then TYPE id
                            else ID id
           }

    | "//" [^ '\n']* { (* ignore comments *) decaf lexbuf }

    (* C-style comments*)
    | "/*"                    (* start with "/*" *)
        ( [^ '*']             (* any symbol that is not a star *)
        | ('*'+ [^ '*' '/'])  (* or is a series of stars not followed by slash *)
        )*                    (* any numbers of chars in comment's body *)
        "*/"                  (* finish comment with "*/" *)
          { decaf lexbuf }

    | [' ' '\n' '\t'] { (* ignore whitespace *) decaf lexbuf }
    | _ as c { raise Parsing.Parse_error }
    | eof { raise End_of_file }


{
  let rec parse lexbuf =
     let token = decaf lexbuf in
     mprint token;
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
