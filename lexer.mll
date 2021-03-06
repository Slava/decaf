{
  open Lexing
  open Printf
  open Parser

  exception SyntaxError of string

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
                ("new", NEW);
                ("NewArray", NEWARRAY);
                ("ReadInteger", READINTEGER);
                ("ReadLine", READLINE);
                ("Print", PRINT)
              ]
  let types = ["void"; "int"; "double"; "bool"; "string"]
  let string_cut_quotes str =
    String.sub str 1 (String.length str - 2)

  let stringify_token t =
      match t with
      | INT i -> "int " ^ string_of_int i
      | DOUBLE f -> "double " ^ string_of_float f
      | ID str -> "id " ^ str
      | STRING str -> "str " ^ str
      | TYPE str -> "type " ^ str
      | CHAR c -> "char " ^ (String.make 1 c)
      | BOOL b -> "bool " ^ (string_of_bool b)
      | NULL -> "null"
      | CLASS -> "class"
      | INTERFACE -> "interface"
      | THIS -> "this"
      | EXTENDS -> "extends"
      | IMPLEMENTS -> "implements"
      | FOR -> "for"
      | WHILE -> "while"
      | IF -> "if"
      | ELSE -> "else"
      | RETURN -> "return"
      | BREAK -> "break"
      | NEW -> "new"
      | EOF -> "eof"
      | ARRAY_DECL -> "array_decl"
      | COMMA -> "comma"
      | PAREN_OPEN -> "paren_open"
      | PAREN_CLOSE -> "paren_close"
      | BRACE_OPEN -> "brace_open"
      | BRACE_CLOSE -> "brace_close"
      | BRACKET_OPEN -> "bracket_open"
      | BRACKET_CLOSE -> "bracket_close"
      | SEMICOLON -> "semicolon"
      | EQUALS -> "equals"
      | DOT -> "dot"
      | UN_LOG_OP o -> "unary_logical_op(" ^ o  ^ ")"
      | BIN_LOG_OP o -> "binary_logical_op(" ^ o  ^ ")"
      | BIN_UN_ADD_OP o -> "binary_unary_add_op(" ^ o  ^ ")"
      | BIN_MULT_OP o -> "binary_mult_op(" ^ o  ^ ")"
      | BIN_CMP_OP o -> "binary_cmp_op(" ^ o  ^ ")"
      | READLINE -> "ReadLine"
      | READINTEGER -> "ReadInteger"
      | PRINT -> "Print"
      | NEWARRAY -> "NewArray"
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
    | '"' [^ '\n' '"']* '"' as str { STRING (string_cut_quotes str) }
    | '\'' [^ '\''] '\'' as c_str { CHAR (String.get (string_cut_quotes c_str) 0) }
    | ("true"|"false") as ibool
          {
            BOOL (if ibool = "true" then true else false)
          }
    | "null" { NULL }

    (* special operator "[]" for array type declarations *)
    | "[]" { ARRAY_DECL }
    | "," { COMMA }
    | "(" { PAREN_OPEN }
    | ")" { PAREN_CLOSE }
    | "{" { BRACE_OPEN }
    | "}" { BRACE_CLOSE }
    | "[" { BRACKET_OPEN }
    | "]" { BRACKET_CLOSE }
    | ";" { SEMICOLON }
    | "=" { EQUALS }
    | "." { DOT }
    (* unary operators *)
    | "!" { UN_LOG_OP ("!") }
    (* unary and binary *)
    | "+" { BIN_UN_ADD_OP ("+") }
    | "-" { BIN_UN_ADD_OP ("-") }
    (* binary *)
    | "*" { BIN_MULT_OP ("*") }
    | "/" { BIN_MULT_OP ("/") }
    | "%" { BIN_MULT_OP ("%") }
    | "<=" { BIN_CMP_OP ("<=") }
    | ">=" { BIN_CMP_OP (">=") }
    | "==" { BIN_CMP_OP ("==") }
    | "!=" { BIN_CMP_OP ("!=") }
    | "<" { BIN_CMP_OP ("<") }
    | ">" { BIN_CMP_OP (">") }
    | "&&" { BIN_LOG_OP ("&&") }
    | "||" { BIN_LOG_OP ("||") }

    (* identifiers, keywords, builtins *)
    | letter (letter|digit|'_')* as id
        {
          try Hashtbl.find keyword_table id with
            Not_found -> if List.mem id types
            then TYPE id
            else ID id
        }

    | "//" [^ '\n']* { (* ignore comments *) decaf lexbuf }

    (* C-style comments*)
    | "/*"                    (* start with "/*" *)
      ( [^ '*']               (* any symbol that is not a star *)
      | ('*'+ [^ '*' '/'])    (* or is a series of stars not followed by slash *)
      )*                      (* any numbers of chars in comment's body *)
      "*/"                    (* finish comment with "*/" *)
        { decaf lexbuf }

    | [' ' '\t'] { (* ignore whitespace *) decaf lexbuf }
    | '\n' { Lexing.new_line lexbuf; decaf lexbuf }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
    | eof { EOF }
