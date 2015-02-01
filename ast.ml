open Types
open Constants

type ast_variable = (decaf_type * string);;

type ast =
  | Variable of ast_variable
  | Function of ast_function
  | Program of ast_program
  | Statement of ast_statement

and ast_function =
  {
    name: string;
    arguments: ast_variable list;
    body: ast list;
    return_type: decaf_type;
  }

and ast_program =
  {
    body: ast list;
  }

and ast_statement =
  | Expression of ast_expression

and ast_expression =
  | Constant of decaf_constant

let stringify_pair v =
  "<" ^ (fst v) ^ " " ^ (snd v) ^ ">"

let stringify_var_pair v =
  stringify_pair (stringify_type (fst v), (snd v))

let stringify_expression e =
  match e with
  | Constant c -> stringify_constant c

let stringify_statement v =
  match v with
  | Expression e -> stringify_expression e

let rec stringify_ast_list (v: ast list) indent =
  String.concat "\n"
    (List.map (stringify_ast ~indent:(indent+1)) v)
and stringify_ast ?(indent=0) (v: ast) =
  (String.make indent ' ') ^
  (match v with
   | Variable var -> stringify_var_pair var
   | Function func ->
     "Function "
     ^ (stringify_type func.return_type)
     ^ ": " ^ func.name ^ ": "
     ^ (String.concat ", " (List.map stringify_var_pair func.arguments)) ^ "\n"
     ^ (stringify_ast_list func.body (indent + 1))
   | Program p -> "Program\n" ^ (stringify_ast_list p.body (indent + 1))
   | Statement stmt -> "<" ^ (stringify_statement stmt) ^ ">"
  )
;;
