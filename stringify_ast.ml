open Types
open Constants
open Ast

let stringify_pair v =
  "<" ^ (fst v) ^ " " ^ (snd v) ^ ">"

let stringify_var_pair v =
  stringify_pair (stringify_type (fst v), (snd v))

let rec stringify_expression e =
  match e with
  | Constant c -> stringify_constant c
  | Symbol s -> "#" ^ s
  | MemberExpression e -> "<" ^ (stringify_expression e.host) ^ ">"
                          ^ ".<" ^ (stringify_expression e.member) ^ ">"
  | ArrayExpression e -> "<" ^ (stringify_expression e.array) ^ ">"
                         ^ "[" ^ (stringify_expression e.index) ^ "]"
  | AssignmentExpression e -> "(= " ^ (stringify_expression e.lvalue) ^ " " ^ (stringify_expression e.rvalue) ^ ")"
  | ArithmeticExpression e -> "(" ^ e.operator ^ " " ^ (stringify_expression e.loperand) ^ " " ^ (stringify_expression e.roperand) ^ ")"
  | UnArithmeticExpression e -> "(" ^ e.operator ^ " " ^ (stringify_expression e.operand) ^ ")"
  | This -> "this"
  | CallExpression e ->
    "(" ^
    (stringify_expression e.callee) ^
    (String.concat ""
       (List.map
          (fun s -> " " ^ (stringify_expression s))
          e.arguments)) ^
    ")"
  | ArrayAllocExpression e -> "new " ^ (stringify_type e.type_) ^ "[" ^ (stringify_expression e.size) ^ "]"
  | AllocExpression e -> "new " ^ (stringify_type e.type_)

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
     ^ ": " ^ func.name ^ ":"
     ^ (String.concat "," (List.map (fun s -> " " ^ s) (List.map stringify_var_pair func.arguments))) ^ "\n"
     ^ (stringify_ast_list func.body (indent + 1))
   | Program p -> "Program\n" ^ (stringify_ast_list p.body (indent + 1))
   | Statement stmt -> (stringify_statement stmt)
  )
;;
