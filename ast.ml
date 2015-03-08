open Types
open Constants
open Symbols

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
  | Symbol of decaf_symbol
  | MemberExpression of ast_member_expression
  | ArrayExpression of ast_array_expression
  | AssignmentExpression of ast_assignment_expression
  | ArithmeticExpression of ast_arithmetic_expression
  | UnArithmeticExpression of ast_unary_arithmetic_expression
  | This
  | CallExpression of ast_call_expression
  | ArrayAllocExpression of ast_array_alloc_expression
  | AllocExpression of ast_alloc_expression

and ast_member_expression =
  {
    host: ast_expression;
    member: ast_expression;
  }

and ast_array_expression =
  {
    array: ast_expression;
    index: ast_expression;
  }

and ast_assignment_expression =
  {
    lvalue: ast_expression;
    rvalue: ast_expression;
  }

and ast_arithmetic_expression =
  {
    loperand: ast_expression;
    roperand: ast_expression;
    operator: string;
  }

and ast_unary_arithmetic_expression =
  {
    operand: ast_expression;
    operator: string;
  }

and ast_call_expression =
  {
    callee: ast_expression;
    arguments: ast_expression list;
  }

and ast_array_alloc_expression =
  {
    type_: decaf_type;
    size: ast_expression;
  }

and ast_alloc_expression =
  {
    type_: decaf_type;
  }

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
