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

