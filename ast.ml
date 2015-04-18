open Types
open Constants
open Symbols

type ast_variable = (decaf_type * string);;

type ast =
  | Variable of ast_variable
  | Function of ast_function
  | Class of ast_class
  | Interface of ast_interface
  | Program of ast_program
  | Statement of ast_statement

and ast_function =
  {
    func_name: string;
    func_arguments: ast_variable list;
    func_body: ast_statement_block;
    func_return_type: decaf_type;
  }

and ast_class =
  {
    class_name: string;
    super: string option;
    interfaces: string list;
    methods: ast_function list;
    class_properties: ast_variable list;
  }

and ast_interface =
  {
    interface_name: string;
    interface_properties: ast_prototype list;
  }

and ast_prototype =
  {
    proto_return_type: decaf_type;
    proto_name: string;
    proto_arguments: ast_variable list;
  }

and ast_program =
  {
    program_body: ast list;
  }

and ast_statement =
  | Expression of ast_expression
  | StatementBlock of ast_statement_block
  | IfStatement of ast_if_statement
  | LoopStatement of ast_loop_statement
  | ReturnStatement of ast_return_statement
  | BreakStatement

and ast_statement_block =
  {
    declarations: ast_variable list;
    statements: ast_statement list;
  }

and ast_if_statement =
  {
    condition: ast_expression;
    consequence: ast_statement option;
    alternative: ast_statement option;
  }

and ast_loop_statement =
  {
    loop_type: ast_loop_type;
    loop_initialization: ast_expression option;
    loop_condition: ast_expression;
    loop_afterthought: ast_expression option;
    loop_body: ast_statement option;
  }

and ast_loop_type =
  | For
  | While

and ast_return_statement =
  {
    value: ast_expression option;
  }

and ast_expression =
  | Constant of decaf_constant
  | Symbol of decaf_symbol
  | MemberExpression of ast_member_expression
  | ArrayExpression of ast_array_expression
  | AssignmentExpression of ast_assignment_expression
  | ArithmeticExpression of ast_arithmetic_expression
  | This
  | CallExpression of ast_call_expression
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
    roperand: ast_expression option;
    operator: string;
  }

and ast_call_expression =
  {
    callee: ast_expression;
    call_arguments: ast_expression list;
  }

and ast_alloc_expression =
  {
    type_: decaf_type;
    size: ast_expression option;
  }

