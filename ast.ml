open Types
open Constants
open Symbols

type ast_variable = (decaf_type * string);;

type ast =
  | Variable of ast_variable Location.loc
  | Function of ast_function Location.loc
  | Class of ast_class Location.loc
  | Interface of ast_interface Location.loc
  | Program of ast_program Location.loc
  | Statement of ast_statement Location.loc

and ast_function =
  {
    func_name: string Location.loc;
    func_arguments: ast_variable Location.loc list;
    func_body: ast_statement_block Location.loc;
    func_return_type: decaf_type Location.loc;
  }

and ast_class =
  {
    class_name: string Location.loc;
    super: string Location.loc option;
    interfaces: string Location.loc list;
    methods: ast_function Location.loc list;
    class_properties: ast_variable Location.loc list;
  }

and ast_interface =
  {
    interface_name: string Location.loc;
    interface_properties: ast_prototype Location.loc list;
  }

and ast_prototype =
  {
    proto_return_type: decaf_type Location.loc;
    proto_name: string Location.loc;
    proto_arguments: ast_variable Location.loc list;
  }

and ast_program =
  {
    program_body: ast Location.loc list;
  }

and ast_statement =
  | Expression of ast_expression Location.loc
  | StatementBlock of ast_statement_block Location.loc
  | IfStatement of ast_if_statement Location.loc
  | LoopStatement of ast_loop_statement Location.loc
  | ReturnStatement of ast_return_statement Location.loc
  | BreakStatement of unit Location.loc

and ast_statement_block =
  {
    declarations: ast_variable Location.loc list;
    statements: ast_statement Location.loc list;
  }

and ast_if_statement =
  {
    condition: ast_expression Location.loc;
    consequence: ast_statement Location.loc option;
    alternative: ast_statement Location.loc option;
  }

and ast_loop_statement =
  {
    loop_type: ast_loop_type;
    loop_initialization: ast_expression Location.loc option;
    loop_condition: ast_expression Location.loc;
    loop_afterthought: ast_expression Location.loc option;
    loop_body: ast_statement Location.loc option;
  }

and ast_loop_type =
  | For
  | While

and ast_return_statement =
  {
    value: ast_expression Location.loc option;
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
    host: ast_expression Location.loc;
    member: ast_expression Location.loc;
  }

and ast_array_expression =
  {
    array: ast_expression Location.loc;
    index: ast_expression Location.loc;
  }

and ast_assignment_expression =
  {
    lvalue: ast_expression Location.loc;
    rvalue: ast_expression Location.loc;
  }

and ast_arithmetic_expression =
  {
    loperand: ast_expression Location.loc;
    roperand: ast_expression Location.loc option;
    operator: string Location.loc;
  }

and ast_call_expression =
  {
    callee: ast_expression Location.loc;
    call_arguments: ast_expression Location.loc list;
  }

and ast_alloc_expression =
  {
    type_: decaf_type Location.loc;
    size: ast_expression Location.loc option;
  }

