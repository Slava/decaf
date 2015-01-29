open Types

type ast_variable = (decaf_type * string);;

type ast_node_function =
  {
    name: string;
    arguments: ast_variable list;
  }
;;

type ast_node_type =
  | Variable of ast_variable
  | Function of ast_node_function
  | Program
;;

type ast =
  {
    tag: ast_node_type;
    children: ast list;
  }
;;
