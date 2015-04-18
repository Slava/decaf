open Types
open Constants
open Ast
open Core.Std

let stringify_list_helper (f: 'a -> string) (lst: 'a list): string =
  String.concat ~sep:"\n"
    (List.map ~f:f lst)

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
  | ArithmeticExpression e -> "(" ^ e.operator ^ " " ^ (stringify_expression e.loperand) ^
                              (match e.roperand with
                               | None -> ")"
                               | Some operand -> " " ^ (stringify_expression operand) ^ ")")
  | This -> "this"
  | CallExpression e ->
    "(" ^
    (stringify_expression e.callee) ^
    (String.concat ~sep:""
       (List.map
          ~f:(fun s -> " " ^ (stringify_expression s))
          e.call_arguments)) ^
    ")"
  | AllocExpression e -> "new " ^ (stringify_type e.type_) ^ (
      match e.size with
      | None -> ""
      | Some size -> "[" ^ (stringify_expression size) ^ "]")

let rec stringify_ast_list (v: ast list) =
  String.concat ~sep:"\n"
    (List.map ~f:stringify_ast v)

and stringify_statement_block block =
  (match block.declarations with
   | [] -> ""
   | decls ->
     (stringify_list_helper stringify_var_pair decls) ^ "\n") ^
  (stringify_list_helper stringify_statement block.statements)

and stringify_statement (v: ast_statement) =
  match v with
  | Expression e -> stringify_expression e
  | StatementBlock bl ->
    indent_lines (stringify_statement_block bl)
  | IfStatement st ->
    "if " ^ (stringify_expression st.condition) ^ "\n" ^
    indent_lines (stringify_opt_statement st.consequence) ^
    (match st.alternative with
     | Some alt ->
       "\nelse\n" ^
       indent_lines (stringify_statement alt)
     | None -> "")
  | LoopStatement st -> (
      match st.loop_type with
      | For -> ("for |" ^ (stringify_opt_expression st.loop_initialization) ^
                "|" ^ (stringify_expression st.loop_condition) ^
                "|" ^ (stringify_opt_expression st.loop_afterthought) ^ "|\n" ^
                indent_lines (stringify_opt_statement st.loop_body))
      | While -> ("while " ^ (stringify_expression st.loop_condition) ^ "\n" ^
                  indent_lines (stringify_opt_statement st.loop_body)))
  | ReturnStatement st ->
    "return " ^ (stringify_opt_expression st.value)
  | BreakStatement ->
    "break"

and stringify_opt_statement st =
  (match st with
   | Some s -> stringify_statement s
   | None -> "")

and stringify_opt_expression e =
  match e with
  | Some e -> stringify_expression e
  | None -> ""

and stringify_formals fs =
  fs
  |> List.map ~f:(fun s -> " " ^ stringify_var_pair s)
  |> String.concat ~sep:","

and stringify_function (func: ast_function) =
  "Function " ^
  stringify_type func.func_return_type ^
  ": " ^ func.func_name ^ ":" ^
  stringify_formals func.func_arguments ^
  "\n" ^
  indent_lines (stringify_statement_block func.func_body)

and stringify_prototype (p: ast_prototype) =
  stringify_type p.proto_return_type ^
  ": " ^ p.proto_name ^ ":" ^
  stringify_formals p.proto_arguments

and stringify_ast (v: ast) =
  (match v with
   | Variable var -> stringify_var_pair var
   | Class c ->
     "Class " ^ c.class_name ^
     (match c.super with
      | Some s -> ": " ^ s
      | None -> "") ^
     " [" ^ (String.concat ~sep:" " c.interfaces) ^ "]" ^ "\n" ^
     indent_lines (stringify_list_helper stringify_var_pair c.class_properties) ^ "\n" ^
     indent_lines (stringify_list_helper stringify_function c.methods)
   | Interface i ->
     "Interface " ^ i.interface_name ^ "\n" ^
     indent_lines (stringify_list_helper stringify_prototype i.interface_properties)
   | Function func -> (stringify_function func)
   | Program p -> "Program\n" ^ indent_lines (stringify_ast_list p.program_body)
   | Statement stmt -> (stringify_statement stmt)
  )

and indent_lines (lines: string) =
  String.split_on_chars ~on:['\n'] lines
  |> List.map ~f:(fun s ->
      match s with
      | "" -> ""
      | _ -> "  " ^ s)
  |> String.concat ~sep:"\n"
;;

