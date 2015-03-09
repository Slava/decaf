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

let rec stringify_ast_list (v: ast list) indent =
  String.concat "\n"
    (List.map (stringify_ast ~indent:indent) v)

and stringify_statement_block block indent =
  (
    match block.declarations with
    | [] -> ""
    | decls ->
      (String.concat
         "\n"
         (List.map
            (fun d -> (make_indent indent) ^ (stringify_var_pair d)) decls)
      ) ^ "\n"
  ) ^ (
    String.concat
      "\n"
      (List.map (fun stmt -> (stringify_statement ~indent stmt)) block.statements)
  )

and stringify_statement ?(indent=0) v =
  match v with
  | Expression e -> (make_indent indent) ^ (stringify_expression e)
  | StatementBlock bl ->
    stringify_statement_block bl (indent + 1)
  | IfStatement st ->
    (make_indent indent) ^
    "if " ^ (stringify_expression st.condition) ^ "\n" ^
    (stringify_opt_statement (indent + 1) st.consequence) ^
    (match st.alternative with
     | Some s -> "\n" ^
                 (make_indent indent) ^
                 "else\n" ^
                 (stringify_statement ~indent:(indent + 1) s)
     | None -> "")
  | WhileStatement st ->
    (make_indent indent) ^
    "while " ^ (stringify_expression st.condition) ^ "\n" ^
    (stringify_opt_statement (indent + 1) st.body)
  | ForStatement st ->
    (make_indent indent) ^
    "for |" ^ (stringify_opt_expression st.initialization) ^
    "|" ^ (stringify_expression st.condition) ^
    "|" ^ (stringify_opt_expression st.afterthought) ^ "|\n" ^
    (stringify_opt_statement (indent + 1) st.body)
  | ReturnStatement st ->
    (make_indent indent) ^
    "return " ^ (stringify_opt_expression st.value)
  | BreakStatement ->
    (make_indent indent) ^ "break"

and stringify_opt_statement indent st =
  (match st with
   | Some s -> stringify_statement ~indent:indent s
   | None -> "")

and stringify_opt_expression e =
  match e with
  | Some e -> stringify_expression e
  | None -> ""

and stringify_ast ?(indent=0) (v: ast) =
  (make_indent indent) ^
  (match v with
   | Variable var -> stringify_var_pair var
   | Function func ->
     "Function "
     ^ (stringify_type func.return_type)
     ^ ": " ^ func.name ^ ":"
     ^ (String.concat "," (List.map (fun s -> " " ^ s) (List.map stringify_var_pair func.arguments))) ^ "\n"
     ^ (stringify_statement_block func.body (indent + 1))
   | Program p -> "Program\n" ^ (stringify_ast_list p.body (indent + 1))
   | Statement stmt -> (stringify_statement ~indent:indent stmt)
  )

and make_indent indent =
  String.make (2 * indent) ' '
;;
