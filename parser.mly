%{
open Ast
open Constants
open Types
%}

%token <int> INT
%token <float> DOUBLE
%token <string> ID
%token <string> OP
%token <string> STRING
%token <string> TYPE
%token <string> BUILTIN
%token <char> CHAR
%token <bool> BOOL
%token NULL
%token CLASS
%token INTERFACE
%token THIS
%token EXTENDS
%token IMPLEMENTS
%token FOR
%token WHILE
%token IF
%token ELSE
%token RETURN
%token BREAK
%token NEW
%token EOF
%token ARRAY_DECL
%token COMMA
%token PAREN_OPEN
%token PAREN_CLOSE
%token BRACE_OPEN
%token BRACE_CLOSE
%token BRACKET_OPEN
%token BRACKET_CLOSE
%token SEMICOLON
%token EQUALS
%token DOT

%left DOT
%right EQUALS
%left OP

%start <Ast.ast> program
%%


/* list of declarations */
program:
  | program_
    {
      Program { body = $1; }
    }
  ;

program_:
  | EOF { [] }
  | decl program_ { $1 :: $2 }
  ;

decl:
  | variable_decl { $1 }
  | function_decl { $1 }
  ;

variable_decl:
  | variable SEMICOLON
    {
      Variable $1
    }
  ;

function_decl:
  /* Type name (Type a, Type b) stmt_block */
  | return_type = type_ name = ID PAREN_OPEN arguments = formals
    PAREN_CLOSE body = stmt_block
    {
        Function { name; arguments; return_type; body; };
    }
  ;

formals:
  | variables = separated_list(COMMA, variable) { variables }
  ;

stmt_block:
  | BRACE_OPEN variable_decls = list(variable_decl) stmts = list(stmt) BRACE_CLOSE
    {
      (* deoptionalize statements *)
      List.append variable_decls (Core.Std.List.filter_map ~f:(fun o ->
          match o with
          | None -> None
          | Some stmt -> Some (Statement stmt)) stmts)
    }
  ;

stmt:
  | opt_expr = option(expr) SEMICOLON
      {
        match opt_expr with
        | None -> None
        | Some expr -> Some (Expression expr)
      }
  ;

expr:
  /* implicitly make '=' operator right-associative */
  | lvalue EQUALS expr_no_assignment
    {
      AssignmentExpression
        {
          lvalue = $1;
          rvalue = $3;
        }
    }
  | expr_no_assignment { $1 }
  ;

(* extra rule to resolve shift-reduce conflict of '=' being associative *)
expr_no_assignment:
  | constant { Constant $1 }
  | lvalue { $1 }
  ;

lvalue:
  | ID { Symbol($1) }
  | expr_no_assignment DOT ID { MemberExpression { host = $1; member = Symbol($3); } }
  | expr_no_assignment BRACKET_OPEN expr BRACKET_CLOSE
    {
      ArrayExpression { array = $1; index = $3; }
    }
  ;

constant:
  | i = INT { IntConstant i }
  | s = STRING { StringConstant s }
  | d = DOUBLE { DoubleConstant d }
  | c = CHAR { CharConstant c }
  | b = BOOL { BoolConstant b }
  | NULL { NullConstant }
  ;

variable:
  | type_ ID { ($1, $2); }
  ;

type_:
  | type_ ARRAY_DECL { ArrayType $1 }
  | TYPE { PrimitiveType $1 }
  | ID { ComplexType $1 }
  ;
