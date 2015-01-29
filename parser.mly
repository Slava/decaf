%{
  open Printf
  open Types
  open Ast
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
%token SEMICOLON

%left OP

%start <Ast.ast> program
%%


/* list of declarations */
program:
  | program_
    {
      {
        tag = Program;
        children = $1;
      }
    }
  ;

program_:
  | EOF { printf "matching EOF!\n"; [] }
  | decl program_ { printf "matchin decl\n"; $1 :: $2 }
  ;

decl:
  | variable_decl { $1 }
  | function_decl { $1 }
  ;

variable_decl:
  | variable SEMICOLON
    {
      {
        tag = Variable $1;
        children = [];
      }
    }
  ;

function_decl:
  /* Type name (Type a, Type b) stmt_block */
  | type_ name = ID PAREN_OPEN arguments = formals PAREN_CLOSE body = stmt_block
    {
      {
        tag = Function { name; arguments; };
        children = body;
      }
    }
  ;

formals:
  | variables = separated_list(COMMA, variable) { variables }
  ;

stmt_block:
  | BRACE_OPEN list(variable_decl) /*list(stmt)*/ BRACE_CLOSE { [] }
  ;

variable:
  | type_ ID { ($1, $2); }
  ;

type_:
  | type_ ARRAY_DECL { ArrayType $1 }
  | TYPE { PrimitiveType $1 }
  | ID { ComplexType $1 }
  ;
