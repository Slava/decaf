%{
  open Printf
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
%token SEMICOLON

%left OP

%start <(Types.decaf_type * string) list> program
%%


/* list of declarations */
program:
  | EOF { printf "matching EOF!\n"; [] }
  | decl program { printf "matchin decl\n"; $1 :: $2 }
  ;

decl:
  | variable_decl { $1 }
  ;

variable_decl:
  | variable SEMICOLON { $1 }
  ;

function_decl:
  /* Type name (Type a, Type b) stmt_block */
  | type_ ID PAREN_OPEN formals PAREN_CLOSE stmt_block {}
  ;

formals:
  | separated_list(COMMA, variable) { $1 }
  ;

stmt_block:
  | BRACE_OPEN list(variable_decl) /*list(stmt)*/ BRACE_CLOSE {}
  ;

variable:
  | type_ ID { ($1, $2) }
  ;

type_:
  | type_ ARRAY_DECL { ArrayType $1 }
  | TYPE { PrimitiveType $1 }
  | ID { ComplexType $1 }
  ;
