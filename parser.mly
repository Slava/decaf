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

%left OP

%start <(string * string) list> program
%%

/* list of declarations */
program:
  | EOF { [] }
  | decl program { $1 :: $2 }
  ;

decl:
  | variable_decl { $1 }
  ;

variable_decl:
  | variable { $1 }
  ;

variable:
  | TYPE ID { ($1, $2) }
  ;
