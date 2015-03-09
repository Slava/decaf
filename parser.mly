%{
open Ast
open Constants
open Types

exception SyntaxError of string
%}

%token <int> INT
%token <float> DOUBLE
%token <string> ID
%token <string> STRING
%token <string> TYPE
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
%token NEWARRAY
%token PRINT
%token READINTEGER
%token READLINE
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
%token <string> UN_LOG_OP
%token <string> BIN_UN_ADD_OP
%token <string> BIN_MULT_OP
%token <string> BIN_CMP_OP
%token <string> BIN_LOG_OP

%right EQUALS
%left BIN_LOG_OP
%nonassoc BIN_CMP_OP
%left BIN_UN_ADD_OP
%left BIN_MULT_OP
%nonassoc UN_LOG_OP

%nonassoc NOELSE
%nonassoc ELSE

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
  | variable_decl { Variable $1 }
  | function_decl { Function $1 }
  | class_decl { Class $1 }
  | interface_decl { Interface $1 }
  ;

variable_decl:
  | variable SEMICOLON
    { $1 }
  ;

function_decl:
  /* Type name (Type a, Type b) stmt_block */
  | return_type = type_ name = ID PAREN_OPEN arguments = formals
    PAREN_CLOSE body = stmt_block
    {
      { name; arguments; return_type; body; }
    }
  ;

class_decl:
  | CLASS; name = ID;
    ext = option(class_decl_extends); implmnts = class_decl_implements;
    BRACE_OPEN; fields = class_fields; BRACE_CLOSE;
    {
      {
        name;
        super = ext;
        interfaces = implmnts;
        methods = (fst fields);
        properties = (snd fields);
      }
    }
  ;

class_decl_extends:
  | EXTENDS ID { $2 }
  ;

class_decl_implements:
  | /* empty */ { [] }
  | IMPLEMENTS l = separated_list(COMMA, ID) { l }
  ;

class_fields:
  | /* empty */ { ([], []) }
  | variable_decl class_fields
    { ((fst $2), $1::(snd $2)) }
  | function_decl class_fields
    { ($1::(fst $2), (snd $2)) }
  ;

interface_decl:
  | INTERFACE; name = ID; BRACE_OPEN; props = list(prototype); BRACE_CLOSE;
    {
      {
        name;
        properties = props;
      }
    }
  ;

prototype:
  | return_type = type_; name = ID; PAREN_OPEN; arguments = formals; PAREN_CLOSE; SEMICOLON;
    {
      { return_type; name; arguments; }
    }
  ;

formals:
  | variables = separated_list(COMMA, variable) { variables }
  ;

stmt_block:
  | BRACE_OPEN; bl = stmt_block_; BRACE_CLOSE
    { bl }
  ;

stmt_block_:
  | /* empty */ { { declarations = []; statements = []; } }
  | var = variable; SEMICOLON; rest = stmt_block_;
    {
      {
        declarations = var::rest.declarations;
        statements = rest.statements;
      }
    }
  | stmt = stmt; statements = list(stmt)
    {
      (* deoptionalize statements *)
      let stmts = (Core.Std.List.filter_map ~f:Core.Std.Fn.id (stmt::statements)) in
        {
          declarations = [];
          statements = stmts;
        }
    }
  ;

stmt:
  | opt_expr = option(expr) SEMICOLON
      {
        match opt_expr with
        | None -> None
        | Some expr -> Some (Expression expr)
      }
  | p = print_stmt
      { Some p; }
  | stmt_block
      { Some (StatementBlock $1) }
  | if_stmt
      { Some (IfStatement $1) }
  | while_stmt
      { Some (WhileStatement $1) }
  | for_stmt
      { Some (ForStatement $1) }
  | return_stmt
      { Some (ReturnStatement $1) }
  | break_stmt
      { Some BreakStatement }
  ;

if_stmt:
  | IF PAREN_OPEN cond = expr; PAREN_CLOSE cons = stmt; altern = opt_else_tail;
    {
      {
        condition = cond;
        consequence = cons;
        alternative = altern;
      }
    }
  ;

opt_else_tail:
  | /* empty */ %prec NOELSE
    { None }
  | ELSE stmt;
    { $2 }
  ;

while_stmt:
  | WHILE; PAREN_OPEN; cond = expr; PAREN_CLOSE; body = stmt;
    {
      {
        condition = cond;
        body = body;
      }
    }
  ;

for_stmt:
  | FOR; PAREN_OPEN;
      initialization = option(expr); SEMICOLON;
      condition = expr; SEMICOLON;
      afterthought = option(expr); PAREN_CLOSE;
    body = stmt;
    {
      { initialization; condition; afterthought; body; }
    }
  ;

return_stmt:
  | RETURN; opt_expr = option(expr); SEMICOLON;
    {
      { value = opt_expr; }
    }
  ;

break_stmt:
  | BREAK; SEMICOLON;
    { }
  ;
print_stmt:
  | PRINT; PAREN_OPEN; el = separated_list(COMMA, expr); PAREN_CLOSE; SEMICOLON
      {
        match el with
        | [] -> raise (SyntaxError ("Print must have at least one argument."))
        | el -> Expression(CallExpression {
                  callee = Symbol("Print");
                  arguments = el;
                })
      }
  ;

expr:
  /* implicitly make '=' operator right-associative */
  | lvalue EQUALS expr
    {
      AssignmentExpression
        {
          lvalue = $1;
          rvalue = $3;
        }
    }
  | expr_simple { $1 }
  | ae = expr_arithm { ae }
  | ae = expr_un_arithm { ae }
  | e = alloc_expr { e }
  ;

(* extra rule to resolve shift-reduce conflict of operators being associative *)
expr_simple:
  | constant { Constant $1 }
  | THIS { This }
  | call { $1 }
  | lvalue { $1 }
  | PAREN_OPEN; expr; PAREN_CLOSE; { $2 }
  ;

expr_arithm:
  | e = expr_arithm_gen(BIN_LOG_OP)
  | e = expr_arithm_gen(BIN_UN_ADD_OP)
  | e = expr_arithm_gen(BIN_MULT_OP)
  | e = expr_arithm_gen(BIN_CMP_OP)
      { e }
  ;

%inline expr_arithm_gen(OP):
  | l = expr; op = OP; r = expr;
    {
      ArithmeticExpression {
        loperand = l;
        roperand = r;
        operator = op;
      }
    }
  ;

expr_un_arithm:
  | e = expr_un_arithm_gen(BIN_UN_ADD_OP)
  | e = expr_un_arithm_gen(UN_LOG_OP)
      { e }
  ;

%inline expr_un_arithm_gen(OP):
  | op = OP; e = expr;
    {
      UnArithmeticExpression {
        operand = e;
        operator = op;
      }
    }
  ;

lvalue:
  | ID { Symbol($1) }
  | expr_simple DOT ID { MemberExpression { host = $1; member = Symbol($3); } }
  | expr_simple BRACKET_OPEN expr BRACKET_CLOSE
    {
      ArrayExpression { array = $1; index = $3; }
    }
  ;

alloc_expr:
  | NEW t = type_
    {
      AllocExpression {
        type_ = t;
      }
    }
  ;

call:
  /* this doesn't follow decaf's spec precisely */
  /* according to the spec, an expr like a[1]() is not a valid call */
  | lval = lvalue PAREN_OPEN args = actuals PAREN_CLOSE
    {
      CallExpression {
        callee = lval;
        arguments = args;
      }
    }
  | READINTEGER PAREN_OPEN PAREN_CLOSE
      {
        CallExpression {
          callee = Symbol("ReadInteger");
          arguments = [];
        }
      }
  | READLINE PAREN_OPEN PAREN_CLOSE
      {
        CallExpression {
          callee = Symbol("ReadLine");
          arguments = [];
        }
      }
  | NEWARRAY PAREN_OPEN size = expr; COMMA; t = type_; PAREN_CLOSE
      {
        ArrayAllocExpression {
          type_ = t;
          size = size;
        }
      }
  ;

actuals:
  | l = separated_list(COMMA, expr) { l }
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
