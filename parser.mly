%{
open Ast
open Constants
open Types
module L = Location

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

%inline loc(X):
  | x = X { L.mkloc x (L.mk $startpos $endpos) }

%inline loc_opt(X):
  | x = X
    {
      match x with
      | None -> None
      | Some v -> Some (L.mkloc v (L.mk $startpos $endpos))
    }

/* list of declarations */
program:
  | p = loc(program_)
    {
      Program (L.mkloc { program_body = p.L.item; } p.L.loc)
    }
  ;

program_:
  | EOF { [] }
  | d = loc(decl); rest = program_ { d :: rest }
  ;

decl:
  | l = loc(variable_decl) { Variable l }
  | l = loc(function_decl) { Function l }
  | l = loc(class_decl) { Class l }
  | l = loc(interface_decl) { Interface l }
  ;

variable_decl:
  | variable SEMICOLON
    { $1 }
  ;

function_decl:
  /* Type name (Type a, Type b) stmt_block */
  | func_return_type = loc(type_);
    func_name = loc(ID);
    PAREN_OPEN; func_arguments = formals; PAREN_CLOSE;
    func_body = loc(stmt_block)
    {
      { func_name; func_arguments; func_return_type; func_body; }
    }
  ;

class_decl:
  | CLASS; class_name = loc(ID);
    ext = option(loc(class_decl_extends));
    implmnts = class_decl_implements;
    BRACE_OPEN; fields = class_fields; BRACE_CLOSE;
    {
      {
        class_name;
        super = ext;
        interfaces = implmnts;
        methods = (fst fields);
        class_properties = (snd fields);
      }
    }
  ;

class_decl_extends:
  | EXTENDS ID { $2 }
  ;

class_decl_implements:
  | /* empty */ { [] }
  | IMPLEMENTS l = separated_list(COMMA, loc(ID)) { l }
  ;

class_fields:
  | /* empty */ { ([], []) }
  | vd = loc(variable_decl); rest = class_fields
    { ((fst rest), vd::(snd rest)) }
  | vd = loc(function_decl); rest = class_fields
    { (vd::(fst rest), (snd rest)) }
  ;

interface_decl:
  | INTERFACE;
    interface_name = loc(ID);
    BRACE_OPEN; props = list(loc(prototype)); BRACE_CLOSE;
    {
      {
        interface_name;
        interface_properties = props;
      }
    }
  ;

prototype:
  | proto_return_type = loc(type_);
    proto_name = loc(ID);
    PAREN_OPEN; proto_arguments = formals; PAREN_CLOSE; SEMICOLON;
    {
      { proto_return_type; proto_name; proto_arguments; }
    }
  ;

formals:
  | variables = separated_list(COMMA, loc(variable)) { variables }
  ;

stmt_block:
  | BRACE_OPEN; bl = stmt_block_; BRACE_CLOSE
    { bl }
  ;

stmt_block_:
  | /* empty */ { { declarations = []; statements = []; } }
  | var = loc(variable); SEMICOLON; rest = stmt_block_;
    {
      {
        declarations = var::rest.declarations;
        statements = rest.statements;
      }
    }
  | stmt = loc_opt(stmt); statements = list(loc_opt(stmt))
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
  | opt_expr = option(loc(expr)) SEMICOLON
      {
        match opt_expr with
        | None -> None
        | Some l -> Some (Expression l)
      }
  | l = loc(print_stmt)
      { Some (Expression l); }
  | l = loc(stmt_block)
      { Some (StatementBlock l) }
  | l = loc(if_stmt)
      { Some (IfStatement l) }
  | l = loc(while_stmt)
      { Some (LoopStatement l) }
  | l = loc(for_stmt)
      { Some (LoopStatement l) }
  | l = loc(return_stmt)
      { Some (ReturnStatement l) }
  | l = loc(break_stmt)
      { Some (BreakStatement l) }
  ;

if_stmt:
  | IF; PAREN_OPEN; cond = loc(expr); PAREN_CLOSE;
    cons = loc_opt(stmt); altern = opt_else_tail;
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
  | ELSE; st = loc_opt(stmt);
    { st }
  ;

while_stmt:
  | WHILE; PAREN_OPEN; cond = loc(expr); PAREN_CLOSE; body = loc_opt(stmt);
    {
      {
        loop_type = While;
        loop_initialization = None;
        loop_condition = cond;
        loop_afterthought = None;
        loop_body = body;
      }
    }
  ;

for_stmt:
  | FOR; PAREN_OPEN;
    loop_initialization = option(loc(expr)); SEMICOLON;
    loop_condition = loc(expr); SEMICOLON;
    loop_afterthought = option(loc(expr)); PAREN_CLOSE;
    loop_body = loc_opt(stmt);
    {
      {
        loop_type = For;
        loop_initialization;
        loop_condition;
        loop_afterthought;
        loop_body;
      }
    }
  ;

return_stmt:
  | RETURN; opt_expr = option(loc(expr)); SEMICOLON;
    {
      { value = opt_expr; }
    }
  ;

break_stmt:
  | BREAK; SEMICOLON;
    { }
  ;
print_stmt:
  | p = loc(print); PAREN_OPEN; el = separated_list(COMMA, loc(expr)); PAREN_CLOSE; SEMICOLON
      {
        match el with
        | [] -> raise (SyntaxError ("Print must have at least one argument."))
        | el -> CallExpression {
            callee = p;
            call_arguments = el;
          }
      }
  ;

print:
  | PRINT { Symbol("Print") }
  ;

expr:
  /* implicitly make '=' operator right-associative */
  | lv = loc(lvalue); EQUALS; e = loc(expr)
    {
      AssignmentExpression
        {
          lvalue = lv;
          rvalue = e;
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
  | l = loc(expr); op = loc(OP); r = loc(expr);
    {
      ArithmeticExpression {
        loperand = l;
        roperand = Some r;
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
  | op = loc(OP); e = loc(expr);
    {
      ArithmeticExpression {
        loperand = e;
        roperand = None;
        operator = op;
      }
    }
  ;

lvalue:
  | ID { Symbol($1) }
  | es = loc(expr_simple); DOT; m = loc(ID)
    {
      MemberExpression {
        host = es;
        member = L.mkloc (Symbol m.L.item) m.L.loc;
      }
    }
    | es = loc(expr_simple); BRACKET_OPEN; e = loc(expr); BRACKET_CLOSE
    {
      ArrayExpression { array = es; index = e; }
    }
  ;

alloc_expr:
  | NEW t = loc(type_)
    {
      AllocExpression {
        type_ = t;
        size = None;
      }
    }
  ;

call:
  /* this doesn't follow decaf's spec precisely */
  /* according to the spec, an expr like a[1]() is not a valid call */
  | lval = loc(lvalue) PAREN_OPEN args = actuals PAREN_CLOSE
    {
      CallExpression {
        callee = lval;
        call_arguments = args;
      }
    }
  | l = loc(READINTEGER) PAREN_OPEN PAREN_CLOSE
      {
        CallExpression {
          callee = L.mkloc (Symbol "ReadInteger") l.L.loc;
          call_arguments = [];
        }
      }
  | l = loc(READLINE) PAREN_OPEN PAREN_CLOSE
      {
        CallExpression {
          callee = L.mkloc (Symbol "ReadLine") l.L.loc;
          call_arguments = [];
        }
      }
  | NEWARRAY PAREN_OPEN size = loc(expr); COMMA; t = loc(type_); PAREN_CLOSE
      {
        AllocExpression {
          type_ = t;
          size = Some size;
        }
      }
  ;

actuals:
  | l = separated_list(COMMA, loc(expr)) { l }
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
