%{
  open Printf
  open Syntax
%}
%token <int> INT
%token <string> STRING
%token <string> SYMBOL
%token FUNC
%token LPAREN RPAREN
%token LBRACE RBRACE
%token EQUAL
%token TERM COMMA
%token EOF
%token PLUS MINUS
%token TIMES DIV
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS
%start main
%type <Syntax.expr> main
%%
main:
  | expr_list EOF           { $1 }

expr_list:
  | exprs                   { ExprList $1 }

exprs:
  | void TERM               { 
printf "exprs: TERM\n"; flush stdout;
    [] }
  | expr TERM               { 
printf "exprs: expr TERM\n"; flush stdout;
    [$1] }
  | exprs expr TERM    {
printf "exprs: exprs expr TERM\n"; flush stdout;
    $1 @ [$2] }
  | exprs void TERM    {
printf "exprs: exprs void TERM\n"; flush stdout;
    $1 }

void: {}

expr:
  | value                   { 
printf "expr: value\n"; flush stdout;
    Value $1 }
  | symbol EQUAL expr       { 
printf "expr: symbol EQUAL expr\n"; flush stdout;
    SetVar ($1, $3) }
  | symbol LPAREN func_params RPAREN { 
printf "expr: callfunc\n"; flush stdout;
    CallFunc ($1, ExprList $3) }

value:
  | INT                     { 
printf "value: INT\n"; flush stdout;
    Int ($1) }
  | MINUS INT %prec UMINUS  { 
printf "value: MINUS INT prec UMINUS\n"; flush stdout;
    Int (-$2) }
  | STRING                  { String $1 }
  | symbol                  { 
printf "value: symbol\n"; flush stdout;
    Symbol $1 }
  | FUNC LPAREN func_args RPAREN LBRACE expr_list RBRACE { 
printf "value: func\n"; flush stdout;
    Func ($3, $6)
  }

symbol:
  | SYMBOL                  { 
printf "symbol\n"; flush stdout;
    $1 }

func_def:
  | FUNC symbol LPAREN func_args RPAREN LBRACE expr_list RBRACE { 
    SetVar ($2, Value (Func ($4, $7)))
  }

func_args:
  | symbol                  { [$1] } 
  | func_args COMMA symbol  { $1 @ [$3] }

func_params:
  | expr                    { [$1] }
  | func_params COMMA expr  { $1 @ [$3] }

