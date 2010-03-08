%{
  open Printf
  open Syntax
  
  let eval_value_symbol expr =
    match expr with
    | Value (Symbol s) -> GetVar s
    | _ -> expr
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
  | expr_list EOF            { $1 }

expr_list:
  | exprs                    { ExprList $1 }

exprs:
  | TERM                     { [] }
  | expr TERM                { [$1] }
  | exprs expr TERM          { $1 @ [$2] }
  | exprs TERM               { $1 }

expr:
  | value                    { 
    Value $1 }
  | symbol EQUAL expr        { SetVar ($1, $3) }
  | symbol LPAREN func_params RPAREN { CallFunc ($1, ExprList $3) }

value:
  | INT                      { Int ($1) }
  | MINUS INT %prec UMINUS   { Int (-$2) }
  | STRING                   { String $1 }
  | symbol                   { Symbol $1 }
  | FUNC LPAREN func_args RPAREN LBRACE expr_list RBRACE { Func ($3, $6) }

symbol:
  | SYMBOL                   { $1 }

func_def:
  | FUNC symbol LPAREN func_args RPAREN LBRACE expr_list RBRACE { 
    SetVar ($2, Value (Func ($4, $7)))
  }

func_args:
  | symbol                   { [$1] } 
  | func_args COMMA symbol   { $1 @ [$3] }

func_params:
  | expr                     { [eval_value_symbol $1] }
  | func_params COMMA expr   { $1 @ [eval_value_symbol $3] }

