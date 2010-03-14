%{
  open Printf
  open Syntax
  open Eval
  
  let eval_param expr =
    match expr with
    | Value (Var s) -> GetVar s
    | _ -> expr
%}
%token <int> INT
%token <string> STRING
%token <string> VAR
%token FUNC
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token ASSIGN
%token TERM COMMA
%token EQUAL NOTEQUAL GT GE LT LE 
%token IF ELSE
%token PLUS MINUS
%token TIMES DIV
%token EOF
%left ASSIGN
%left EQUAL NOTEQUAL GT GE LT LE 
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

expr:
  | value                    { Value $1 }
  | var ASSIGN expr          { SetVar ($1, $3) }
  | var LPAREN src_of_expr_list RPAREN { CallFunc ($1, ExprList $3) }
  | LPAREN expr RPAREN       { $2 }
  | expr  PLUS  expr         { 
    CallFunc ("+", ExprList[eval_param $1; eval_param $3])
  }
  | expr  MINUS expr         { 
    CallFunc ("-", ExprList[eval_param $1; eval_param $3])
  }
  | expr  TIMES expr         { 
    CallFunc ("*", ExprList[eval_param $1; eval_param $3])
  }
  | expr  DIV   expr         { 
    CallFunc ("/", ExprList[eval_param $1; eval_param $3])
  }
  | expr EQUAL    expr       { 
    CallFunc ("==", ExprList[eval_param $1; eval_param $3])
  }
  | expr NOTEQUAL expr       { 
    CallFunc ("!=", ExprList[eval_param $1; eval_param $3])
  }
  | expr GT       expr       { 
    CallFunc (">", ExprList[eval_param $1; eval_param $3])
  }
  | expr GE       expr       { 
    CallFunc (">=", ExprList[eval_param $1; eval_param $3])
  }
  | expr LT       expr       { 
    CallFunc ("<", ExprList[eval_param $1; eval_param $3])
  }
  | expr LE       expr       { 
    CallFunc ("<=", ExprList[eval_param $1; eval_param $3])
  }
  | IF LPAREN expr RPAREN LBRACE expr_list RBRACE ELSE LBRACE expr_list RBRACE {
    CallFunc ("if", ExprList[eval_param $3; $6; $10])
  }
  | def_func                 { $1 }

value:
  | FUNC LPAREN func_args RPAREN LBRACE expr_list RBRACE { 
    Func ($3, $6) 
  }
  | INT                      { Int ($1) }
  | MINUS INT %prec UMINUS   { Int (-$2) }
  | STRING                   { String $1 }
  | var                      { Var $1 }
  | LBRACKET value_list RBRACKET { Collection $2 }

value_list:
  |                          { [] }
  | value                    { [$1] }
  | value_list COMMA value   { $1 @ [$3] }

var:
  | VAR                      { $1 }

src_of_expr_list:
  | expr                          { [eval_param $1] }
  | src_of_expr_list COMMA expr   { $1 @ [eval_param $3] }

def_func:
  | FUNC var LPAREN func_args RPAREN LBRACE expr_list RBRACE { 
    SetVar ($2, Value (Func ($4, $7)))
  }

func_args:
  | var                   { [$1] } 
  | func_args COMMA var   { $1 @ [$3] }

