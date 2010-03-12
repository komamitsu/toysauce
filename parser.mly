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
%token ASSIGN
%token TERM COMMA
%token EQUAL NOTEQUAL GT GE LT LE 
%token EOF
%token PLUS MINUS
%token TIMES DIV
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
  | def_func                 { [$1] }
  | exprs expr TERM          { $1 @ [$2] }
  | exprs def_func           { $1 @ [$2] }

def_func:
  | FUNC symbol LPAREN func_args RPAREN LBRACE expr_list RBRACE { 
    SetVar ($2, Value (Func ($4, $7)))
  }

expr:
  | value                    { Value $1 }
  | symbol ASSIGN expr       { SetVar ($1, $3) }
  | symbol LPAREN func_params RPAREN { CallFunc ($1, ExprList $3) }
  | expr  PLUS  expr         { 
    CallFunc ("+", ExprList[eval_value_symbol $1; eval_value_symbol $3])
  }
  | expr  MINUS expr         { 
    CallFunc ("-", ExprList[eval_value_symbol $1; eval_value_symbol $3])
  }
  | expr  TIMES expr         { 
    CallFunc ("*", ExprList[eval_value_symbol $1; eval_value_symbol $3])
  }
  | expr  DIV   expr         { 
    CallFunc ("/", ExprList[eval_value_symbol $1; eval_value_symbol $3])
  }
  | expr EQUAL    expr       { 
    CallFunc ("==", ExprList[eval_value_symbol $1; eval_value_symbol $3])
  }
  | expr NOTEQUAL expr       { 
    CallFunc ("!=", ExprList[eval_value_symbol $1; eval_value_symbol $3])
  }
  | expr GT       expr       { 
    CallFunc (">", ExprList[eval_value_symbol $1; eval_value_symbol $3])
  }
  | expr GE       expr       { 
    CallFunc (">=", ExprList[eval_value_symbol $1; eval_value_symbol $3])
  }
  | expr LT       expr       { 
    CallFunc ("<", ExprList[eval_value_symbol $1; eval_value_symbol $3])
  }
  | expr LE       expr       { 
    CallFunc ("<=", ExprList[eval_value_symbol $1; eval_value_symbol $3])
  }

value:
  | FUNC LPAREN func_args RPAREN LBRACE expr_list RBRACE { 
    Func ($3, $6) 
  }
  | INT                      { Int ($1) }
  | MINUS INT %prec UMINUS   { Int (-$2) }
  | STRING                   { String $1 }
  | symbol                   { Symbol $1 }

symbol:
  | SYMBOL                   { $1 }


func_args:
  | symbol                   { [$1] } 
  | func_args COMMA symbol   { $1 @ [$3] }

func_params:
  | expr                     { [eval_value_symbol $1] }
  | func_params COMMA expr   { $1 @ [eval_value_symbol $3] }

