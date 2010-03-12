{
  open Printf
  open Parser
  exception Eof
}
rule token = parse
  | [' ' '\t' '\n']+         { token lexbuf }
	| ['0'-'9']+               { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '"' [^ '\n']* '"'        { 
    let s = Lexing.lexeme lexbuf in
    let striped = 
      String.sub s 1 ((String.length s) -2) in
    STRING striped 
  }
  | "func"                   { FUNC }
  | ['0'-'9' 'a'-'z']+       { SYMBOL (Lexing.lexeme lexbuf) }
	| '+'                      { PLUS }
	| '-'                      { MINUS }
	| '*'                      { TIMES }
	| '/'                      { DIV }
	| '('                      { LPAREN }
	| ')'                      { RPAREN }
	| '{'                      { LBRACE }
	| '}'                      { RBRACE }
  | ';'                      { TERM }
  | ','                      { COMMA }
  | "=="                     { EQUAL }
  | "!="                     { NOTEQUAL }
  | ">"                      { GT }
  | ">="                     { GE }
  | "<"                      { LT }
  | "<="                     { LE }
  | '='                      { ASSIGN }
	| eof                      { EOF }
  | _ as other               { 
    printf "Unrecognized character: %c\n" other; token lexbuf 
  }
 
