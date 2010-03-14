{
  open Printf
  open Parser
  let line = ref 0
}
rule token = parse
  | [' ' '\t']+         { token lexbuf }
  | '\n'                { line := !line + 1; token lexbuf }
	| ['0'-'9']+               { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '"' [^ '"']* '"'        { 
    let s = Lexing.lexeme lexbuf in
    let striped = 
      String.sub s 1 ((String.length s) -2) in
    STRING striped 
  }
  | "func"                   { FUNC }
  | "if"                     { IF }
  | "else"                   { ELSE }
  | ['0'-'9' '_' 'a'-'z']+   { SYMBOL (Lexing.lexeme lexbuf) }
	| '+'                      { PLUS }
	| '-'                      { MINUS }
	| '*'                      { TIMES }
	| '/'                      { DIV }
	| '('                      { LPAREN }
	| ')'                      { RPAREN }
	| '{'                      { LBRACE }
	| '}'                      { RBRACE }
	| '['                      { LBRACKET }
	| ']'                      { RBRACKET }
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
 
