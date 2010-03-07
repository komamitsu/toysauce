{
  open Printf
  open Parser
  exception Eof
}
rule token = parse
  | [' ' '\t' '\n']+       { 
printf "blank\n"; flush stdout;
    token lexbuf }
	| ['0'-'9']+             { 
printf "integer\n"; flush stdout;
    INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '"' [^ '\n']* '"'      { 
    let s = Lexing.lexeme lexbuf in
    let striped = String.sub s 1 ((String.length s) -2) in
printf "string\n"; flush stdout;
    STRING striped
  }
  | ['0'-'9' 'a'-'z']+     { 
printf "symbol\n"; flush stdout;
    SYMBOL (Lexing.lexeme lexbuf) }
	| '+'            { PLUS }
	| '-'            { MINUS }
	| '*'            { TIMES }
	| '/'            { DIV }
	| '('            { LPAREN }
	| ')'            { RPAREN }
	| '{'            { LBRACE }
	| '}'            { RBRACE }
  | ';'            { 
printf "term\n"; flush stdout;
    TERM }
  | ','            { COMMA }
  | '='            { 
printf "equal\n"; flush stdout;
    EQUAL }
(*	| eof            { raise Eof } *)
	| eof            { EOF }
  | _ as other     { printf "Unrecognized character: %c\n" other; token lexbuf }
 
