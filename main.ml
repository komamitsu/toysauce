open Printf
open Syntax

let debug = ref false
let source = ref stdin

let _ =
  let args_spec = [
    "-g", Arg.Set debug, "for debugging";
  ] in
  Arg.parse args_spec (fun file -> source := open_in file)
    (sprintf "usage: %s [options] [programfile]" Sys.argv.(0));
  Printexc.record_backtrace true;
  let lexbuf = Lexing.from_channel !source in
  try 
    let rec loop parsed =
      let result = Parser.main Lexer.token lexbuf in
      result::parsed in
    let expr_list = loop [] in
    if !debug 
    then print_endline (string_of_expr (ExprList expr_list)); flush stdout;
    ignore (Eval.eval_expr (Env.empty ()) (ExprList expr_list))
  with 
  | Parsing.Parse_error ->
    Printf.eprintf "Parse error near \"%s\" in line %d\n"
      (String.sub lexbuf.Lexing.lex_buffer 
        lexbuf.Lexing.lex_start_pos 
        (lexbuf.Lexing.lex_curr_pos - lexbuf.Lexing.lex_start_pos))
      !Lexer.line
  | e -> 
    Printf.eprintf "Unexpected exception : %s\n" (Printexc.to_string e);
    Printexc.print_backtrace stderr

