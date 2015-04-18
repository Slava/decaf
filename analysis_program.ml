open Core.Std
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.program Lexer.decaf lexbuf with
  | Lexer.SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    exit (-1)
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let analyze_with_error ast =
  try Analyzer.check ast with
  | Analyzer.ReferenceError msg ->
    fprintf stderr "%s\n" msg;
    exit (-1)

let analyze_and_check lexbuf =
  let ast = parse_with_error lexbuf in
  let ext_ast = analyze_with_error ast in
  printf "no errors\n"

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  analyze_and_check lexbuf;
  In_channel.close inx

let () =
  Command.basic ~summary:"Parse and Analyze Decaf programs (WIP)"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop
  |> Command.run
