open Core.Std
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let lex_with_error lexbuf =
  try Lexer.decaf lexbuf with
  | Lexer.SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    exit (-1)

let rec lex_and_print lexbuf =
  let lexem = lex_with_error lexbuf in
  printf "%s\n" (Lexer.stringify_token lexem);
  match lexem with
  | EOF -> ()
  | _ -> lex_and_print lexbuf

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  lex_and_print lexbuf;
  In_channel.close inx

let () =
  Command.basic ~summary:"Parse Lex programs (WIP)"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop
  |> Command.run
