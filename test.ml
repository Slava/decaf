open Core.Std
open Lexer
open Lexing
open Types
open Ast

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let lex lexbuf =
  Lexer.decaf lexbuf

let parse_with_error lexbuf =
  try Parser.program Lexer.decaf lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    exit (-1)
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let rec parse_and_print lexbuf =
  let ast = parse_with_error lexbuf in
  printf "%s\n" (Ast.stringify_ast ast)

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx

let () =
  Command.basic ~summary:"Parse Decaf programs (WIP)"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop
  |> Command.run
