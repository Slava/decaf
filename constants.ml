open Core.Std

type decaf_constant =
  | IntConstant of int
  | DoubleConstant of float
  | StringConstant of string
  | BoolConstant of bool
  | CharConstant of char
;;

let stringify_constant c =
  match c with
  | IntConstant i -> string_of_int i
  | DoubleConstant f -> Float.to_string f
  | StringConstant s -> "\"" ^ s ^ "\""
  | BoolConstant b -> string_of_bool b
  | CharConstant c -> String.of_char c
;;
