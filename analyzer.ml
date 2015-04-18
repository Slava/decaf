open Ast

exception ReferenceError of string

let check ast =
  raise (ReferenceError "nope")

