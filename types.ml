type decaf_type =
  | PrimitiveType of string
  | ArrayType of decaf_type
  | ComplexType of string
;;

let rec stringify_type t =
  match t with
  | PrimitiveType (name) -> name
  | ArrayType (subt) -> "<array " ^ (stringify_type subt) ^ ">"
  | ComplexType (name) -> "<complex " ^ name ^ ">"
