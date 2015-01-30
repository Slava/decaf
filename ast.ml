open Types

type ast_variable = (decaf_type * string);;

type ast_node_function =
  {
    name: string;
    arguments: ast_variable list;
    (* TODO: return value? *)
  }
;;

type ast_node_type =
  | Variable of ast_variable
  | Function of ast_node_function
  | Program
;;

type ast =
  {
    tag: ast_node_type;
    children: ast list;
  }
;;

let stringify_pair v =
  "<" ^ (fst v) ^ " " ^ (snd v) ^ ">"
;;

let stringify_var_pair v =
  stringify_pair (stringify_type (fst v), (snd v))
;;

let stringify_tag t =
  match t with
  | Variable var -> stringify_var_pair var
  | Function func ->
     stringify_pair ("Function",
                     func.name ^ ": " ^
                       (String.concat ", " (List.map stringify_var_pair func.arguments)))
  | Program -> "Program"
;;

let rec stringify_ast ?(indent=0) (v: ast) =
  (String.make indent ' ')
  ^ (stringify_tag v.tag)
  ^ (match v.children with
     | [] -> ""
     | hd::tl -> (String.concat
                    "\n"
                    (""::List.map
                           (stringify_ast ~indent:(indent+1))
                           v.children)))
;;
