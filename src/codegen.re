open Modulegen.BsDecl;

open Modulegen.BsType;

let rec declaration_to_code =
  fun
  | VarDecl id type_of => "external " ^ id ^ " : (type) = \"\" [@@bs.val]"
  | FuncDecl id type_of => "external " ^ id ^ " : (type) = \"\" [@@bs.send]"
  | ModuleDecl id statements =>
    "module " ^
    id ^ " = {\n" ^ String.concat "\n  " (List.map declaration_to_code statements) ^ "\n}"
  | Unknown => "??";

let stack_to_code =
  fun
  | ModuleDecl id statements =>
    Some (id, String.concat "\n" (List.map declaration_to_code statements))
  | _ => None;
