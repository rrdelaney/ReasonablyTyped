open Modulegen.BsDecl;

open Modulegen.BsType;

let rec bstype_to_code =
  fun
  | Unit => "()"
  | Null => "null"
  | Unknown => "??"
  | Any => "any"
  | Number => "number"
  | String => "string"
  | Boolean => "boolean"
  | Function params rt =>
    String.concat " => " (List.map (fun (name, param_type) => bstype_to_code param_type) params) ^
    " => " ^ bstype_to_code rt;

let rec declaration_to_code id =>
  fun
  | VarDecl id type_of =>
    "external " ^ id ^ " : " ^ bstype_to_code type_of ^ " = \"\" [@@bs.module \"" ^ id ^ "\"];"
  | FuncDecl id type_of =>
    "external " ^ id ^ " : " ^ bstype_to_code type_of ^ " = \"\" [@@bs.module \"" ^ id ^ "\"];"
  | ExportsDecl type_of =>
    "external " ^ id ^ " : " ^ bstype_to_code type_of ^ " = \"\" [@@bs.module];"
  | ModuleDecl id statements =>
    "module " ^
    id ^ " = {\n" ^ String.concat "\n  " (List.map (declaration_to_code id) statements) ^ "\n};"
  | Unknown => "??;";

let stack_to_code =
  fun
  | ModuleDecl id statements =>
    Some (id, String.concat "\n" (List.map (declaration_to_code id) statements))
  | _ => None;
