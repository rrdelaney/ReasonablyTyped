open Modulegen.BsDecl;

open Modulegen.BsType;

module Utils = {
  let unquote str => String.concat "" (String.split_on_char '\'' str);
  let to_module_name str => {
    let toks = String.split_on_char '-' (unquote str);
    String.concat "_" toks
  };
};

let rec bstype_to_code =
  fun
  | Unit => "()"
  | Null => "null"
  | Unknown => "??"
  | Any => "_"
  | Object props => "object"
  | Number => "number"
  | String => "string"
  | Boolean => "bool"
  | Function params rt =>
    String.concat " => " (List.map (fun (name, param_type) => bstype_to_code param_type) params) ^
    " => " ^ bstype_to_code rt;

let rec declaration_to_code module_id =>
  fun
  | VarDecl id type_of =>
    "external " ^
    id ^
    " : " ^ bstype_to_code type_of ^ " = \"\" [@@bs.module \"" ^ Utils.unquote module_id ^ "\"];"
  | FuncDecl id type_of =>
    "external " ^
    id ^
    " : " ^ bstype_to_code type_of ^ " = \"\" [@@bs.module \"" ^ Utils.unquote module_id ^ "\"];"
  | ExportsDecl type_of =>
    "external " ^
    Utils.to_module_name module_id ^
    " : " ^ bstype_to_code type_of ^ " = \"" ^ Utils.unquote module_id ^ "\" [@@bs.module];"
  | ModuleDecl id statements =>
    "module " ^
    id ^ " = {\n" ^ String.concat "\n  " (List.map (declaration_to_code id) statements) ^ "\n};"
  | TypeDecl => "$$type$$"
  | Unknown => "??;";

let stack_to_code =
  fun
  | ModuleDecl id statements =>
    Some (id, String.concat "\n" (List.map (declaration_to_code id) statements))
  | _ => None;
