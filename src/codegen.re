open Modulegen.BsDecl;

open Modulegen.BsType;

module Utils = {
  let unquote str => String.sub str 1 (String.length str - 2);
  let to_module_name str =>
    String.map
      (
        fun ch =>
          if (ch == '-') {
            '_'
          } else {
            ch
          }
      )
      (unquote str);
};

let rec bstype_to_code =
  fun
  | Unit => "()"
  | Null => "null"
  | Unknown => "??"
  | Any => "_"
  | Object props =>
    "Js.t <" ^
    String.concat "," (List.map (fun (key, type_of) => key ^ ": " ^ bstype_to_code type_of) props) ^ ">"
  | Number => "float"
  | String => "string"
  | Boolean => "Js.boolean"
  | Union types => "$$union of " ^ String.concat ", " (List.map bstype_to_code types) ^ "$$"
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
  | TypeDecl id type_of => "type " ^ id ^ " = " ^ bstype_to_code type_of ^ ";"
  | Unknown => "??;";

let stack_to_code =
  fun
  | ModuleDecl id statements =>
    Some (
      Utils.to_module_name id,
      String.concat "\n" (List.map (declaration_to_code id) statements)
    )
  | _ => None;
