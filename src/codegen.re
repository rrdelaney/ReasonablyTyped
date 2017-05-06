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

let rec bstype_name =
  fun
  | Unit => "unit"
  | Null => "null"
  | Any => "any"
  | Object _ => "object"
  | Number => "number"
  | String => "string"
  | Boolean => "bool"
  | Function _ => "func"
  | Unknown => "unknown"
  | Named s => String.uncapitalize_ascii s
  | Union types => union_types_to_name types
and union_types_to_name types => {
  let type_names = List.map bstype_name types;
  String.concat "_or_" type_names
};

let rec bstype_to_code =
  fun
  | Unit => "()"
  | Null => "null"
  | Unknown => "??"
  | Any => "_"
  | Object props =>
    "Js.t <" ^
    String.concat ", " (List.map (fun (key, type_of) => key ^ ": " ^ bstype_to_code type_of) props) ^ ">"
  | Number => "float"
  | String => "string"
  | Boolean => "Js.boolean"
  | Named s => String.uncapitalize_ascii s
  | Union types => union_types_to_name types
  | Function params rt =>
    String.concat " => " (List.map (fun (name, param_type) => bstype_to_code param_type) params) ^
    " => " ^ bstype_to_code rt
and function_typedefs defs =>
  List.map
    (
      fun (id, t) =>
        switch t {
        | Union types =>
          Some (
            "type " ^
            bstype_name t ^
            " = " ^
            String.concat
              ""
              (
                List.map
                  (
                    fun union_type =>
                      "\n| " ^
                      String.capitalize_ascii (bstype_name union_type) ^
                      " (" ^ bstype_to_code union_type ^ ")"
                  )
                  types
              ) ^ ";\n"
          )
        | _ => None
        }
    )
    defs |>
  List.filter (
    fun
    | Some t => true
    | _ => false
  ) |>
  List.map (
    fun
    | Some t => t
    | None => ""
  ) |>
  String.concat "\n"
and bstype_precode def =>
  switch def {
  | Function params rt => function_typedefs params
  | _ => ""
  };

let rec declaration_to_code module_id =>
  fun
  | VarDecl id type_of =>
    "external " ^
    id ^
    " : " ^ bstype_to_code type_of ^ " = \"\" [@@bs.module \"" ^ Utils.unquote module_id ^ "\"];"
  | FuncDecl id type_of =>
    bstype_precode type_of ^
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
  | TypeDecl id type_of =>
    "type " ^ String.uncapitalize_ascii id ^ " = " ^ bstype_to_code type_of ^ ";"
  | Unknown => "??;";

let stack_to_code =
  fun
  | ModuleDecl id statements =>
    Some (
      Utils.to_module_name id,
      String.concat "\n" (List.map (declaration_to_code id) statements)
    )
  | _ => None;
