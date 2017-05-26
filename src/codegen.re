open Modulegen.BsDecl;

open Modulegen.BsType;

exception CodegenTypeError string;

exception CodegenConstructorError string;

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
  let rec uniq =
    fun
    | [] => []
    | [h, ...t] => {
        let no_dups = uniq (List.filter (fun x => x != h) t);
        [h, ...no_dups]
      };
  let is_optional (_, type_of) =>
    switch type_of {
    | Optional _ => true
    | _ => false
    };
};

let rec bstype_name =
  fun
  | Regex => "regex"
  | Unit => "unit"
  | Null => "null"
  | Any => "any"
  | Object _ => "object"
  | Number => "number"
  | String => "string"
  | Boolean => "bool"
  | Function _ => "func"
  | Unknown => "unknown"
  | Array t => "array_" ^ bstype_name t
  | Tuple types => "tuple_of_" ^ (List.map bstype_name types |> String.concat "_")
  | Named s => String.uncapitalize_ascii s
  | Union types => union_types_to_name types
  | Class props => raise (CodegenTypeError "Unable to translate class into type name")
  | Optional t => ""
and union_types_to_name types => {
  let type_names = List.map bstype_name types;
  String.concat "_or_" type_names
};

let rec bstype_to_code =
  fun
  | Regex => "Js.Re.t"
  | Optional t => bstype_to_code t ^ "?"
  | Unit => "unit"
  | Null => "null"
  | Array t => "array " ^ bstype_to_code t
  | Tuple types => "(" ^ (List.map bstype_to_code types |> String.concat ", ") ^ ")"
  | Unknown => "??"
  | Any => "_"
  | Object props =>
    "Js.t {. " ^
    String.concat ", " (List.map (fun (key, type_of) => key ^ ": " ^ bstype_to_code type_of) props) ^ " }"
  | Number => "float"
  | String => "string"
  | Boolean => "Js.boolean"
  | Named s => String.uncapitalize_ascii s
  | Union types => union_types_to_name types
  | Function params rt =>
    String.concat
      " => " (List.map (fun (name, param_type) => name ^ "::" ^ bstype_to_code param_type) params) ^
    " => " ^ (List.exists Utils.is_optional params ? "unit => " : "") ^ bstype_to_code rt
  | Class props =>
    "Js.t {. " ^
    String.concat
      ", "
      (
        List.filter (fun (key, type_of) => key != "constructor") props |>
        List.map (fun (key, type_of) => key ^ ": " ^ bstype_to_code type_of ^ " [@bs.meth]")
      ) ^ " }";

module Precode = {
  let rec bstype_precode def =>
    switch def {
    | Union types => [string_of_union_types def types]
    | Function params rt => List.map (fun (id, t) => bstype_precode t) params |> List.flatten
    | Object types => List.map (fun (id, type_of) => bstype_precode type_of) types |> List.flatten
    | Class types => List.map (fun (id, type_of) => bstype_precode type_of) types |> List.flatten
    | Optional t => bstype_precode t
    | Array t => bstype_precode t
    | _ => [""]
    }
  and string_of_union_types t types =>
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
      ) ^ ";\n";
  let decl_to_precode =
    fun
    | VarDecl _ type_of => bstype_precode type_of
    | FuncDecl _ type_of => bstype_precode type_of
    | TypeDecl id type_of =>
      bstype_precode type_of |>
      List.cons ("type " ^ String.uncapitalize_ascii id ^ " = " ^ bstype_to_code type_of ^ ";")
    | ClassDecl _ type_of => bstype_precode type_of
    | ExportsDecl type_of => bstype_precode type_of
    | _ => [""];
  let from_stack stack =>
    switch stack {
    | ModuleDecl id statements =>
      List.map decl_to_precode statements |> List.flatten |> Utils.uniq |> String.concat "\n"
    | TypeDecl _ type_of => ""
    | _ => ""
    };
};

let constructor_type =
  fun
  | Class props => {
      let (_, cons_type) = List.find (fun (id, _) => id == "constructor") props;
      bstype_to_code cons_type
    }
  | _ => raise (CodegenConstructorError "Type has no constructor");

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
  | TypeDecl id type_of => ""
  | ClassDecl id type_of =>
    "type " ^
    String.uncapitalize_ascii id ^
    " = " ^
    bstype_to_code type_of ^
    ";\n" ^
    "external create_" ^
    String.uncapitalize_ascii id ^
    ": " ^
    constructor_type type_of ^
    " = \"" ^ id ^ "\" [@@bs.new] [@@bs.module \"" ^ Utils.unquote module_id ^ "\"];"
  | Unknown => "??;";

let stack_to_code stack =>
  switch stack {
  | ModuleDecl id statements =>
    Some (
      Utils.to_module_name id,
      Precode.from_stack stack ^ String.concat "\n" (List.map (declaration_to_code id) statements)
    )
  | TypeDecl _ _ => Some ("", Precode.from_stack stack ^ declaration_to_code "" stack)
  | _ => None
  };
