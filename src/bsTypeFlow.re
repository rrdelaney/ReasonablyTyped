let format_obj_key = key =>
  if (String.contains(key, '-')) {
    "'" ++ key ++ "'";
  } else {
    key;
  };

let rec show_type =
  fun
  | BsTypeAst.Regex => "RegExp"
  | BsTypeAst.Optional(t) => "?" ++ show_type(t)
  | BsTypeAst.Any => "any"
  | BsTypeAst.AnyObject => "Object"
  | BsTypeAst.AnyFunction => "Function"
  | BsTypeAst.Unit => "void"
  | BsTypeAst.Dict(t) => "{ [key: string]: " ++ show_type(t) ++ " }"
  | BsTypeAst.Tuple(types) =>
    "[" ++ (List.map(show_type, types) |> String.concat(", ")) ++ "]"
  | BsTypeAst.Array(t) => show_type(t) ++ "[]"
  | BsTypeAst.Typeof(t) => "typeof " ++ show_type(t)
  | BsTypeAst.Function({typeParams, formalParams, restParam, returnType}) => {
      let paramList =
        List.map(
          ((name, type_of)) =>
            switch type_of {
            | BsTypeAst.Unit => ""
            | BsTypeAst.Optional(t) => name ++ "?: " ++ show_type(t)
            | _ => name ++ ": " ++ show_type(type_of)
            },
          formalParams
        );
      (List.length(typeParams) > 0 ? "<" : "")
      ++ String.concat(", ", typeParams)
      ++ (List.length(typeParams) > 0 ? ">" : "")
      ++ "("
      ++ String.concat(", ", paramList)
      ++ (
        switch restParam {
        | Some((name, type_of)) =>
          (List.length(paramList) > 0 ? ", " : "")
          ++ "..."
          ++ name
          ++ ": "
          ++ show_type(type_of)
        | _ => ""
        }
      )
      ++ "): "
      ++ show_type(returnType);
    }
  | BsTypeAst.Null => "null"
  | BsTypeAst.Number => "number"
  | BsTypeAst.Boolean => "boolean"
  | BsTypeAst.String => "string"
  | BsTypeAst.Union(types) => String.concat(" | ", List.map(show_type, types))
  | BsTypeAst.Object(props) =>
    "{ "
    ++ String.concat(
         ", ",
         List.map(
           ((key, prop, optional)) =>
             if (key == "$$callProperty") {
               show_type(prop);
             } else {
               format_obj_key(key)
               ++ (optional ? "?" : "")
               ++ (
                 switch prop {
                 | BsTypeAst.Function(_) => ""
                 | _ => ": "
                 }
               )
               ++ show_type(prop);
             },
           props
         )
       )
    ++ " }"
  | BsTypeAst.Class(extends, props) =>
    (
      switch extends {
      | None => ""
      | Some(parent) => " extends " ++ show_type(parent)
      }
    )
    ++ "{ "
    ++ String.concat(
         "; ",
         List.map(((key, prop)) => key ++ ": " ++ show_type(prop), props)
       )
    ++ " }"
  | BsTypeAst.Named(type_params, s, _) =>
    s
    ++ (
      if (List.length(type_params) > 0) {
        "<" ++ (List.map(show_type, type_params) |> String.concat(", ")) ++ ">";
      } else {
        "";
      }
    )
  | BsTypeAst.Promise(t) => "Promise<" ++ show_type(t) ++ ">"
  | BsTypeAst.StringLiteral(t) => "\"" ++ t ++ "\""
  | BsTypeAst.Date => "Date";

let rec show_decl =
  fun
  | BsTypeAst.Noop => ""
  | BsTypeAst.Ignore(s) => "ignored: " ++ s
  | BsTypeAst.ExportsDecl(of_type) =>
    "declare module.exports: " ++ show_type(of_type)
  | BsTypeAst.ModuleDecl(name, decls) =>
    "declare module "
    ++ name
    ++ " {\n  "
    ++ String.concat("\n  ", List.map(show_decl, decls))
    ++ "\n}"
  | BsTypeAst.TypeDecl(id, _type_params, of_type) =>
    "declare type " ++ id ++ " = " ++ show_type(of_type)
  | BsTypeAst.FuncDecl(name, of_type) =>
    "declare export function " ++ name ++ show_type(of_type)
  | BsTypeAst.VarDecl(name, of_type) =>
    "declare export var " ++ name ++ ": " ++ show_type(of_type)
  | BsTypeAst.ClassDecl(name, type_params, of_type) =>
    "declare class "
    ++ name
    ++ " "
    ++ (List.length(type_params) > 0 ? "<" : "")
    ++ String.concat(", ", type_params)
    ++ (List.length(type_params) > 0 ? ">" : "")
    ++ show_type(of_type)
  | BsTypeAst.InterfaceDecl(name, _type_params, of_type) =>
    "declare interface " ++ name ++ " " ++ show_type(of_type)
  | BsTypeAst.ImportDecl(import_names, module_name) =>
    "import type { "
    ++ String.concat(
         ", ",
         List.map(
           ((remote, local)) =>
             if (remote == local) {
               remote;
             } else {
               remote ++ " as " ++ local;
             },
           import_names
         )
       )
    ++ " } from '"
    ++ module_name
    ++ "'\n"
  | BsTypeAst.ReactClass(className, of_type) =>
    "react class " ++ className ++ " with prop type " ++ show_type(of_type);
