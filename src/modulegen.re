open Ast.Statement.DeclareModule;

open Ast.Statement.Block;

open Ast.Literal;

open Ast.Type;

open Ast.Type.Function;

open Ast.Type.Function.Param;

open Ast.Type.Object;

open Ast.Type.Object.Property;

open Ast.Expression.Object.Property;

open Ast.Statement.DeclareExportDeclaration;

open Ast.Statement.DeclareVariable;

open Ast.Statement.DeclareFunction;

open Ast.Statement.TypeAlias;

module BsType = {
  type t =
    | Null
    | Number
    | String
    | Function (list (string, t)) t
    | Object (list (string, t))
    | Union (list t)
    | Unknown
    | Boolean
    | Unit
    | Any;
};

let string_of_id (loc: Loc.t, id: string) => id;

let string_of_key (key: Ast.Expression.Object.Property.key) =>
  switch key {
  | Identifier id => string_of_id id
  | _ => "??"
  };

let rec type_annotation_to_bstype (annotation: option Ast.Type.annotation) =>
  switch annotation {
  | Some (_, (_, t)) => type_to_bstype t
  | None => BsType.Unknown
  }
and type_to_bstype =
  fun
  | Mixed => BsType.Any
  | Any => BsType.Any
  | Null => BsType.Null
  | Number => BsType.Number
  | String => BsType.String
  | Boolean => BsType.Boolean
  | Function f => function_type_to_bstype f
  | Object o => object_type_to_bstype o
  | Union (_, first) (_, second) rest =>
    BsType.Union [
      type_to_bstype first,
      type_to_bstype second,
      ...List.map (fun (loc, t) => type_to_bstype t) rest
    ]
  | _ => BsType.Unknown
and function_type_to_bstype {params: (formal, rest), returnType: (_, rt)} => {
  let params =
    if (List.length formal > 0) {
      List.map
        (
          fun ((_, {typeAnnotation: (_, t), name}): Ast.Type.Function.Param.t) => (
            switch name {
            | Some id => string_of_id id
            | None => ""
            },
            type_to_bstype t
          )
        )
        formal
    } else {
      [("", BsType.Unit)]
    };
  let return = type_to_bstype rt;
  BsType.Function params return
}
and value_to_bstype (value: Ast.Type.Object.Property.value) =>
  switch value {
  | Init (loc, t) => type_to_bstype t
  | Get (loc, func) => function_type_to_bstype func
  | Set (loc, func) => function_type_to_bstype func
  }
and object_type_to_bstype {properties} =>
  BsType.Object (
    List.map
      (
        fun
        | Property (loc, {key, value}) => (string_of_key key, value_to_bstype value)
        | _ => ("??", BsType.Unknown)
      )
      properties
  );

module BsDecl = {
  type t =
    | VarDecl string BsType.t
    | FuncDecl string BsType.t
    | ModuleDecl string (list t)
    | ExportsDecl BsType.t
    | TypeDecl string BsType.t
    | Unknown;
};

let declaration_to_jsdecl =
  fun
  | Variable (loc, {id, typeAnnotation}) =>
    BsDecl.VarDecl (string_of_id id) (type_annotation_to_bstype typeAnnotation)
  | Function (loc, {id, typeAnnotation}) =>
    BsDecl.FuncDecl (string_of_id id) (type_annotation_to_bstype (Some typeAnnotation))
  | _ => BsDecl.Unknown;

let rec statement_to_stack (loc, s) =>
  switch s {
  | Ast.Statement.DeclareModuleExports annotation =>
    BsDecl.ExportsDecl (type_annotation_to_bstype (Some annotation))
  | Ast.Statement.DeclareExportDeclaration {declaration: Some declaration} =>
    declaration_to_jsdecl declaration
  | Ast.Statement.DeclareFunction declare_function =>
    declaration_to_jsdecl (Function (loc, declare_function))
  | Ast.Statement.TypeAlias {id, right: (loc, t)} =>
    BsDecl.TypeDecl (string_of_id id) (type_to_bstype t)
  | Ast.Statement.DeclareModule s => declare_module_to_jsdecl s
  | _ => BsDecl.Unknown
  }
and block_to_stack (loc, {body}) => List.map statement_to_stack body
and declare_module_to_jsdecl {id, body} =>
  switch id {
  | Literal (loc, {raw}) => BsDecl.ModuleDecl raw (block_to_stack body)
  | _ => BsDecl.Unknown
  };

let rec show_type =
  fun
  | BsType.Any => "any"
  | BsType.Unit => "unit"
  | BsType.Function params return =>
    "(" ^
    String.concat
      ", "
      (
        List.map
          (
            fun (name, type_of) =>
              switch type_of {
              | BsType.Unit => ""
              | _ => name ^ ": " ^ show_type type_of
              }
          )
          params
      ) ^
    ") => " ^ show_type return
  | BsType.Null => "null"
  | BsType.Number => "number"
  | BsType.Boolean => "boolean"
  | BsType.String => "string"
  | BsType.Union types => String.concat " | " (List.map show_type types)
  | BsType.Object props =>
    "{ " ^
    String.concat ", " (List.map (fun (key, prop) => key ^ ": " ^ show_type prop) props) ^ " }"
  | BsType.Unknown => "??";

let rec show_decl =
  fun
  | BsDecl.ExportsDecl of_type => "declare module.exports: " ^ show_type of_type
  | BsDecl.ModuleDecl name decls =>
    "declare module " ^ name ^ " {\n  " ^ String.concat "\n  " (List.map show_decl decls) ^ "\n}"
  | BsDecl.TypeDecl id of_type => "declare type " ^ id ^ " = " ^ show_type of_type
  | BsDecl.Unknown => "external ??"
  | BsDecl.FuncDecl name of_type => "declare export function " ^ name ^ show_type of_type
  | BsDecl.VarDecl name of_type => "declare export var " ^ name ^ ": " ^ show_type of_type;
