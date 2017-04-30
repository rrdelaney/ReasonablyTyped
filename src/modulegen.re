open Ast.Statement.DeclareModule;

open Ast.Statement.Block;

open Ast.Literal;

open Ast.Type;

open Ast.Type.Function;

open Ast.Type.Function.Param;

open Ast.Statement.DeclareExportDeclaration;

open Ast.Statement.DeclareVariable;

open Ast.Statement.DeclareFunction;

module BsType = {
  type t =
    | Null
    | Number
    | Function (list t) t
    | Unknown
    | Unit
    | Any;
};

let string_of_id (loc: Loc.t, id: string) => id;

let rec type_annotation_to_bstype (annotation: option Ast.Type.annotation) =>
  switch annotation {
  | Some (_, (_, t)) => type_to_bstype t
  | None => BsType.Unknown
  }
and type_to_bstype =
  fun
  | Any => BsType.Any
  | Null => BsType.Null
  | Number => BsType.Number
  | Function f => function_type_to_bstype f
  | _ => BsType.Unknown
and function_type_to_bstype {params: (formal, rest), returnType: (_, rt)} => {
  let params =
    if (List.length formal > 0) {
      List.map
        (fun ((_, {typeAnnotation: (_, t)}): Ast.Type.Function.Param.t) => type_to_bstype t) formal
    } else {
      [BsType.Unit]
    };
  let return = type_to_bstype rt;
  BsType.Function params return
};

module BsDecl = {
  type t =
    | VarDecl string BsType.t
    | FuncDecl string BsType.t
    | ModuleDecl string (list t)
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
  | Ast.Statement.DeclareExportDeclaration {declaration: Some declaration} =>
    declaration_to_jsdecl declaration
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
    String.concat " => " (List.map show_type params) ^ " => " ^ show_type return
  | BsType.Null => "null"
  | BsType.Number => "number"
  | BsType.Unknown => "??";

let rec show_decl =
  fun
  | BsDecl.ModuleDecl name decls =>
    "module " ^ name ^ " = {\n  " ^ String.concat "\n  " (List.map show_decl decls) ^ "\n}"
  | BsDecl.Unknown => "external ??"
  | BsDecl.FuncDecl name of_type =>
    "external " ^ name ^ " : " ^ show_type of_type ^ " = \"\" [@@bs.send];"
  | BsDecl.VarDecl name of_type =>
    "external " ^ name ^ " : " ^ show_type of_type ^ " = \"\" [@@bs.val];";
