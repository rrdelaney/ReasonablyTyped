open Ast.Statement.Block;

open Ast.Literal;

open Ast.Type.StringLiteral;

open Ast.Type;

open Ast.Type.Generic;

open Ast.Type.Generic.Identifier;

open Ast.Type.Object;

open Ast.Type.Object.Property;

open Ast.Type.Object.Indexer;

open Ast.Expression.Object.Property;

open Ast.Statement.DeclareExportDeclaration;

open Ast.Statement.DeclareVariable;

open Ast.Statement.DeclareFunction;

open Ast.Statement.TypeAlias;

open Loc;

type context = {loc: Loc.t, is_params: bool};

let intctx = {loc: Loc.none, is_params: false};

exception ModulegenDeclError string;

exception ModulegenTypeError string;

exception ModulegenStatementError string;

let loc_to_msg ({source, start, _end}: Loc.t) =>
  (
    switch source {
    | Some fname => " [in " ^ Loc.string_of_filename fname ^ " "
    | None => " ["
    }
  ) ^
  "from " ^
  string_of_int start.line ^
  ":" ^
  string_of_int start.column ^
  " to " ^ string_of_int _end.line ^ ":" ^ string_of_int _end.column ^ "]";

let not_supported interface (context: context) =>
  interface ^ " is not currently supported" ^ loc_to_msg context.loc;

let sanity_check problem (context: context) =>
  problem ^ " should not happen" ^ loc_to_msg context.loc;

module BsType = {
  type t =
    | Null
    | Number
    | Regex
    | String
    | Function (list (string, t)) t
    | AnyFunction
    | Object (list (string, t))
    | AnyObject
    | Class (list (string, t))
    | Union (list t)
    | Array t
    | Dict t
    | Boolean
    | Tuple (list t)
    | Unit
    | Any
    | Typeof t
    | Named string
    | Optional t
    | StringLiteral string;
};

let string_of_id (loc: Loc.t, id: string) => id;

let string_of_key (key: Ast.Expression.Object.Property.key) =>
  switch key {
  | Identifier id => string_of_id id
  | Literal (loc, {value}) =>
    switch value {
    | String s => s
    | _ =>
      raise (ModulegenTypeError (sanity_check "Non-string as object property" {...intctx, loc}))
    }
  | Computed (loc, _) =>
    raise (ModulegenTypeError (not_supported "Computed object properties" {...intctx, loc}))
  };

let rec type_annotation_to_bstype (annotation: option Ast.Type.annotation) =>
  switch annotation {
  | Some (loc, (_, t)) => type_to_bstype {...intctx, loc} t
  | None => raise (ModulegenTypeError "Unknown type when parsing annotation")
  }
and type_to_bstype (ctx: context) =>
  fun
  | Void => BsType.Unit
  | Mixed => BsType.Any
  | Any => BsType.Any
  | Null => BsType.Null
  | Number => BsType.Number
  | String => BsType.String
  | Boolean => BsType.Boolean
  | Function f => function_type_to_bstype ctx f
  | Object o =>
    if (List.length o.properties == 0) {
      BsType.Object []
    } else {
      let first_prop = List.hd o.properties;
      switch first_prop {
      | Indexer (_, {value}) =>
        let (_, value_type) = value;
        BsType.Dict (type_to_bstype ctx value_type)
      | _ => BsType.Object (object_type_to_bstype o)
      }
    }
  | Array (loc, t) => BsType.Array (type_to_bstype {...ctx, loc} t)
  | Tuple types => BsType.Tuple (List.map (fun (loc, t) => type_to_bstype {...ctx, loc} t) types)
  | Union (loc_a, first) (loc_b, second) rest =>
    BsType.Union [
      type_to_bstype {...ctx, loc: loc_a} first,
      type_to_bstype {...ctx, loc: loc_b} second,
      ...List.map (fun (_, t) => type_to_bstype ctx t) rest
    ]
  | Generic {id} =>
    switch id {
    | Qualified (_, q) => BsType.Named (string_of_id q.id)
    | Unqualified q =>
      switch q {
      | (_, "RegExp") => BsType.Regex
      | (_, "Object") => BsType.AnyObject
      | (_, "Function") => BsType.AnyFunction
      | (loc, "Class") => raise (ModulegenTypeError (not_supported "Class types" {...ctx, loc}))
      | _ => BsType.Named (string_of_id q)
      }
    }
  | StringLiteral {value} => raise (ModulegenTypeError (not_supported "StringLiteral" ctx)) /* BsType.StringLiteral value */
  | NumberLiteral _ => raise (ModulegenTypeError (not_supported "NumberLiteral" ctx))
  | BooleanLiteral _ => raise (ModulegenTypeError (not_supported "BooleanLiteral" ctx))
  | Typeof (loc, t) => BsType.Typeof (type_to_bstype {...ctx, loc} t)
  | _ =>
    raise (
      ModulegenTypeError ("Unknown type when converting to Bucklescript type" ^ loc_to_msg ctx.loc)
    )
and function_type_to_bstype ctx f => {
  open Ast.Type.Function;
  open Ast.Type.Function.Param;
  let {params: (formal, rest), returnType: (rt_loc, rt), typeParameters} = f;
  switch typeParameters {
  | Some _ => raise (ModulegenTypeError (not_supported "Type parameters" ctx))
  | None => ()
  };
  let params =
    if (List.length formal > 0) {
      List.map
        (
          fun ((_, {typeAnnotation: (loc, t), name, optional}): Ast.Type.Function.Param.t) => (
            switch name {
            | Some id => string_of_id id
            | None => ""
            },
            if optional {
              BsType.Optional (type_to_bstype {...ctx, loc} t)
            } else {
              type_to_bstype {...ctx, loc} t
            }
          )
        )
        formal
    } else {
      [("", BsType.Unit)]
    };
  let return = type_to_bstype {...ctx, loc: rt_loc} rt;
  BsType.Function params return
}
and value_to_bstype (value: Ast.Type.Object.Property.value) =>
  switch value {
  | Init (loc, t) => type_to_bstype {...intctx, loc} t
  | Get (loc, func) => function_type_to_bstype {...intctx, loc} func
  | Set (loc, func) => function_type_to_bstype {...intctx, loc} func
  }
and object_type_to_bstype {properties} =>
  List.map
    (
      fun
      | Property (loc, {key, value}) => (string_of_key key, value_to_bstype value)
      | CallProperty (_loc, props) => {
          open Ast.Type;
          open Ast.Type.Object.CallProperty;
          let {value: (loc, value), static} = props;
          if static {
            raise (
              ModulegenTypeError (
                not_supported "static CallProperty on Object types" {...intctx, loc}
              )
            )
          };
          ("$$callProperty", type_to_bstype {...intctx, loc} (Function value))
        }
      | Indexer (loc, _) =>
        raise (ModulegenTypeError (not_supported "Indexer on Object types" {...intctx, loc}))
      | SpreadProperty (loc, _) =>
        raise (
          ModulegenTypeError (not_supported "SpreadProperty on Object types" {...intctx, loc})
        )
    )
    properties;

module BsDecl = {
  type t =
    | VarDecl string BsType.t
    | FuncDecl string BsType.t
    | ModuleDecl string (list t)
    | ExportsDecl BsType.t
    | TypeDecl string BsType.t
    | ClassDecl string BsType.t
    | InterfaceDecl string BsType.t;
};

let declaration_to_jsdecl loc =>
  Ast.Statement.Interface.(
    fun
    | Variable (loc, {id, typeAnnotation}) =>
      BsDecl.VarDecl (string_of_id id) (type_annotation_to_bstype typeAnnotation)
    | Function (loc, {id, typeAnnotation}) =>
      BsDecl.FuncDecl (string_of_id id) (type_annotation_to_bstype (Some typeAnnotation))
    | Class (loc, {id, body: (_, interface)}) =>
      BsDecl.ClassDecl (string_of_id id) (BsType.Class (object_type_to_bstype interface))
    | _ =>
      raise (
        ModulegenDeclError (
          "Unknown declaration when converting a module property declaration" ^ loc_to_msg loc
        )
      )
  );

let rec statement_to_stack (loc, s) =>
  Ast.Statement.Interface.(
    switch s {
    | Ast.Statement.DeclareModuleExports annotation =>
      BsDecl.ExportsDecl (type_annotation_to_bstype (Some annotation))
    | Ast.Statement.DeclareExportDeclaration {declaration: Some declaration} =>
      declaration_to_jsdecl loc declaration
    | Ast.Statement.DeclareFunction declare_function =>
      declaration_to_jsdecl loc (Function (loc, declare_function))
    | Ast.Statement.DeclareClass {id, body: (_, interface)} =>
      BsDecl.ClassDecl (string_of_id id) (BsType.Class (object_type_to_bstype interface))
    | Ast.Statement.TypeAlias {id, right: (loc, t)} =>
      BsDecl.TypeDecl (string_of_id id) (type_to_bstype {...intctx, loc} t)
    | Ast.Statement.DeclareModule s => declare_module_to_jsdecl loc s
    | Ast.Statement.DeclareVariable {id, typeAnnotation} =>
      if (string_of_id id == "exports") {
        BsDecl.ExportsDecl (type_annotation_to_bstype typeAnnotation)
      } else {
        BsDecl.VarDecl (string_of_id id) (type_annotation_to_bstype typeAnnotation)
      }
    | Ast.Statement.Debugger =>
      raise (ModulegenStatementError (not_supported "Debugger statments" {...intctx, loc}))
    | Ast.Statement.InterfaceDeclaration s => declare_interface_to_jsdecl loc s
    | _ =>
      raise (
        ModulegenStatementError ("Unknown statement type when parsing libdef" ^ loc_to_msg loc)
      )
    }
  )
and block_to_stack (loc, {body}) => List.map statement_to_stack body
and declare_module_to_jsdecl loc s => {
  open Ast.Statement.DeclareModule;
  let {id, body} = s;
  switch id {
  | Literal (loc, {raw}) => BsDecl.ModuleDecl raw (block_to_stack body)
  | _ =>
    raise (
      ModulegenDeclError (
        "Unknown declaration type when converting a module declaration" ^ loc_to_msg loc
      )
    )
  }
}
and declare_interface_to_jsdecl loc s => {
  open Ast.Statement.Interface;
  open Ast.Type;
  let {id, body, typeParameters, extends} = s;
  switch (typeParameters, extends) {
  | (Some _tp, _extends) =>
    raise (ModulegenStatementError (not_supported "Generic Intefaces" {...intctx, loc}))
  | (_tp, [(loc, _extends), ...t]) =>
    raise (ModulegenStatementError (not_supported "Inheriting in interfaces" {...intctx, loc}))
  | _ => ()
  };
  let (body_loc, obj_type) = body;
  let body_type = Object obj_type;
  BsDecl.InterfaceDecl (string_of_id id) (type_to_bstype {...intctx, loc: body_loc} body_type)
};

module Printer = {
  let format_obj_key key =>
    if (String.contains key '-') {
      "'" ^ key ^ "'"
    } else {
      key
    };
  let rec show_type =
    fun
    | BsType.Regex => "RegExp"
    | BsType.Optional t => show_type t ^ "?"
    | BsType.Any => "any"
    | BsType.AnyObject => "Object"
    | BsType.AnyFunction => "Function"
    | BsType.Unit => "unit"
    | BsType.Dict t => "{ [key: string]: " ^ show_type t ^ " }"
    | BsType.Tuple types => "[" ^ (List.map show_type types |> String.concat ", ") ^ "]"
    | BsType.Array t => show_type t ^ "[]"
    | BsType.Typeof t => "typeof " ^ show_type t
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
                | BsType.Optional t => name ^ "?: " ^ show_type t
                | _ => name ^ ": " ^ show_type type_of
                }
            )
            params
        ) ^
      "): " ^ show_type return
    | BsType.Null => "null"
    | BsType.Number => "number"
    | BsType.Boolean => "boolean"
    | BsType.String => "string"
    | BsType.Union types => String.concat " | " (List.map show_type types)
    | BsType.Object props =>
      "{ " ^
      String.concat
        ", " (List.map (fun (key, prop) => format_obj_key key ^ ": " ^ show_type prop) props) ^ " }"
    | BsType.Class props =>
      "{ " ^
      String.concat "; " (List.map (fun (key, prop) => key ^ ": " ^ show_type prop) props) ^ " }"
    | BsType.Named s => s
    | BsType.StringLiteral t => "\"" ^ t ^ "\"";
  let rec show_decl =
    fun
    | BsDecl.ExportsDecl of_type => "declare module.exports: " ^ show_type of_type
    | BsDecl.ModuleDecl name decls =>
      "declare module " ^ name ^ " {\n  " ^ String.concat "\n  " (List.map show_decl decls) ^ "\n}"
    | BsDecl.TypeDecl id of_type => "declare type " ^ id ^ " = " ^ show_type of_type
    | BsDecl.FuncDecl name of_type => "declare export function " ^ name ^ show_type of_type
    | BsDecl.VarDecl name of_type => "declare export var " ^ name ^ ": " ^ show_type of_type
    | BsDecl.ClassDecl name of_type => "declare class " ^ name ^ " " ^ show_type of_type
    | BsDecl.InterfaceDecl name of_type => "declare interface " ^ name ^ " " ^ show_type of_type;
};
