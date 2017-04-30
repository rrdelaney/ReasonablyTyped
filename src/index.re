open Parser_flow;

open Ast.Statement.DeclareModule;

open Ast.Statement.Block;

open Ast.Literal;

open Ast.Type;

open Ast.Statement.DeclareExportDeclaration;

open Ast.Statement.DeclareVariable;

open Ast.Statement.DeclareFunction;

let module_name = Loc.SourceFile Sys.argv.(1);

let module_def = Sys.argv.(2);

let (ocaml_ast, errors) = Parser_flow.program_file module_def (Some module_name);

let (locs, statements, comments) = ocaml_ast;

let string_of_id (loc: Loc.t, id: string) => id;

let string_of_type (annotation: option Ast.Type.annotation) =>
  switch annotation {
  | Some (_, (_, Any)) => "any"
  | Some (_, (_, Null)) => "null"
  | Some (_, (_, Number)) => "number"
  | Some (_, (_, Function f)) => "function"
  | Some _ => "??"
  | None => "any"
  };

let string_of_declaration =
  fun
  | Variable (loc, {id, typeAnnotation}) =>
    "variable " ^ string_of_id id ^ " as " ^ string_of_type typeAnnotation
  | Function (loc, {id, typeAnnotation}) =>
    "function " ^ string_of_id id ^ " as " ^ string_of_type (Some typeAnnotation)
  | _ => "??";

let string_of_declare_export_declaration =
  fun
  | {declaration: Some declaration} => "export " ^ string_of_declaration declaration
  | _ => "declare ??";

let rec statement_to_stack (loc, s) =>
  switch s {
  | Ast.Statement.DeclareExportDeclaration d => string_of_declare_export_declaration d
  | Ast.Statement.DeclareModule s => string_of_declare_module s
  | _ => "??"
  }
and block_to_stack (loc, {body}) => String.concat "; " (List.map statement_to_stack body)
and string_of_declare_module {id, body} =>
  switch id {
  | Literal (loc, {raw}) => "module name = " ^ raw ^ "; " ^ block_to_stack body
  | _ => "??"
  };

let sss = List.map statement_to_stack statements;

List.iter print_endline sss;
