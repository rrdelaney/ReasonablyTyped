open Parser_flow;

open Ast.Statement.VariableDeclaration;

open Ast.Statement.DeclareModule;

open Ast.Statement.Block;

open Ast.Literal;

let module_name = Loc.SourceFile Sys.argv.(1);

let module_def = Sys.argv.(2);

let (ocaml_ast, errors) = Parser_flow.program_file module_def (Some module_name);

let (locs, statements, comments) = ocaml_ast;

let rec statement_to_stack (loc, s) =>
  switch s {
  | Ast.Statement.Block b => "block"
  | Ast.Statement.DeclareExportDeclaration d => "declare"
  | Ast.Statement.VariableDeclaration {kind: Ast.Statement.VariableDeclaration.Const} => "const"
  | Ast.Statement.VariableDeclaration {kind: Ast.Statement.VariableDeclaration.Var} => "var"
  | Ast.Statement.DeclareModule {id, body} =>
    switch id {
    | Literal (loc, {raw}) => "module name = " ^ raw ^ "; " ^ block_to_stack body
    | _ => "??"
    }
  | _ => "??"
  }
and block_to_stack (loc, {body}) => String.concat "; " (List.map statement_to_stack body);

let sss = List.map statement_to_stack statements;

List.iter print_endline sss;
