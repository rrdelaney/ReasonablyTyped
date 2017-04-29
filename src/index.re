open Parser_flow;

open Ast.Statement.VariableDeclaration;

open Ast.Statement.DeclareModule;

let module_name = Loc.SourceFile Sys.argv.(1);

let module_def = Sys.argv.(2);

let (ocaml_ast, errors) = Parser_flow.program_file module_def (Some module_name);

let (locs, statements, comments) = ocaml_ast;

let sss =
  List.map
    (
      fun (loc, s) =>
        switch s {
        | Ast.Statement.Block b => "block"
        | Ast.Statement.DeclareExportDeclaration d => "declare"
        | Ast.Statement.VariableDeclaration {kind: Ast.Statement.VariableDeclaration.Const} => "const"
        | Ast.Statement.VariableDeclaration {kind: Ast.Statement.VariableDeclaration.Var} => "var"
        | _ => "??"
        }
    )
    statements;

List.iter print_endline sss;
