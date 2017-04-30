open Parser_flow;

let (_loc, statements, errors) = {
  let module_name = Loc.SourceFile Sys.argv.(1);
  let module_def = Sys.argv.(2);
  let (ocaml_ast, errors) = Parser_flow.program_file module_def (Some module_name);
  ocaml_ast
};

let sss = List.map Module_gen.statement_to_stack statements;

List.iter (fun d => print_endline (Module_gen.show_decl d)) sss;
