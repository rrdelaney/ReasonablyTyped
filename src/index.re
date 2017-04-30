open Parser_flow;

let (_loc, statements, errors) = {
  let module_name = Loc.SourceFile Sys.argv.(1);
  let module_def = Sys.argv.(2);
  let (ocaml_ast, errors) = Parser_flow.program_file module_def (Some module_name);
  ocaml_ast
};

let stacks = List.map Modulegen.statement_to_stack statements;

let modules = List.map Codegen.stack_to_code stacks;

List.iter print_endline modules;
