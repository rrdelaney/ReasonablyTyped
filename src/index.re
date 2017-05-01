open Parser_flow;

let (_loc, statements, errors) = {
  let module_name = Loc.SourceFile Sys.argv.(1);
  let module_def = Sys.argv.(2);
  let (ocaml_ast, errors) = Parser_flow.program_file module_def (Some module_name);
  ocaml_ast
};

let stacks = List.map Modulegen.statement_to_stack statements;

print_endline "(* Flow Definition *)";

List.iter (fun m => print_endline (Modulegen.show_decl m)) stacks;

let (module_id, module_code) = {
  let result = stacks |> List.map Codegen.stack_to_code |> List.hd;
  switch result {
  | Some m => m
  | None => ("??", "??")
  }
};

print_newline ();

print_endline "(* Bucklescript Definition *)";

print_endline ("Module: " ^ module_id);

print_endline module_code;
