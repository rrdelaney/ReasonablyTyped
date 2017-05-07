open Parser_flow;

let compile module_name module_def => {
  print_endline module_def;
  let (statements, errors) = {
    let (ocaml_ast, errors) = Parser_flow.program_file module_def (Some module_name);
    let (_, statements, _) = ocaml_ast;
    (statements, errors)
  };
  /*print_int (List.length statements);*/
  let stacks = List.map Modulegen.statement_to_stack statements;
  /*print_int (List.length stacks);*/
  /*print_newline ();*/
  let main_stack = List.hd stacks;
  let flow_code = Modulegen.show_decl main_stack;
  let (module_id, bs_code) =
    switch (Codegen.stack_to_code main_stack) {
    | Some m => m
    | None => ("??", "??")
    };
  (module_id, flow_code, bs_code)
  /*(">>", ">>", ">>")*/
};
