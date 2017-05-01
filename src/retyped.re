open Parser_flow;

let compile module_name module_def => {
  let (_loc, statements, errors) = {
    let (ocaml_ast, errors) = Parser_flow.program_file module_def (Some module_name);
    ocaml_ast
  };
  let stacks = List.map Modulegen.statement_to_stack statements;
  let main_stack = List.hd stacks;
  let flow_code = Modulegen.show_decl main_stack;
  let (module_id, bs_code) =
    switch (Codegen.stack_to_code main_stack) {
    | Some m => m
    | None => ("??", "??")
    };
  (module_id, flow_code, bs_code)
};
