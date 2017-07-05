open Parser_flow;

let combine_stacks =
  List.fold_left
    (
      fun (current_id, all_code) stack =>
        switch (Codegen.stack_to_code stack) {
        | Some (stack_id, stack_code) =>
          if (stack_id !== "") {
            (stack_id, all_code ^ "\n" ^ stack_code)
          } else {
            (current_id, all_code ^ "\n" ^ stack_code)
          }
        | None => (current_id, all_code)
        }
    )
    ("Unknown ID", "");

let compile module_name module_def => {
  let (statements, errors) = {
    let (ocaml_ast, errors) =
      Parser_flow.program_file module_def (Some (Loc.SourceFile module_name));
    let (_, statements, _) = ocaml_ast;
    (statements, errors)
  };
  let stacks = List.map Modulegen.statement_to_stack statements;
  let flow_code = String.concat "\n" (List.map Flowprinter.show_decl stacks);
  let (module_id, bs_code) = combine_stacks stacks;
  (module_id, flow_code, bs_code)
};
