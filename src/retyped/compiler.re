module Stage = {
  open Parser_flow;
  let parse_source name source => {
    let (ocaml_ast, errors) =
      Parser_flow.program_file source (Some (Loc.SourceFile name));
    let (_, statements, _) = ocaml_ast;
    statements
  };
  let make_typetable =
    fun
    | Modulegen.BsDecl.ModuleDecl _ statements => Typetable.create statements
    | _ => [];
  let optimize_program program =>
    Optimizer.optimize (make_typetable program) program;
  let render_program program =>
    Codegen.program_to_code program (make_typetable program);
  let combine_programs =
    List.fold_left
      (
        fun (current_id, all_code) result =>
          switch result {
          | Some (program_id, program_code) when program_id !== "" => (
              program_id,
              all_code ^ "\n" ^ program_code
            )
          | Some (program_id, program_code) => (
              current_id,
              all_code ^ "\n" ^ program_code
            )
          | None => (current_id, all_code)
          }
      )
      ("Unknown ID", "");
};

let compile module_name module_def => {
  let statements = Stage.parse_source module_name module_def;
  let programs = List.map Modulegen.statement_to_program statements;
  let flow_code = String.concat "\n" (List.map Flowprinter.show_decl programs);
  let linked_programs = Imports.link programs;
  let optimized_programs = List.map Stage.optimize_program linked_programs;
  let rendered_programs = List.map Stage.render_program optimized_programs;
  let (module_id, bs_code) = Stage.combine_programs rendered_programs;
  (module_id, flow_code, bs_code)
};
