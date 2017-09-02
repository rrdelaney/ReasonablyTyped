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
  module Debug = {
    open Modulegen.BsDecl;
    let show_imports = Imports.show_imports;
    let show_types =
      List.iter (
        fun
        | ModuleDecl name statements as md =>
          Typetable.show (make_typetable md)
        | _ => ()
      );
    let show_flow programs => {
      let flow_code =
        List.map Flowprinter.show_decl programs |> String.concat "\n";
      print_endline "=== Flow Definition ===";
      print_endline flow_code;
      print_newline ()
    };
  };
};

let compile ::debug=false module_name module_def => {
  if debug {
    let debug_programs =
      Stage.parse_source module_name module_def |>
      List.map Modulegen.statement_to_program;
    Stage.Debug.show_imports debug_programs;
    Stage.Debug.show_types debug_programs;
    Stage.Debug.show_flow debug_programs
  } else {
    ()
  };
  Stage.parse_source module_name module_def |>
  List.map Modulegen.statement_to_program |> Imports.link |>
  List.map Stage.optimize_program |>
  List.map Stage.render_program |> Stage.combine_programs
};
