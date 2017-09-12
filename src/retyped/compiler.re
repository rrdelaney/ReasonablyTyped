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
    let show_imports programs => {
      print_endline "\027[1;36m=== Imports ===\027[0m";
      Imports.show_imports programs
    };
    let show_types programs => {
      print_endline "\027[1;36m=== Types ===\027[0m";
      List.iter
        (
          fun
          | ModuleDecl name statements as md =>
            Typetable.show (make_typetable md)
          | _ => ()
        )
        programs
    };
    let show_flow programs => {
      let flow_code =
        List.map Flowprinter.show_decl programs |> String.concat "\n";
      print_endline "\027[1;36m=== Flow Definition ===\027[0m";
      print_endline flow_code;
      print_newline ()
    };
    let show_code result => {
      let (name, code) = result;
      print_endline "\027[1;36m=== Bucklescript Definition ===\027[0m";
      print_endline ("\027[1;30m/* Module " ^ name ^ " */\027[0m");
      print_endline code
    };
  };
};

let compile ::debug=false module_name module_def => {
  let result =
    Stage.parse_source module_name module_def |>
    List.map Modulegen.statement_to_program |> Imports.link |>
    List.map Stage.optimize_program |>
    List.map Stage.render_program |> Stage.combine_programs;
  if debug {
    let debug_programs =
      Stage.parse_source module_name module_def |>
      List.map Modulegen.statement_to_program;
    Stage.Debug.show_imports debug_programs;
    Stage.Debug.show_types debug_programs;
    Stage.Debug.show_flow debug_programs;
    Stage.Debug.show_code result
  } else {
    ()
  };
  result
};
