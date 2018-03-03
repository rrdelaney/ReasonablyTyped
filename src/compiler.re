module Stage = {
  let parse_source = (name, source) => {
    let (ocaml_ast, _errors) =
      Parser_flow.program_file(source, Some(Loc.SourceFile(name)));
    let (_, statements, _) = ocaml_ast;
    statements;
  };
  let make_module_typetable =
    fun
    | BsTypeAst.ModuleDecl(_, statements) => Typetable.create(statements)
    | _ => [];
  let optimize_program = program =>
    Optimizer.optimize(make_module_typetable(program), program);
  let optimize_programs = programs => List.map(optimize_program, programs);
  let render_program = (globalTypeTable, program) =>
    Codegen.program_to_code(
      program,
      make_module_typetable(program) @ globalTypeTable
    );
  let render_programs = programs => {
    let globalTypeTable = Typetable.create(programs);
    List.map(render_program(globalTypeTable), programs);
  };
  let combine_programs =
    List.fold_left(
      ((current_id, all_code), result) =>
        switch result {
        | Some((program_id, program_code)) when program_id !== "" => (
            program_id,
            all_code ++ "\n" ++ program_code
          )
        | Some((_program_id, program_code)) => (
            current_id,
            all_code ++ "\n" ++ program_code
          )
        | None => (current_id, all_code)
        },
      ("Unknown ID", "")
    );
  module Debug = {
    open BsTypeAst;
    let show_imports = programs => {
      print_endline("\027[1;36m=== Imports ===\027[0m");
      Imports.show_imports(programs);
    };
    let show_types = programs => {
      print_endline("\027[1;36m=== Types ===\027[0m");
      Typetable.show(Typetable.create(programs));
      List.iter(
        fun
        | ModuleDecl(_name, _statements) as md =>
          Typetable.show(make_module_typetable(md))
        | _ => (),
        programs
      );
    };
    let show_flow = programs => {
      let flow_code =
        List.map(Flowprinter.show_decl, programs) |> String.concat("\n");
      print_endline("\027[1;36m=== Flow Definition ===\027[0m");
      print_endline(flow_code);
      print_newline();
    };
    let show_code = result => {
      let (name, code) = result;
      print_endline("\027[1;36m=== Bucklescript Definition ===\027[0m");
      print_endline("\027[1;30m/* Module " ++ name ++ " */\027[0m");
      print_endline(code);
    };
  };
};

let compile = (module_name, module_def, debug) => {
  let result =
    Stage.parse_source(module_name, module_def)
    |> List.map(Modulegen.statement_to_program)
    |> Imports.link
    |> Stage.optimize_programs
    |> Stage.render_programs
    |> Stage.combine_programs;
  if (debug) {
    let debug_programs =
      Stage.parse_source(module_name, module_def)
      |> List.map(Modulegen.statement_to_program);
    Stage.Debug.show_imports(debug_programs);
    Stage.Debug.show_types(debug_programs);
    Stage.Debug.show_flow(debug_programs);
    Stage.Debug.show_code(result);
  } else {
    ();
  };
  result;
};
