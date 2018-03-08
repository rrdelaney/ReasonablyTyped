module Stage = {
  let parseSource = (name, source) => {
    let parseFlowSource = (name, source) => {
      let (flowAst, _errors) =
        Parser_flow.program_file(source, Some(Loc.SourceFile(name)));
      let (_, statements, _) = flowAst;
      statements;
    };
    let parseTypescriptSource = (filename, source) => {
      let isDts =
        String.sub(filename, String.length(filename) - 5, 5) == ".d.ts";
      let module_name =
        if (isDts) {
          String.sub(filename, 0, String.length(filename) - 5);
        } else {
          String.sub(filename, 0, String.length(filename) - 3);
        };
      Typescript.parse(module_name, source);
    };
    let extension = {
      let parts = Js.String.split(".", name);
      parts[Array.length(parts) - 1];
    };
    switch extension {
    | "js" =>
      parseFlowSource(name, source) |> List.map(FlowBsType.flowAstToBsTypeAst)
    | "ts" => [
        parseTypescriptSource(name, source)
        |> TypescriptBsType.typescriptAstToBsTypeAst
      ]
    | "graphql" => [
        Graphql.parse(name, source) |> GraphqlBsType.graphqlAstToBsTypeAst
      ]
    | "css" => [
        Csstree.parse(name, source) |> CssBsType.cssAstToBsTypeAst(name)
      ]
    | _ => []
    };
  };
  let make_module_typetable =
    fun
    | BsTypeAst.ModuleDecl(_, statements) => Typetable.create(statements)
    | _ => [];
  let optimizeAst = program =>
    Optimizer.optimize(make_module_typetable(program), program);
  let renderAst = programs => {
    let globalTypeTable = Typetable.create(programs);
    List.map(
      program =>
        BsTypeReason.program_to_code(
          program,
          make_module_typetable(program) @ globalTypeTable
        ),
      programs
    );
  };
  let combineAst =
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
    let showSource = (fileName, source) => {
      print_endline("\027[1;36m=== Source ===\027[0m");
      print_endline("\027[1;30m/* " ++ fileName ++ " */\027[0m");
      print_endline(source);
    };
    let showImports = programs => {
      print_endline("\027[1;36m=== Imports ===\027[0m");
      Imports.show_imports(programs);
    };
    let showTypes = programs => {
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
    let showFlow = programs => {
      let flow_code =
        List.map(BsTypeFlow.show_decl, programs) |> String.concat("\n");
      print_endline("\027[1;36m=== Flow Definition ===\027[0m");
      print_endline(flow_code);
      print_newline();
    };
    let showCode = result => {
      let (name, code) = result;
      print_endline("\027[1;36m=== Bucklescript Definition ===\027[0m");
      print_endline("\027[1;30m/* Module " ++ name ++ " */\027[0m");
      print_endline(code);
    };
  };
};

exception ReportableError(string);

let compile = (moduleName, moduleSource, debug) => {
  let result =
    try (
      Stage.parseSource(moduleName, moduleSource)
      |> Imports.link
      |> List.map(Stage.optimizeAst)
      |> Stage.renderAst
      |> Stage.combineAst
    ) {
    | Parse_error.Error(xs) =>
      raise(Diagnostic.diagnosticOfFlow(xs, moduleSource))
    };
  if (debug) {
    let debugAsts = Stage.parseSource(moduleName, moduleSource);
    Stage.Debug.showSource(moduleName, moduleSource);
    Stage.Debug.showImports(debugAsts);
    Stage.Debug.showTypes(debugAsts);
    Stage.Debug.showFlow(debugAsts);
    Stage.Debug.showCode(result);
  };
  result;
};
