module ImportTable = {
  /* (local, (remote, source)) */
  type t = list((string, (string, string)));
  let add = (name, from, table) => [(name, from), ...table];
  let get = (key, table: t) => {
    let lookup =
      try (List.assoc(key, table)) {
      | Not_found => ("NOT_FOUND", "NOT_FOUND")
      };
    switch lookup {
    | ("NOT_FOUND", "NOT_FOUND") => None
    | s => Some(s)
    };
  };
  let show = table => {
    List.iter(
      ((local_name, (remote_name, module_name))) =>
        print_endline(
          "import type { "
          ++ (
            if (remote_name == local_name) {
              remote_name;
            } else {
              remote_name ++ " as " ++ local_name;
            }
          )
          ++ " } from '"
          ++ module_name
          ++ "'"
        ),
      table
    );
    print_newline();
  };
};

type renameImportReducer = {
  imports: ImportTable.t,
  statements: list(BsTypeAst.decl)
};

let process_module = imports =>
  List.map(
    Genutils.walk(
      fun
      | Named(params, name, _module_name) =>
        switch (ImportTable.get(name, imports)) {
        | Some((remote, source)) =>
          Some(
            Named(params, remote, Some(Genutils.import_module_name(source)))
          )
        | _ => None
        }
      | _s => None
    )
  );

let linker =
  List.fold_left(
    ({imports, statements}, statement) =>
      switch statement {
      | BsTypeAst.ImportDecl(names, source) => {
          statements: statements @ [statement],
          imports:
            imports
            @ List.map(((remote, local)) => (local, (remote, source)), names)
        }
      | BsTypeAst.ModuleDecl(name, module_statements) => {
          imports,
          statements:
            statements
            @ [
              BsTypeAst.ModuleDecl(
                name,
                process_module(imports, module_statements)
              )
            ]
        }
      | _ => {statements: statements @ [statement], imports}
      },
    {imports: [], statements: []}
  );

let show_imports = program => {
  let linked_program = linker(program);
  ImportTable.show(linked_program.imports);
};

let link = program => {
  let linked_program = linker(program);
  linked_program.statements;
};
