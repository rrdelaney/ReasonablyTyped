module ImportTable = {
  /* (local, (remote, source)) */
  type t = list (string, (string, string));
  let add name from table => [(name, from), ...table];
  let get key table::(table: t) => {
    let lookup =
      try (List.assoc key table) {
      | Not_found => ("NOT_FOUND", "NOT_FOUND")
      };
    switch lookup {
    | ("NOT_FOUND", "NOT_FOUND") => None
    | s => Some s
    }
  };
  let show table => {
    print_endline "=== Imports ===";
    List.iter
      (
        fun (local_name, (remote_name, module_name)) =>
          print_endline (
            "import type { " ^
            (
              if (remote_name == local_name) {
                remote_name
              } else {
                remote_name ^ " as " ^ local_name
              }
            ) ^
            " } from " ^ module_name
          )
      )
      table;
    print_newline ()
  };
};

type renameImportReducer = {
  imports: ImportTable.t,
  statements: list Modulegen.BsDecl.t
};

let process_module imports::(imports: ImportTable.t) =>
  List.map (
    Genutils.walk (
      fun
      | Named params name module_name =>
        switch (ImportTable.get name imports) {
        | Some (remote, source) =>
          Some (
            Named params remote (Some (Genutils.import_module_name source))
          )
        | _ => None
        }
      | s => None
    )
  );

let linker =
  List.fold_left
    (
      fun {imports, statements} statement =>
        switch statement {
        | Modulegen.BsDecl.ImportDecl names source => {
            statements: statements @ [statement],
            imports:
              imports @
              List.map (fun (remote, local) => (local, (remote, source))) names
          }
        | Modulegen.BsDecl.ModuleDecl name statements => {
            imports,
            statements:
              statements @ [
                Modulegen.BsDecl.ModuleDecl
                  name (process_module imports statements)
              ]
          }
        | _ => {statements: statements @ [statement], imports}
        }
    )
    {imports: [], statements: []};

let show_imports program => {
  let linked_program = linker program;
  ImportTable.show linked_program.imports
};

let link program => {
  let linked_program = linker program;
  linked_program.statements
};
