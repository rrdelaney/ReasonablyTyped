module ImportTable = {
  /* (local, (remote, source)) */
  type t = list (string, (string, string));
  let add name from table => [(name, from), ...table];
  let get key table => {
    let lookup =
      try (List.assoc key table) {
      | Not_found => None
      };
    Some lookup
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

let process_module =
  List.map (
    Genutils.walk (
      fun
      | _ => None
    )
  );

let link program => {
  let linked_program =
    List.fold_left
      (
        fun {imports, statements} statement =>
          switch statement {
          | Modulegen.BsDecl.ImportDecl names source => {
              statements: statements @ [statement],
              imports:
                imports @
                List.map
                  (fun (remote, local) => (local, (remote, source))) names
            }
          | Modulegen.BsDecl.ModuleDecl name statements => {
              imports,
              statements:
                statements @ [
                  Modulegen.BsDecl.ModuleDecl name (process_module statements)
                ]
            }
          | _ => {statements: statements @ [statement], imports}
          }
      )
      {imports: [], statements: []}
      program;
  /* DEBUG */
  ImportTable.show linked_program.imports;
  linked_program.statements
};
