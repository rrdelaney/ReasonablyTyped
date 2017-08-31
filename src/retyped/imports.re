module ImportTable = {
  type t = list (string, string);
  let make () => [];
  let add name from table => [(name, from), ...table];
  let get key table => {
    let lookup =
      try (List.assoc key table) {
      | Not_found => None
      };
    Some lookup
  };
  let show table => {
    print_endline "Imports:";
    List.iter
      (
        fun (import_name, module_name) =>
          print_endline ("import " ^ import_name ^ " from " ^ module_name)
      )
      table;
    print_newline ()
  };
};

type renameImportReducer = {
  imports: ImportTable.t,
  statements: list Modulegen.BsDecl.t
};

let link program => {
  let linked_program =
    List.fold_left
      (
        fun {imports, statements} statement =>
          switch statement {
          | _ => {statements: statements @ [statement], imports}
          }
      )
      {imports: ImportTable.make (), statements: []}
      program;
  linked_program.statements
};
