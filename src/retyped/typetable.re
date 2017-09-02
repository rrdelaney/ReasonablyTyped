type t =
  | Class
  | Type Modulegen.BsType.t
  | Variable string
  | None
  | NotFound;

let rec get key table => {
  let lookup =
    try (List.assoc key table) {
    | Not_found => NotFound
    };
  switch lookup {
  | Variable s => get s table
  | match => match
  }
};

let create statements =>
  List.map
    Modulegen.BsDecl.(
      fun
      | VarDecl id type_of => (id, None)
      | ClassDecl id type_params type_of => (id, Class)
      | TypeDecl id type_params type_of => (id, Type type_of)
      | FuncDecl id type_of => (id, Type type_of)
      | _ => ("", None)
    )
    statements |>
  List.filter (fun (key, _) => key != "");

let show table => {
  List.iter
    (
      fun (id, typeof) =>
        print_endline (
          "typeof " ^
          id ^
          " = " ^ (
            switch typeof {
            | Class => "Class"
            | None => "None"
            | NotFound => "NotFound"
            | Variable s => "typeof " ^ s
            | Type t => Flowprinter.show_type t
            }
          )
        )
    )
    table;
  print_newline ()
};
