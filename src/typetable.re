type t =
  | Class
  | Type(BsTypeAst.t)
  | Variable(string)
  | None
  | NotFound;

let rec get = (key, table) => {
  let lookup =
    try (List.assoc(key, table)) {
    | Not_found => NotFound
    };
  switch lookup {
  | Variable(s) => get(s, table)
  | switch_ => switch_
  };
};

let create = statements =>
  List.map(
    fun
    | BsTypeAst.VarDecl(id, _type_of) => (id, None)
    | BsTypeAst.ClassDecl(id, _type_params, _type_of) => (id, Class)
    | BsTypeAst.TypeDecl(id, _type_params, type_of) => (id, Type(type_of))
    | BsTypeAst.FuncDecl(id, type_of) => (id, Type(type_of))
    | _ => ("", None),
    statements
  )
  |> List.filter(((key, _)) => key != "");

let show = table => {
  List.iter(
    ((id, typeof)) =>
      print_endline(
        "typeof "
        ++ id
        ++ " = "
        ++ (
          switch typeof {
          | Class => "Class"
          | None => "None"
          | NotFound => "NotFound"
          | Variable(s) => "typeof " ++ s
          | Type(t) => BsTypeFlow.show_type(t)
          }
        )
      ),
    table
  );
  print_newline();
};
