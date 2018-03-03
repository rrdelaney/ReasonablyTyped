let inline_union_types = types =>
  List.map(((name, t)) =>
    switch t {
    | BsTypeAst.Named(type_params, type_name, _)
        when List.length(type_params) == 0 =>
      Typetable.(
        switch (Typetable.get(type_name, types)) {
        | Type(inner_type) =>
          switch inner_type {
          | Union(_union_types) => (name, inner_type)
          | _ => (name, t)
          }
        | _ => (name, t)
        }
      )
    | _ => (name, t)
    }
  );

let optimize_function = types =>
  fun
  | BsTypeAst.Function(type_params, params, rest_param, rt) => {
      let params = inline_union_types(types, params);
      BsTypeAst.Function(type_params, params, rest_param, rt);
    }
  | f => f;

let optimize_statements = (types, statements) =>
  List.map(
    fun
    | BsTypeAst.FuncDecl(id, type_of) =>
      BsTypeAst.FuncDecl(id, optimize_function(types, type_of))
    | s => s,
    statements
  );

let optimize = (types: list((string, Typetable.t)), program) =>
  switch program {
  | BsTypeAst.ModuleDecl(id, statements) =>
    BsTypeAst.ModuleDecl(id, optimize_statements(types, statements))
  | s => s
  };
