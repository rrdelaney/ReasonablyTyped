open Modulegen.BsDecl;

open Modulegen.BsType;

let inline_union_types types =>
  List.map (
    fun (name, t) =>
      switch t {
      | Named type_params type_name when List.length type_params == 0 =>
        Typetable.(
          switch (Typetable.get type_name types) {
          | Type inner_type =>
            switch inner_type {
            | Union union_types => (name, inner_type)
            | _ => (name, t)
            }
          | _ => (name, t)
          }
        )
      | _ => (name, t)
      }
  );

let optimize_function types =>
  fun
  | Function type_params params rest_param rt => {
      let params = inline_union_types types params;
      Function type_params params rest_param rt
    }
  | f => f;

let optimize_statements types statements =>
  List.map
    (
      fun
      | FuncDecl id type_of => FuncDecl id (optimize_function types type_of)
      | s => s
    )
    statements;

let optimize types::(types: list (string, Typetable.t)) program =>
  switch program {
  | ModuleDecl id statements => ModuleDecl id (optimize_statements types statements)
  | s => s
  };
