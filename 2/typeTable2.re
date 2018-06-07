open Belt;

let make = (declarations: array(DotTyped.declaration)) =>
  Array.reduce(declarations, Map.String.empty, (typeTable, declaration) =>
    switch (declaration) {
    | DotTyped.InterfaceDeclaration({name, type_})
    | DotTyped.ClassDeclaration({name, type_})
    | DotTyped.LetDeclaration({name, type_})
    | DotTyped.FunctionDeclaration({name, type_})
    | DotTyped.ReactComponent({name, type_}) =>
      switch (name) {
      | DotTyped.Identifier(id) => Map.String.set(typeTable, id, type_)
      | _ => typeTable
      }

    | _ => typeTable
    }
  );
