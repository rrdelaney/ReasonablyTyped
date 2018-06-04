let formatObjKey = key =>
  if (String.contains(key, '-')) {
    "'" ++ key ++ "'";
  } else {
    key;
  };

let extractName = identifier =>
  switch (identifier) {
  | DotTyped.Identifier(id) => id
  | DotTyped.UnknownIdentifier => "Unknown"
  };

let rec compileType =
  fun
  | DotTyped.Optional(t) => "?" ++ compileType(t)
  | DotTyped.Any => "any"
  | DotTyped.Void => "void"
  | DotTyped.Dict(_, t) => "{ [key: string]: " ++ compileType(t) ++ " }"
  | DotTyped.Tuple(types) =>
    "[" ++ (Array.map(compileType, types) |> Js.Array.joinWith(", ")) ++ "]"
  | DotTyped.Array(t) => compileType(t) ++ "[]"
  | DotTyped.Null => "null"
  | DotTyped.Float => "number"
  | DotTyped.Boolean => "boolean"
  | DotTyped.String => "string"
  | DotTyped.Promise(t) => "Promise<" ++ compileType(t) ++ ">"
  | DotTyped.Function({parameters, rest, returnType}) => {
      let paramList =
        Array.map(
          ({name, type_}: DotTyped.property) =>
            switch (type_) {
            | DotTyped.Void => ""
            | DotTyped.Optional(t) =>
              extractName(name) ++ "?: " ++ compileType(t)
            | _ => extractName(name) ++ ": " ++ compileType(type_)
            },
          parameters,
        );
      "("
      ++ Js.Array.joinWith(", ", paramList)
      ++ (
        switch (rest) {
        | Some(({name, type_}: DotTyped.property)) =>
          (Array.length(paramList) > 0 ? ", " : "")
          ++ "..."
          ++ extractName(name)
          ++ ": "
          ++ compileType(type_)
        | _ => ""
        }
      )
      ++ ") => "
      ++ compileType(returnType);
    }
  | _ => raise(Errors2.Unimplemented);

let rec compile =
  fun
  | DotTyped.ModuleDeclaration({name, declarations}) =>
    "declare module "
    ++ extractName(name)
    ++ " {\n  "
    ++ (declarations |> Array.map(compile) |> Js.Array.joinWith("\n  "))
    ++ "\n}"
  | DotTyped.LetDeclaration({name, type_}) =>
    "declare export var " ++ extractName(name) ++ ": " ++ compileType(type_)
  | _ => raise(Errors2.Unimplemented);
