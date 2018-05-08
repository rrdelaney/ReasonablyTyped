let formatObjKey = key =>
  if (String.contains(key, '-')) {
    "'" ++ key ++ "'";
  } else {
    key;
  };

let extractName = identifier =>
  switch (identifier) {
  | DotTyped.Identifier(id) => id
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
