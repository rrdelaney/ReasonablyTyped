open Belt;

let extractName = identifier =>
  switch (identifier) {
  | DotTyped.Identifier(id) => id
  | DotTyped.UnknownIdentifier => "Unknown"
  };

let extractTypeName = identifier =>
  identifier |. extractName |. Js.String.toLowerCase;

let rec fromDotTyped =
  fun
  | DotTyped.Regex => Rabel.regex()
  | DotTyped.Void => Rabel.unit()
  | DotTyped.Null => Rabel.null()
  | DotTyped.Float => Rabel.float()
  | DotTyped.String => Rabel.string()
  | DotTyped.Boolean => Rabel.bool()
  | DotTyped.Dict(_, t) => Rabel.dict(fromDotTyped(t))
  | DotTyped.Optional(t) => Rabel.optional(fromDotTyped(t))
  | DotTyped.Array(t) => Rabel.array(fromDotTyped(t))
  | DotTyped.Tuple(types) => types |. Array.map(fromDotTyped) |. Rabel.tuple;

let rec compile = (~moduleName=?, moduleDefinition) =>
  switch (moduleDefinition) {
  | DotTyped.ModuleDeclaration({name, declarations}) =>
    let declarations =
      Array.map(declarations, compile(~moduleName=extractName(name)));
    Js.Array.joinWith("\n", declarations);

  | DotTyped.LetDeclaration({name, type_}) =>
    Rabel.bsModule(
      ~module_=Option.getExn(moduleName),
      Rabel.external_(extractName(name), fromDotTyped(type_), ""),
    )

  | DotTyped.InterfaceDeclaration({
      name,
      type_: DotTyped.Object({properties}),
    }) =>
    Rabel.type_(
      extractTypeName(name),
      Rabel.bsDeriving(
        "abstract",
        Rabel.record_(
          Array.map(properties, prop =>
            (extractName(prop.name), fromDotTyped(prop.type_))
          ),
        ),
      ),
    )
  };
