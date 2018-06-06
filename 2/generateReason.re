open Belt;

exception ReasonGenerationError(string);

let extractName = identifier =>
  switch (identifier) {
  | DotTyped.Identifier(id) => id
  | DotTyped.UnknownIdentifier => "Unknown"
  };

let extractTypeName = identifier =>
  identifier |. extractName |. Js.String.toLowerCase;

let extractModuleName = identifier => {
  let name = extractName(identifier);
  let first = Js.String.get(name, 0);
  let rest = Js.String.sliceToEnd(~from=1, name);
  Js.String.toUpperCase(first) ++ rest;
};

let rec fromDotTyped =
  fun
  | DotTyped.Regex => Rabel.Types.regex()
  | DotTyped.Void => Rabel.Types.unit()
  | DotTyped.Null => Rabel.Types.null()
  | DotTyped.Float => Rabel.Types.float()
  | DotTyped.String => Rabel.Types.string()
  | DotTyped.Boolean => Rabel.Types.bool()
  | DotTyped.Dict(_, t) => Rabel.Types.dict(fromDotTyped(t))
  | DotTyped.Optional(t) => Rabel.Types.optional(fromDotTyped(t))
  | DotTyped.Array(t) => Rabel.Types.array(fromDotTyped(t))
  | DotTyped.Tuple(types) =>
    types |. Array.map(fromDotTyped) |. Rabel.Types.tuple
  | DotTyped.Function({parameters, returnType}) =>
    Rabel.Types.function_(
      Array.map(parameters, ({name, type_, optional}) =>
        (extractName(name), fromDotTyped(type_), optional)
      ),
      fromDotTyped(returnType),
    );

let rec compile = (~moduleName=?, moduleDefinition) =>
  switch (moduleDefinition) {
  | DotTyped.ModuleDeclaration({name, declarations}) =>
    let declarations =
      Array.map(declarations, compile(~moduleName=extractName(name)));
    Js.Array.joinWith("\n", declarations);

  | DotTyped.LetDeclaration({name, type_: DotTyped.ReactComponent({props})}) =>
    Rabel.module_(
      extractModuleName(name),
      [|
        Rabel.Decorators.bsModule(
          ~module_=Option.getExn(moduleName),
          Rabel.external_(
            "reactClass",
            "ReasonReact.reactClass",
            extractName(name),
          ),
        ),
      |],
    )

  | DotTyped.LetDeclaration({name, type_}) =>
    Rabel.Decorators.bsModule(
      ~module_=Option.getExn(moduleName),
      Rabel.external_(extractName(name), fromDotTyped(type_), ""),
    )

  | DotTyped.InterfaceDeclaration({
      name,
      type_: DotTyped.Object({properties}),
    }) =>
    Rabel.module_(
      extractModuleName(name),
      [|
        Rabel.type_(
          "t",
          Rabel.Decorators.bsDeriving(
            "abstract",
            Rabel.Types.record_(
              Array.map(properties, prop =>
                (
                  extractName(prop.name),
                  fromDotTyped(prop.type_),
                  prop.optional,
                )
              ),
            ),
          ),
        ),
      |],
    )

  | DotTyped.InterfaceDeclaration(_) =>
    raise(ReasonGenerationError("Interfaces can only contain objects"))

  | DotTyped.FunctionDeclaration({name, type_}) =>
    Rabel.Decorators.bsModule(
      ~module_=Option.getExn(moduleName),
      Rabel.external_(
        extractName(name),
        fromDotTyped(type_),
        extractName(name),
      ),
    )
  };
