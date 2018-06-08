open Belt;

exception ReasonGenerationError(string);

let rec extractName = identifier =>
  switch (identifier) {
  | DotTyped.Identifier(id) => id
  | DotTyped.UnknownIdentifier => "Unknown"
  | DotTyped.MemberAccess(id, member) => id ++ "." ++ extractName(member)
  };

let extractTypeName = identifier => {
  let name = extractName(identifier);
  let first = Js.String.get(name, 0);
  let rest = Js.String.sliceToEnd(~from=1, name);
  Js.String.toLowerCase(first) ++ rest;
};

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
    )

  | _ => raise(ReasonGenerationError("Unknown dottyped type"));

let rec compile =
        (~moduleName=?, ~typeTable=?, ~skipFmt=false, moduleDefinition) =>
  switch (moduleDefinition) {
  | DotTyped.ModuleDeclaration({name, declarations}) =>
    let declarations =
      Array.map(
        declarations,
        compile(
          ~moduleName=extractName(name),
          ~typeTable=TypeTable2.make(declarations),
          ~skipFmt,
        ),
      );

    if (skipFmt) {
      Js.Array.joinWith("\n", declarations);
    } else {
      Js.Array.joinWith("\n", declarations)
      |. Reason.parseRE
      |. Reason.printRE;
    };

  | DotTyped.ReactComponent({name, type_: DotTyped.Object(propTypes)}) =>
    let hasOptional = Array.some(propTypes.properties, prop => prop.optional);

    Rabel.module_(
      extractModuleName(name),
      [|
        Rabel.Decorators.bsDeriving(
          "abstract",
          Rabel.type_(
            "jsProps",
            Rabel.Types.record_(
              Array.map(propTypes.properties, prop =>
                (
                  extractName(prop.name),
                  fromDotTyped(prop.type_),
                  prop.optional,
                )
              ),
            ),
          ),
        ),
        Rabel.Decorators.bsModule(
          ~module_=Option.getExn(moduleName),
          Rabel.external_(
            "reactClass",
            "ReasonReact.reactClass",
            extractName(name),
          ),
        ),
        Rabel.let_(
          "make",
          Rabel.function_(
            Array.concat(
              Array.map(propTypes.properties, prop =>
                (
                  extractName(prop.name),
                  true,
                  prop.optional ? Some("?") : None,
                )
              ),
              [|("children", false, None)|],
            ),
            Rabel.Ast.apply(
              "ReasonReact.wrapJsForReason",
              [|
                "~reactClass",
                "~props="
                ++ Rabel.Ast.apply(
                     "jsProps",
                     Array.concat(
                       Array.map(propTypes.properties, prop =>
                         "~"
                         ++ extractName(prop.name)
                         ++ (prop.optional ? "?" : "")
                       ),
                       hasOptional ? [|"()"|] : [||],
                     ),
                   ),
                "children",
              |],
            ),
          ),
        ),
      |],
    );

  | DotTyped.ReactComponent({name, type_: DotTyped.Named(propTypesName)}) =>
    let maybePropTypes =
      Map.String.get(Option.getExn(typeTable), extractName(propTypesName));
    let propTypes =
      switch (maybePropTypes) {
      | Some(DotTyped.Object(p)) => p
      | _ =>
        raise(ReasonGenerationError("React prop types must be an object"))
      };
    let hasOptional = Array.some(propTypes.properties, prop => prop.optional);

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
        Rabel.let_(
          "make",
          Rabel.function_(
            Array.concat(
              Array.map(propTypes.properties, prop =>
                (
                  extractName(prop.name),
                  true,
                  prop.optional ? Some("?") : None,
                )
              ),
              [|("children", false, None)|],
            ),
            Rabel.Ast.apply(
              "ReasonReact.wrapJsForReason",
              [|
                "~reactClass",
                "~props="
                ++ Rabel.Ast.apply(
                     extractModuleName(propTypesName) ++ ".t",
                     Array.concat(
                       Array.map(propTypes.properties, prop =>
                         "~"
                         ++ extractName(prop.name)
                         ++ (prop.optional ? "?" : "")
                       ),
                       hasOptional ? [|"()"|] : [||],
                     ),
                   ),
                "children",
              |],
            ),
          ),
        ),
      |],
    );

  | DotTyped.LetDeclaration({name, type_}) =>
    Rabel.Decorators.bsModule(
      ~module_=Option.getExn(moduleName),
      Rabel.external_(extractName(name), fromDotTyped(type_), ""),
    )

  | DotTyped.ClassDeclaration({name, type_: DotTyped.Object({properties})})
  | DotTyped.InterfaceDeclaration({
      name,
      type_: DotTyped.Object({properties}),
    }) =>
    Rabel.module_(
      extractModuleName(name),
      [|
        Rabel.Decorators.bsDeriving(
          "abstract",
          Rabel.type_(
            "t",
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

  | _ => raise(ReasonGenerationError("Unknown dottyped declaration"))
  };
