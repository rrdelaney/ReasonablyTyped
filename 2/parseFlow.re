open Belt;

module FlowAst = Spider_monkey_ast;

let errorLocation = (loc: Loc.t) =>
  Errors2.{
    source: "",
    column: loc.start.column,
    line: loc.start.line,
    length: 0,
  };

let dotTypedIdentifier = (id: FlowAst.Identifier.t) =>
  DotTyped.Identifier(snd(id));

let dotTypedIdentifierOfPropertyKey = key =>
  switch (key) {
  | FlowAst.Expression.Object.Property.Identifier(id) =>
    dotTypedIdentifier(id)
  | FlowAst.Expression.Object.Property.Literal((_loc, {value})) =>
    switch (value) {
    | String(s) => DotTyped.Identifier(s)
    | _ => DotTyped.UnknownIdentifier
    }
  | FlowAst.Expression.Object.Property.Computed((loc, _)) =>
    raise(
      Errors2.NotSupported({
        message: "Computed object properties",
        loc: errorLocation(loc),
      }),
    )
  };

/** Transforms a typed Flow AST to a DotTyped AST, extracting the types. */
let rec flowTypeToTyped = (flowType: FlowAst.Type.t) => {
  let (loc, type_) = flowType;
  switch (type_) {
  | FlowAst.Type.String => DotTyped.String
  | FlowAst.Type.Number => DotTyped.Float
  | FlowAst.Type.Boolean => DotTyped.Boolean
  | FlowAst.Type.Any => DotTyped.Any
  | FlowAst.Type.Mixed => DotTyped.Any
  | FlowAst.Type.Void => DotTyped.Void
  | FlowAst.Type.Null => DotTyped.Null
  | FlowAst.Type.StringLiteral(_) => DotTyped.String
  | FlowAst.Type.NumberLiteral(_) => DotTyped.Float
  | FlowAst.Type.BooleanLiteral(_) => DotTyped.Boolean
  | FlowAst.Type.Nullable(t) => DotTyped.Optional(flowTypeToTyped(t))
  | FlowAst.Type.Array(t) => DotTyped.Array(flowTypeToTyped(t))
  | FlowAst.Type.Tuple(types) =>
    DotTyped.Tuple(
      types |. Belt.List.toArray |. Belt.Array.map(flowTypeToTyped),
    )
  | FlowAst.Type.Function(f) => functionTypeToTyped(f)
  | _ =>
    raise(
      Errors2.NotSupported({
        message: "Unsupported Flow type",
        loc: errorLocation(loc),
      }),
    )
  };
}
and functionTypeToTyped = (f: FlowAst.Type.Function.t) => {
  let {params: (formal, rest), returnType, typeParameters: _typeParams}: FlowAst.Type.Function.t = f;
  let paramToProperty = ((_loc, param): FlowAst.Type.Function.Param.t) =>
    DotTyped.{
      name:
        Belt.Option.map(param.name, dotTypedIdentifier)
        |. Belt.Option.getWithDefault(DotTyped.Identifier("")),
      type_: flowTypeToTyped(param.typeAnnotation),
      optional: param.optional,
    };
  let parameters =
    formal |. Belt.List.toArray |. Belt.Array.map(paramToProperty);
  let rest =
    rest
    |. Belt.Option.map(((_loc, restType)) =>
         paramToProperty(restType.argument)
       );
  let returnType = flowTypeToTyped(returnType);
  DotTyped.Function({parameters, rest, returnType, typeParameters: [||]});
};

let objectPropertyValueToTyped = (value: FlowAst.Type.Object.Property.value) =>
  switch (value) {
  | FlowAst.Type.Object.Property.Init(t) => flowTypeToTyped(t)
  | FlowAst.Type.Object.Property.Get((_loc, func))
  | FlowAst.Type.Object.Property.Set((_loc, func)) =>
    functionTypeToTyped(func)
  };

let typeAnnotationToTyped = (annotation: FlowAst.Type.annotation) => {
  let (_, t) = annotation;
  flowTypeToTyped(t);
};

let flowAstToTypedAst = ((loc: Loc.t, s)) =>
  switch (s) {
  | FlowAst.Statement.DeclareVariable({id, typeAnnotation}) =>
    DotTyped.LetDeclaration({
      name: dotTypedIdentifier(id),
      type_:
        Belt.Option.mapWithDefault(
          typeAnnotation,
          DotTyped.Any,
          typeAnnotationToTyped,
        ),
    })

  | FlowAst.Statement.DeclareFunction({id, typeAnnotation}) =>
    DotTyped.FunctionDeclaration({
      name: dotTypedIdentifier(id),
      type_: typeAnnotationToTyped(typeAnnotation),
    })

  | FlowAst.Statement.InterfaceDeclaration({id, body}) =>
    let (_bodyLoc, bodyType) = body;
    let properties = List.toArray(bodyType.properties);
    DotTyped.InterfaceDeclaration({
      name: dotTypedIdentifier(id),
      type_:
        DotTyped.Object({
          properties:
            Array.map(properties, prop =>
              switch (prop) {
              | FlowAst.Type.Object.Property((_loc, {key, value, optional})) =>
                DotTyped.{
                  name: dotTypedIdentifierOfPropertyKey(key),
                  type_: objectPropertyValueToTyped(value),
                  optional,
                }
              | FlowAst.Type.Object.CallProperty((loc, _))
              | FlowAst.Type.Object.Indexer((loc, _))
              | FlowAst.Type.Object.SpreadProperty((loc, _)) =>
                raise(
                  Errors2.NotSupported({
                    message: "Special object properties",
                    loc: errorLocation(loc),
                  }),
                )
              }
            ),
          typeParameters: [||],
          extends: None,
        }),
    });

  | _ =>
    raise(
      Errors2.NotSupported({
        message: "Unknown Flow statement type",
        loc: errorLocation(loc),
      }),
    )
  };

let parse = (~name: string, ~source: string) => {
  let (flowAst, _errors) =
    Parser_flow.program_file(source, Some(Loc.SourceFile(name)));
  let (_, statements, _) = flowAst;

  let typedModule =
    DotTyped.ModuleDeclaration({
      name: Identifier(name),
      declarations:
        statements |. List.toArray |. Array.map(flowAstToTypedAst),
    });

  [|typedModule|];
};
