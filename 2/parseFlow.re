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
  | _ =>
    raise(
      Errors2.NotSupported({
        message: "Unsupported Flow type",
        loc: errorLocation(loc),
      }),
    )
  };
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
      typeParameters: [||],
    })
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
  DotTyped.ModuleDeclaration({
    name: Identifier(name),
    declarations:
      statements |. Belt.List.toArray |. Belt.Array.map(flowAstToTypedAst),
  });
};
