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
/** Takes a Flow function and creates a DotTyped type annotation for it. */
and functionTypeToTyped = (func: FlowAst.Type.Function.t) => {
  let makeFunctionProperty =
      (param: FlowAst.Type.Function.Param.t)
      : DotTyped.property => {
    open FlowAst.Type.Function.Param;
    let (_loc, {name, typeAnnotation, optional}) = param;
    let paramName =
      switch (name) {
      | Some(id) => dotTypedIdentifier(id)
      | None => DotTyped.UnknownIdentifier
      };
    let paramType = flowTypeToTyped(typeAnnotation);
    {name: paramName, type_: paramType, optional};
  };

  let makeRestProperty = (param: FlowAst.Type.Function.RestParam.t) => {
    open FlowAst.Type.Function.RestParam;
    let (_loc, {argument}) = param;
    makeFunctionProperty(argument);
  };

  let (formal, rest) = func.params;
  let formalParamProps = List.map(formal, makeFunctionProperty);
  let restParamProp = Option.map(rest, makeRestProperty);
  let returnType = flowTypeToTyped(func.returnType);
  DotTyped.Function({
    parameters: List.toArray(formalParamProps),
    restParameter: restParamProp,
    returnType,
  });
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
  | FlowAst.Statement.DeclareFunction({id, typeAnnotation}) =>
    DotTyped.FunctionDeclaration({
      name: dotTypedIdentifier(id),
      type_: typeAnnotationToTyped(typeAnnotation),
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
    declarations: statements |. List.toArray |. Array.map(flowAstToTypedAst),
  });
};
