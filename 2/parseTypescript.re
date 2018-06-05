open Belt;

exception CannotExtractName;

let getName =
  fun
  | Typescript.FunctionDeclaration({name: Identifier({text})})
  | Typescript.InterfaceDeclaration({name: Identifier({text})})
  | Typescript.Parameter({name: Identifier({text})})
  | Typescript.Identifier({text}) => DotTyped.Identifier(text)
  | Typescript.TypeParameter({name: Identifier({text})}) =>
    DotTyped.Identifier(text)
  | _ => raise(CannotExtractName);

let rec memberToObjectProperty =
  fun
  | Typescript.PropertySignature(prop) =>
    DotTyped.{
      name: getName(prop.name),
      type_: typescriptAstToDotTyped(prop.type_),
      optional: Option.isSome(prop.questionToken),
    }
  | _ => raise(Not_found)
and typescriptAstToDotTyped =
  fun
  | Typescript.Parameter(parameter) =>
    typescriptAstToDotTyped(parameter.type_)
  | Typescript.NumberKeyword(_) => DotTyped.Float
  | Typescript.StringKeyword(_) => DotTyped.String
  | Typescript.TypeLiteral(literal) =>
    DotTyped.Object({
      properties: Array.map(literal.members, memberToObjectProperty),
      typeParameters: [||],
      extends: None,
    })
  | Typescript.TypeReference(typeRef) =>
    DotTyped.Named(getName(typeRef.typeName))
  | _ => DotTyped.Any;

let rec astToDotTyped = ast =>
  switch (ast) {
  | Typescript.SourceFile(sourceFile) =>
    if (Array.length(sourceFile.parseDiagnostics) > 0) {
      raise(Diagnostic.Error(Diagnostic.diagnosticOfTs(sourceFile)));
    } else {
      DotTyped.ModuleDeclaration({
        name: DotTyped.Identifier(sourceFile.fileName),
        declarations: Array.map(sourceFile.statements, astToDotTyped),
      });
    }
  | Typescript.FunctionDeclaration(func) =>
    DotTyped.FunctionDeclaration({
      name: getName(func.name),
      type_:
        DotTyped.Function({
          typeParameters: Array.map(func.typeParameters, getName),
          parameters:
            Array.map(func.parameters, param =>
              DotTyped.{
                name: getName(param),
                type_: typescriptAstToDotTyped(param),
                optional: false,
              }
            ),
          rest: None,
          returnType: typescriptAstToDotTyped(func.type_),
        }),
    })
  | Typescript.InterfaceDeclaration(interface) =>
    DotTyped.InterfaceDeclaration({
      name: getName(interface.name),
      type_:
        DotTyped.Object({
          properties: Array.map(interface.members, memberToObjectProperty),
          typeParameters: Array.map(interface.typeParameters, getName),
          extends: None,
        }),
    })
  | Typescript.TypeAliasDeclaration(decl) =>
    DotTyped.LetDeclaration({
      name: getName(decl.name),
      type_: typescriptAstToDotTyped(decl.type_),
    })
  | _ => DotTyped.EmptyDeclaration
  };

let parse = (~name, ~source) => {
  let ast = Typescript.parse(name, source);
  let typedAst = astToDotTyped(ast);

  [|typedAst|];
};
