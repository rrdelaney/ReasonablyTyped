exception ExpectedIdentifier;

let getName =
  fun
  | Typescript.FunctionDeclaration({name: Identifier({text})}) => text
  | Typescript.InterfaceDeclaration({name: Identifier({text})}) => text
  | Typescript.Parameter({name: Identifier({text})}) => text
  | Typescript.Identifier({text}) => text
  | _ => raise(ExpectedIdentifier);

let rec propertySignatureToObjectProperty =
        (prop: Typescript.propertySignature) => (
  getName(prop.name),
  typescriptAstToBsType(prop.type_),
  switch prop.questionToken {
  | Some(_) => true
  | None => false
  }
)
and typescriptAstToBsType =
  fun
  | Typescript.Parameter(parameter) => typescriptAstToBsType(parameter.type_)
  | Typescript.NumberKeyword(_) => BsTypeAst.Number
  | Typescript.StringKeyword(_) => BsTypeAst.String
  | _ => BsTypeAst.Any;

let rec typescriptAstToBsTypeAst =
  fun
  | Typescript.SourceFile(sourceFile) =>
    BsTypeAst.ModuleDecl(
      "\"" ++ Genutils.normalize_name(sourceFile.fileName) ++ "\"",
      sourceFile.statements
      |> Array.to_list
      |> List.map(typescriptAstToBsTypeAst)
    )
  | Typescript.FunctionDeclaration(func) =>
    BsTypeAst.FuncDecl(
      getName(func.name),
      BsTypeAst.Function({
        typeParams: [],
        formalParams:
          func.parameters
          |> Array.to_list
          |> List.map(param => (getName(param), typescriptAstToBsType(param))),
        restParam: None,
        returnType: typescriptAstToBsType(func.type_)
      })
    )
  | Typescript.InterfaceDeclaration(interface) =>
    BsTypeAst.InterfaceDecl(
      getName(interface.name),
      [],
      BsTypeAst.Object(
        interface.members
        |> Array.to_list
        |> List.map(
             fun
             | Typescript.PropertySignature(p) =>
               propertySignatureToObjectProperty(p)
             | _ => raise(Not_found)
           )
      )
    )
  | _ => BsTypeAst.Noop;
