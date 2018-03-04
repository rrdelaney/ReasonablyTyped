exception ExpectedIdentifier;

let getName =
  fun
  | Typescript.FunctionDeclaration({name: Identifier({text})}) => text
  | Typescript.Parameter({name: Identifier({text})}) => text
  | Typescript.Identifier({text}) => text
  | _ => raise(ExpectedIdentifier);

let rec typescriptAstToBsType =
  fun
  | Typescript.Parameter(parameter) => typescriptAstToBsType(parameter.type_)
  | Typescript.NumberKeyword(_) => BsTypeAst.Number
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
  | _ => BsTypeAst.Noop;
