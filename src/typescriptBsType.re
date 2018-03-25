exception ExpectedIdentifier;

let getName =
  fun
  | Typescript.FunctionDeclaration({name: Identifier({text})}) => text
  | Typescript.InterfaceDeclaration({name: Identifier({text})}) => text
  | Typescript.Parameter({name: Identifier({text})}) => text
  | Typescript.Identifier({text}) => text
  | _ => raise(ExpectedIdentifier);

let rec memberToObjectProperty =
  fun
  | Typescript.PropertySignature(prop) => (
      getName(prop.name),
      typescriptAstToBsType(prop.type_),
      switch prop.questionToken {
      | Some(_) => true
      | None => false
      }
    )
  | _ => raise(Not_found)
and typescriptAstToBsType =
  fun
  | Typescript.Parameter(parameter) => typescriptAstToBsType(parameter.type_)
  | Typescript.NumberKeyword(_) => BsTypeAst.Number
  | Typescript.StringKeyword(_) => BsTypeAst.String
  | Typescript.TypeLiteral(literal) =>
    BsTypeAst.Object(
      literal.members |> Array.to_list |> List.map(memberToObjectProperty)
    )
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
      /*type params */ [],
      BsTypeAst.Object(
        interface.members |> Array.to_list |> List.map(memberToObjectProperty)
      )
    )
  | Typescript.TypeAliasDeclaration(decl) =>
    BsTypeAst.TypeDecl(
      getName(decl.name),
      /*type params */ [],
      typescriptAstToBsType(decl.type_)
    )
  | _ => BsTypeAst.Noop;
