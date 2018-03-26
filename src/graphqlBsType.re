let typescriptAstToBsType =
  fun
  | _ => BsTypeAst.Any;

let rec graphqlAstToBsTypeAst =
  fun
  | Graphql.Document(doc) =>
    BsTypeAst.ModuleDecl(
      "\"" ++ Genutils.normalize_name(doc.loc.source.name) ++ "\"",
      doc.definitions |> Array.to_list |> List.map(graphqlAstToBsTypeAst)
    )
  | _ => BsTypeAst.Noop;
