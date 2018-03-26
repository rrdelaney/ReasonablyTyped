exception CannotExtractName;

let getName =
  fun
  | Graphql.Name({value}) => value
  | _ => raise(CannotExtractName);

let rec fieldToObjectProperty =
  fun
  | Graphql.FieldDefinition(field) => (
      getName(field.name),
      if (Array.length(field.arguments) == 0) {
        graphqlAstToBsType(field.type_);
      } else {
        BsTypeAst.Function({
          typeParams: [],
          formalParams:
            field.arguments
            |> Array.to_list
            |> List.map(
                 fun
                 | Graphql.InputValueDefinition(input) => (
                     getName(input.name),
                     graphqlAstToBsType(input.type_)
                   )
                 | _ => raise(Not_found)
               ),
          restParam: None,
          returnType: graphqlAstToBsType(field.type_)
        });
      },
      Array.length(field.arguments) == 0
    )
  | _ => raise(Not_found)
and graphqlAstToBsType =
  fun
  | NamedType({name}) when getName(name) == "string" => BsTypeAst.String
  | NamedType({name}) when getName(name) == "ID" => BsTypeAst.String
  | NamedType({name}) => BsTypeAst.Named([], getName(name), None)
  | _ => BsTypeAst.Any;

let rec graphqlAstToBsTypeAst =
  fun
  | Graphql.Document(doc) =>
    BsTypeAst.ModuleDecl(
      "\"" ++ Genutils.normalize_name(doc.loc.source.name) ++ "\"",
      doc.definitions |> Array.to_list |> List.map(graphqlAstToBsTypeAst)
    )
  | Graphql.ObjectTypeDefinition(def) =>
    BsTypeAst.InterfaceDecl(
      getName(def.name),
      [],
      BsTypeAst.Object(
        def.fields |> Array.to_list |> List.map(fieldToObjectProperty)
      )
    )
  | _ => BsTypeAst.Noop;
