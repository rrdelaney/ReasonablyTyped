module Internal = {
  module Kind = {
    type t = string;
    [@bs.module "graphql"] [@bs.scope "Kind"] external name : t = "NAME";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external document : t = "DOCUMENT";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external operationDefinition : t = "OPERATION_DEFINITION";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external variableDefinition : t = "VARIABLE_DEFINITION";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external variable : t = "VARIABLE";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external selectionSet : t = "SELECTION_SET";
    [@bs.module "graphql"] [@bs.scope "Kind"] external field : t = "FIELD";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external argument : t = "ARGUMENT";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external fragmentSpread : t = "FRAGMENT_SPREAD";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external inlineFragment : t = "INLINE_FRAGMENT";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external fragmentDefinition : t = "FRAGMENT_DEFINITION";
    [@bs.module "graphql"] [@bs.scope "Kind"] external int : t = "INT";
    [@bs.module "graphql"] [@bs.scope "Kind"] external float : t = "FLOAT";
    [@bs.module "graphql"] [@bs.scope "Kind"] external string : t = "STRING";
    [@bs.module "graphql"] [@bs.scope "Kind"] external boolean : t = "BOOLEAN";
    [@bs.module "graphql"] [@bs.scope "Kind"] external null : t = "NULL";
    [@bs.module "graphql"] [@bs.scope "Kind"] external enum : t = "ENUM";
    [@bs.module "graphql"] [@bs.scope "Kind"] external list : t = "LIST";
    [@bs.module "graphql"] [@bs.scope "Kind"] external object_ : t = "OBJECT";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external objectField : t = "OBJECT_FIELD";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external directive : t = "DIRECTIVE";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external namedType : t = "NAMED_TYPE";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external listType : t = "LIST_TYPE";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external nonNullType : t = "NON_NULL_TYPE";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external schemaDefinition : t = "SCHEMA_DEFINITION";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external operationTypeDefinition : t = "OPERATION_TYPE_DEFINITION";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external scalarTypeDefinition : t = "SCALAR_TYPE_DEFINITION";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external objectTypeDefinition : t = "OBJECT_TYPE_DEFINITION";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external fieldDefinition : t = "FIELD_DEFINITION";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external inputValueDefinition : t = "INPUT_VALUE_DEFINITION";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external interfaceTypeDefinition : t = "INTERFACE_TYPE_DEFINITION";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external unionTypeDefinition : t = "UNION_TYPE_DEFINITION";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external enumTypeDefinition : t = "ENUM_TYPE_DEFINITION";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external enumValueDefinition : t = "ENUM_VALUE_DEFINITION";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external inputObjectTypeDefinition : t = "INPUT_OBJECT_TYPE_DEFINITION";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external scalarTypeExtension : t = "SCALAR_TYPE_EXTENSION";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external objectTypeExtension : t = "OBJECT_TYPE_EXTENSION";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external interfaceTypeExtension : t = "INTERFACE_TYPE_EXTENSION";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external unionTypeExtension : t = "UNION_TYPE_EXTENSION";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external enumTypeExtension : t = "ENUM_TYPE_EXTENSION";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external inputObjectTypeExtension : t = "INPUT_OBJECT_TYPE_EXTENSION";
    [@bs.module "graphql"] [@bs.scope "Kind"]
    external directiveDefinition : t = "DIRECTIVE_DEFINITION";
  };
  type node = {. "kind": Kind.t};
  module Source = {
    type t;
    [@bs.module "graphql"] [@bs.new]
    external make : (string, string) => t = "Source";
  };
  [@bs.module "graphql"] external parse : Source.t => node = "";
};

type source = {
  body: string,
  name: string
};

type loc = {
  start: int,
  end_: int,
  source
};

type document = {
  definitions: array(node),
  loc
}
and objectTypeDefinition = {
  name: node,
  interfaces: array(node),
  directives: array(node),
  fields: array(node),
  loc
}
and fieldDefinition = {
  name: node,
  arguments: array(node),
  type_: node,
  directives: array(node),
  loc
}
and inputValueDefinition = {
  name: node,
  type_: node,
  defaultValue: option(node),
  directives: array(node),
  loc
}
and namedType = {
  name: node,
  loc
}
and nonNullType = {
  type_: node,
  loc
}
and name = {
  value: string,
  loc
}
and node =
  | Document(document)
  | ObjectTypeDefinition(objectTypeDefinition)
  | FieldDefinition(fieldDefinition)
  | InputValueDefinition(inputValueDefinition)
  | NamedType(namedType)
  | NonNullType(nonNullType)
  | Name(name)
  | Unknown(string);

module Decoder = {
  let source = json =>
    Json.Decode.{
      body: json |> field("body", string),
      name: json |> field("name", string)
    };
  let loc = json =>
    Json.Decode.{
      start: json |> field("start", int),
      end_: json |> field("end", int),
      source: json |> field("source", source)
    };
  external nodeToJson : Internal.node => Js.Json.t = "%identity";
  let rec decoders = [
    (Internal.Kind.document, document),
    (Internal.Kind.objectTypeDefinition, objectTypeDefinition),
    (Internal.Kind.fieldDefinition, fieldDefinition),
    (Internal.Kind.inputValueDefinition, inputValueDefinition),
    (Internal.Kind.namedType, namedType),
    (Internal.Kind.nonNullType, nonNullType),
    (Internal.Kind.name, name)
  ]
  and node = json => {
    let kind = json |> Json.Decode.field("kind", Json.Decode.string);
    let decoder =
      try (List.assoc(kind, decoders)) {
      | _ => unknown
      };
    decoder(json);
  }
  and document = json =>
    Document(
      Json.Decode.{
        definitions: json |> field("definitions", array(node)),
        loc: json |> field("loc", loc)
      }
    )
  and objectTypeDefinition = json =>
    ObjectTypeDefinition(
      Json.Decode.{
        name: json |> field("name", node),
        interfaces: json |> field("interfaces", array(node)),
        directives: json |> field("directives", array(node)),
        fields: json |> field("fields", array(node)),
        loc: json |> field("loc", loc)
      }
    )
  and fieldDefinition = json =>
    FieldDefinition(
      Json.Decode.{
        name: json |> field("name", node),
        arguments: json |> field("arguments", array(node)),
        type_: json |> field("type", node),
        directives: json |> field("directives", array(node)),
        loc: json |> field("loc", loc)
      }
    )
  and inputValueDefinition = json =>
    InputValueDefinition(
      Json.Decode.{
        name: json |> field("name", node),
        type_: json |> field("type", node),
        defaultValue: json |> optional(field("defaultValue", node)),
        directives: json |> field("directives", array(node)),
        loc: json |> field("loc", loc)
      }
    )
  and namedType = json =>
    NamedType(
      Json.Decode.{
        name: json |> field("name", node),
        loc: json |> field("loc", loc)
      }
    )
  and nonNullType = json =>
    NonNullType(
      Json.Decode.{
        type_: json |> field("type", node),
        loc: json |> field("loc", loc)
      }
    )
  and name = json =>
    Name(
      Json.Decode.{
        value: json |> field("value", string),
        loc: json |> field("loc", loc)
      }
    )
  and unknown = json => {
    let kind = Json.Decode.field("kind", Json.Decode.string, json);
    Unknown(kind);
  };
  let decode = graphqlNode => {
    let json = nodeToJson(graphqlNode);
    node(json);
  };
};

let parse = (fileName: string, source: string) => {
  let sourceFile = Internal.Source.make(source, fileName);
  let parsedSource = Internal.parse(sourceFile);
  Decoder.decode(parsedSource);
};
