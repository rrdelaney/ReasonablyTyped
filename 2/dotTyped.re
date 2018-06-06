open Belt;

type identifier =
  | Identifier(string)
  | MemberAccess(string, identifier)
  | UnknownIdentifier;

/* TODO: Add support for JS Modules and JS typeof. */
type t =
  | String
  | Int
  | Float
  | Boolean
  | Regex
  | Function(function_)
  | Array(t)
  | Tuple(array(t))
  | Union(array(t))
  | Dict(t, t)
  | Object(object_)
  | Promise(t)
  | TypeVariable(identifier)
  | Optional(t)
  | Any
  | Void
  | Null
  | Named(identifier)
  | TypeAlias
and property = {
  name: identifier,
  type_: t,
  optional: bool,
}
and function_ = {
  parameters: array(property),
  typeParameters: array(identifier),
  rest: option(property),
  returnType: t,
}
and object_ = {
  properties: array(property),
  typeParameters: array(identifier),
  extends: option(identifier),
};

type letDeclaration = {
  name: identifier,
  type_: t,
}
and moduleDeclaration = {
  name: identifier,
  declarations: array(declaration),
}
and declaration =
  | InterfaceDeclaration(letDeclaration)
  | ClassDeclaration(letDeclaration)
  | LetDeclaration(letDeclaration)
  | FunctionDeclaration(letDeclaration)
  | ModuleDeclaration(moduleDeclaration)
  | ReactComponent(letDeclaration)
  | EmptyDeclaration;
