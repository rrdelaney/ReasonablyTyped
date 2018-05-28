type identifier =
  | Identifier(string)
  | UnknownIdentifier;

/* TODO: Add support for JS Modules and JS typeof. */
type t =
  | String
  | Int
  | Float
  | Boolean
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
and property = {
  name: identifier,
  type_: t,
  optional: bool,
}
and function_ = {
  parameters: array(property),
  restParameter: option(property),
  returnType: t,
}
and object_ = {
  properties: array(property),
  extends: identifier,
};

type typeDeclaration = {
  name: identifier,
  type_: t,
  typeParameters: array(string),
}
and classDeclaration = {
  name: identifier,
  type_: t,
  typeParameters: array(string),
}
and letDeclaration = {
  name: identifier,
  type_: t,
  typeParameters: array(string),
}
and moduleDeclaration = {
  name: identifier,
  declarations: array(declaration),
}
and declaration =
  | TypeDeclaration(typeDeclaration)
  | ClassDeclaration(classDeclaration)
  | LetDeclaration(letDeclaration)
  | FunctionDeclaration(letDeclaration)
  | ModuleDeclaration(moduleDeclaration);
