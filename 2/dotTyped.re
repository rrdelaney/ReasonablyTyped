type identifier =
  | Identifier(string);

/* TODO: Add support for JS Modules and JS typeof. */
type t =
  | String
  | Int
  | Float
  | Function(function_)
  | Array(t)
  | Union(array(t))
  | Map(t, t)
  | Object(object_)
  | Promise(t)
  | TypeVariable(identifier)
and property = {
  name: identifier,
  type_: t,
  optional: bool,
}
and function_ = {
  arguments: array(property),
  returnType: t,
}
and object_ = {
  properties: array(property),
  extends: identifier,
};

type typeBinding = {
  name: identifier,
  type_: t,
  arguments: array(string),
};

type classBinding = {
  name: identifier,
  type_: t,
  arguments: array(string),
};

type letBinding = {
  name: identifier,
  type_: t,
  arguments: array(string),
};

type binding =
  | TypeBinding(typeBinding)
  | ClassBinding(classBinding)
  | LetBinding(letBinding);
