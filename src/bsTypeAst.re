type func = {
  typeParams: list(string),
  formalParams: list((string, t)),
  restParam: option((string, t)),
  returnType: t
}
and t =
  | Null
  | Number
  | Regex
  | String
  | Function(func)
  | AnyFunction
  | Object(list((string, t, bool)))
  | AnyObject
  /* inherited class, class properties */
  | Class(option(t), list((string, t)))
  | Union(list(t))
  | Array(t)
  | Dict(t)
  | Boolean
  | Tuple(list(t))
  | Unit
  | Any
  | Typeof(t)
  /* type params, name, module */
  | Named(list(t), string, option(string))
  | Optional(t)
  | StringLiteral(string)
  | Promise(t)
  | Date;

type decl =
  /* variable name, variable type */
  | VarDecl(string, t)
  /* function name, function type */
  | FuncDecl(string, t)
  /* module name, declarations */
  | ModuleDecl(string, list(decl))
  /* Type of exports */
  | ExportsDecl(t)
  /* Type alias name, type params, inner type */
  | TypeDecl(string, list(string), t)
  /* class name, type params, inner type */
  | ClassDecl(string, list(string), t)
  /* interface name, type params, inner type */
  | InterfaceDecl(string, list(string), t)
  /* import {names as name} from {module} */
  | ImportDecl(list((string, string)), string)
  /* react class: name of the class, props type */
  | ReactClass(string, t)
  /* Nothing */
  | Noop
  /* declaration currently ignored */
  | Ignore(string);
