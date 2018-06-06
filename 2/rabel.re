open Belt;

module Ast = {
  let apply = (func: string, args: array(string)) =>
    func ++ "(" ++ Js.Array.joinWith(",", args) ++ ")";

  let decorate = (decorator: string, statement: string) =>
    "[@" ++ decorator ++ "] " ++ statement;
};

module Decorators = {
  let bsOptional = Ast.decorate("bs.optional");
  let bsAs = name => Ast.decorate("bs.as \"" ++ name ++ "\"");
  let bsModule = (~module_=?, statement) => {
    let decorator =
      switch (module_) {
      | Some(moduleName) => "bs.module \"" ++ moduleName ++ "\""
      | None => "bs.module"
      };
    Ast.decorate(decorator, statement);
  };
  let bsDeriving = deriving => Ast.decorate("bs.deriving " ++ deriving);
};

module Types = {
  let record_ = ts => {
    let fields =
      Array.map(
        ts,
        ((name, t, optional)) => {
          let field = name ++ ": " ++ t ++ ", ";
          if (optional) {
            Decorators.bsOptional(field);
          } else {
            field;
          };
        },
      );

    "{ " ++ Js.Array.joinWith("", fields) ++ "}";
  };

  let jsRecord = ts => {
    let fields =
      Array.map(ts, ((name, t)) => "\"" ++ name ++ "\": " ++ t ++ ", ");

    "{. " ++ Js.Array.join(fields) ++ "}";
  };

  let string = () => "string";
  let float = () => "float";
  let int = () => "int";
  let bool = () => "bool";
  let unit = () => "unit";
  let null = () => "null";

  let regex = () => "Js.Re.t";
  let dict = t => Ast.apply("Js.Dict.t", [|t|]);

  let optional = t => Ast.apply("option", [|t|]);
  let array = t => Ast.apply("array", [|t|]);

  let tuple = ts => Ast.apply("", ts);

  let function_ = (parameters, returnType) => {
    let hasOptional =
      Array.some(parameters, ((_name, _type, optional)) => optional);

    let paramStrs =
      Array.map(parameters, ((name, type_, optional)) =>
        if (! hasOptional) {
          type_;
        } else if (! optional) {
          "~" ++ name ++ ": " ++ type_;
        } else {
          "~" ++ name ++ ": " ++ type_ ++ "=?";
        }
      );

    let paramList = Js.Array.joinWith(", ", paramStrs);

    let fullParams =
      if (hasOptional) {
        paramList ++ ", unit";
      } else {
        paramList;
      };

    "(" ++ fullParams ++ ") => " ++ returnType;
  };
};

let module_ = (name, statements) =>
  "module "
  ++ name
  ++ " = {\n"
  ++ (statements |. Array.map(s => "  " ++ s) |> Js.Array.joinWith("\n"))
  ++ "\n};";

let type_ = (name, t) => "type " ++ name ++ " = " ++ t ++ ";";

let external_ = (name, type_, exported) =>
  "external " ++ name ++ " : " ++ type_ ++ " = " ++ "\"" ++ exported ++ "\";";
