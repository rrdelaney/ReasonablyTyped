open Belt;

module Ast = {
  let apply = (func: string, args: array(string)) =>
    func ++ "(" ++ Js.Array.joinWith(",", args) ++ ")";
};

let module_ = (name, statements) =>
  "module "
  ++ name
  ++ " = {\n"
  ++ (statements |. Array.map(s => "  " ++ s) |> Js.Array.joinWith("\n"))
  ++ "\n}";

let type_ = (name, t) => "type " ++ name ++ " = " ++ t ++ ";";

let record_ = ts => {
  let fields = Array.map(ts, ((name, t)) => name ++ ": " ++ t ++ ", ");

  "{ " ++ Js.Array.join(fields) ++ "}";
};

let jsRecord = ts => {
  let fields =
    Array.map(ts, ((name, t)) => "\"" ++ name ++ "\": " ++ t ++ ", ");

  "{. " ++ Js.Array.join(fields) ++ "}";
};

let bsModule = (~module_=?, statement) => {
  let decorator =
    switch (module_) {
    | Some(moduleName) => "[@bs.module \"" ++ moduleName ++ "\"]"
    | None => "[@bs.module]"
    };
  decorator ++ " " ++ statement;
};

let bsDeriving = (deriving, statement) => {
  let decorator = "[@bs.deriving " ++ deriving ++ "]";
  decorator ++ " " ++ statement;
};

let external_ = (name, type_, exported) =>
  "external " ++ name ++ " : " ++ type_ ++ " = " ++ "\"" ++ exported ++ "\";";

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
