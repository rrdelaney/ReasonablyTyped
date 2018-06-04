open Belt;

module Ast = {
  let apply = (func: string, args: array(string)) =>
    func ++ "(" ++ Js.Array.joinWith(",", args) ++ ")";
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
