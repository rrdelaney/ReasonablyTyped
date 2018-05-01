module Internal = {
  module Type = {
    let anPlusB = "AnPlusB";
    let atrule = "Atrule";
    let atrulePrelude = "AtrulePrelude";
    let attributeSelector = "AttributeSelector";
    let block = "Block";
    let brackets = "Brackets";
    let cdc = "CDC";
    let cdo = "CDO";
    let classSelector = "ClassSelector";
    let combinator = "Combinator";
    let comment = "Comment";
    let declaration = "Declaration";
    let declarationList = "DeclarationList";
    let dimension = "Dimension";
    let function_ = "Function";
    let hexColor = "HexColor";
    let identifier = "Identifier";
    let idSelector = "IdSelector";
    let mediaFeature = "MediaFeature";
    let mediaQuery = "MediaQuery";
    let mediaQueryList = "MediaQueryList";
    let nth = "Nth";
    let number = "Number";
    let operator = "Operator";
    let parentheses = "Parentheses";
    let percentage = "Percentage";
    let pseudoClassSelector = "PseudoClassSelector";
    let pseudoElementSelector = "PseudoElementSelector";
    let ratio = "Ratio";
    let raw = "Raw";
    let rule = "Rule";
    let selector = "Selector";
    let selectorList = "SelectorList";
    let string = "String";
    let styleSheet = "StyleSheet";
    let typeSelector = "TypeSelector";
    let unicodeRange = "UnicodeRange";
    let url = "Url";
    let value = "Value";
    let whiteSpace = "WhiteSpace";
  };
  type node = {. "type": string};
  type ast;
  [@bs.module "css-tree"] external parse : string => ast = "";
  [@bs.module "css-tree"] external toPlainObject : ast => node = "";
};

type point = {
  offset: int,
  line: int,
  column: int
};

type loc = {
  source: string,
  start: point,
  end_: point
};

type styleSheet = {
  loc: option(loc),
  children: array(node)
}
and rule = {
  loc: option(loc),
  prelude: node,
  block: node
}
and selectorList = {
  loc: option(loc),
  children: array(node)
}
and selector = {
  loc: option(loc),
  children: array(node)
}
and classSelector = {
  loc: option(loc),
  name: string
}
and node =
  | StyleSheet(styleSheet)
  | Rule(rule)
  | SelectorList(selectorList)
  | Selector(selector)
  | ClassSelector(classSelector)
  | Unknown(string);

module Decoder = {
  let point = json =>
    Json.Decode.{
      offset: json |> field("offset", int),
      line: json |> field("line", int),
      column: json |> field("column", int)
    };
  let loc = json =>
    Json.Decode.{
      source: json |> field("source", string),
      start: json |> field("start", point),
      end_: json |> field("end", point)
    };
  external nodeToJson : Internal.node => Js.Json.t = "%identity";
  let rec decoders = [
    (Internal.Type.styleSheet, styleSheet),
    (Internal.Type.rule, rule),
    (Internal.Type.selectorList, selectorList),
    (Internal.Type.selector, selector),
    (Internal.Type.classSelector, classSelector)
  ]
  and node = json => {
    let kind = json |> Json.Decode.field("type", Json.Decode.string);
    let decoder =
      try (List.assoc(kind, decoders)) {
      | _ => unknown
      };
    decoder(json);
  }
  and styleSheet = json =>
    StyleSheet(
      Json.Decode.{
        children: json |> field("children", array(node)),
        loc: json |> optional(field("loc", loc))
      }
    )
  and rule = json =>
    Rule(
      Json.Decode.{
        loc: json |> optional(field("loc", loc)),
        prelude: json |> field("prelude", node),
        block: json |> field("block", node)
      }
    )
  and selectorList = json =>
    SelectorList(
      Json.Decode.{
        loc: json |> optional(field("loc", loc)),
        children: json |> field("children", array(node))
      }
    )
  and selector = json =>
    Selector(
      Json.Decode.{
        loc: json |> optional(field("loc", loc)),
        children: json |> field("children", array(node))
      }
    )
  and classSelector = json =>
    ClassSelector(
      Json.Decode.{
        loc: json |> optional(field("loc", loc)),
        name: json |> field("name", string)
      }
    )
  and unknown = json => {
    let kind = Json.Decode.field("type", Json.Decode.string, json);
    Unknown(kind);
  };
  let decode = cssNode => {
    let json = nodeToJson(cssNode);
    node(json);
  };
};

let parse = (_fileName: string, source: string) => {
  let ast = Internal.parse(source);
  let astObj = Internal.toPlainObject(ast);
  Decoder.decode(astObj);
};
