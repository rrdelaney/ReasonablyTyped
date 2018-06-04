open Belt;

exception MustBeRoot;

let selectorChildrenToBsTypeAst =
  fun
  | Csstree.ClassSelector(classSelector) => [|
      DotTyped.LetDeclaration({
        name: DotTyped.Identifier(classSelector.name),
        type_: DotTyped.String,
      }),
    |]
  | _ => [||];

let selectorToBsTypeAst =
  fun
  | Csstree.Selector(selector) =>
    selector.children
    |. Array.map(selectorChildrenToBsTypeAst)
    |. Array.concatMany
  | _ => [||];

let selectorListToBsTypeAst =
  fun
  | Csstree.SelectorList(selectorList) =>
    selectorList.children
    |. Array.map(selectorToBsTypeAst)
    |. Array.concatMany
  | _ => [||];

let ruleToBsTypeAst =
  fun
  | Csstree.Rule(rule) => selectorListToBsTypeAst(rule.prelude)
  | _ => [||];

let parse = (~name, ~source) => {
  let ast = Csstree.parse(name, source);
  let typedAst =
    switch (ast) {
    | Csstree.StyleSheet(sheet) =>
      DotTyped.ModuleDeclaration({
        name: DotTyped.Identifier(name),
        declarations:
          sheet.children |. Array.map(ruleToBsTypeAst) |. Array.concatMany,
      })
    | _ => raise(MustBeRoot)
    };

  [|typedAst|];
};
