let selectorChildrenToBsTypeAst =
  fun
  | Csstree.ClassSelector(classSelector) => [
      BsTypeAst.VarDecl(classSelector.name, BsTypeAst.String),
    ]
  | _ => [];

let selectorToBsTypeAst =
  fun
  | Csstree.Selector(selector) =>
    selector.children
    |> Array.to_list
    |> List.map(selectorChildrenToBsTypeAst)
    |> List.flatten
  | _ => [];

let selectorListToBsTypeAst =
  fun
  | Csstree.SelectorList(selectorList) =>
    selectorList.children
    |> Array.to_list
    |> List.map(selectorToBsTypeAst)
    |> List.flatten
  | _ => [];

let ruleToBsTypeAst =
  fun
  | Csstree.Rule(rule) => selectorListToBsTypeAst(rule.prelude)
  | _ => [];

let cssAstToBsTypeAst = fileName =>
  fun
  | Csstree.StyleSheet(sheet) =>
    BsTypeAst.ModuleDecl(
      fileName,
      sheet.children
      |> Array.to_list
      |> List.map(ruleToBsTypeAst)
      |> List.flatten,
    )
  | _ => BsTypeAst.Noop;
