let sourceFile =
  Typescript.Internal.createSourceFile(
    "t.ts",
    "declare function add(x: number, y: number): number",
    Typescript.Internal.ScriptTarget.es2015,
    Js.false_
  );

let root = Typescript.Decoder.decode(sourceFile);
