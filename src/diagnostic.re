exception Error(string);

module BabelCodeFrame = {
  type babelCodeFrameOptions = {
    .
    "highlightCode": Js.undefined(Js.boolean),
    "linesBelow": Js.undefined(int),
    "linesAbove": Js.undefined(int),
    "forceColor": Js.undefined(Js.boolean),
  };
  [@bs.module]
  external default :
    (
      ~rawLines: string,
      ~lineNumber: int,
      ~colNumber: int,
      ~options: babelCodeFrameOptions=?,
      unit
    ) =>
    string =
    "babel-code-frame";
};

let diagnosticOfFlow = (errors, source) =>
  errors
  |> List.map(((loc, _ty)) => {
       let {Loc.line, column} = Loc.(loc.start);
       BabelCodeFrame.default(
         ~rawLines=source,
         ~lineNumber=line,
         ~colNumber=column,
         ~options={
           "highlightCode": Js.Undefined.return(Js.true_),
           "linesBelow": Js.Undefined.empty,
           "linesAbove": Js.Undefined.empty,
           "forceColor": Js.Undefined.empty,
         },
         (),
       );
     })
  |> String.concat("\n----------------\n");

let rec computeColumnAndLine = (~column=1, ~line=1, ~pre="", source) =>
  fun
  | 0 => (column, line)
  | pos => {
      let head = Js.String.get(source, 0);
      let tail = Js.String.sliceToEnd(source, ~from=1);
      computeColumnAndLine(
        tail,
        pos - 1,
        ~pre=head,
        ~column=pre === "\n" ? 1 : column + 1,
        ~line=pre === "\n" ? line + 1 : line,
      );
    };

let diagnosticOfTs = sourceFile => {
  let {Typescript.text, parseDiagnostics} = sourceFile;
  parseDiagnostics
  |> Belt.List.ofArray
  |> List.map(d => {
       let {Typescript.start} = d;
       let (column, line) = computeColumnAndLine(text, start);
       BabelCodeFrame.default(
         ~rawLines=text,
         ~lineNumber=line,
         ~colNumber=column,
         ~options={
           "highlightCode": Js.Undefined.return(Js.true_),
           "linesBelow": Js.Undefined.empty,
           "linesAbove": Js.Undefined.empty,
           "forceColor": Js.Undefined.empty,
         },
         (),
       );
     })
  |> String.concat("\n")
  |> (x => Error(x));
};
