type diagnostic = {
  .
  "source": string,
  "line": int,
  "column": int,
};

exception Error(array(diagnostic));

let diagnosticOfFlow = (errors, source) =>
  errors
  |> List.map(((loc, _ty)) => {
       let {Loc.line, column} = Loc.(loc.start);
       ({"source": source, "line": line, "column": column}) ;
     })
  |> Array.of_list;

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
  |> Array.map(d => {
       let {Typescript.start} = d;
       let (column, line) = computeColumnAndLine(text, start);
       ({"source": text, "line": line, "column": column}) ;
     });
};
