exception Error(string);

type diagnostic = {
  source: string,
  line: int,
  column: int,
  reason: string
};

let pp = ({line, column, reason, source}) => {
  let lineNumberOfIndex = fun
  | i when(i < 10) => Printf.sprintf("  %d", i)
  | i when(i < 100) => Printf.sprintf(" %d", i)
  | i => Printf.sprintf("%d", i)
  ;

  let offset = Belt.List.make(column, " ") |> String.concat("");
  source
  |> Js.String.split("\n")
  |> Belt.List.ofArray
  |> Belt.List.mapWithIndex(_, (i, code) => Printf.sprintf("%s | %s", lineNumberOfIndex(i), code))
  |> (
    codes =>
      switch (Belt.List.splitAt(codes, line)) {
      | Some((xs, ys)) =>
        Belt.List.concat(
          xs,
          Belt.List.add(ys, Printf.sprintf("\027[31m      %s^ %s\027[0m", offset, reason))
        )
      | None => []
      }
  )
  |> String.concat("\n");
};

let diagnosticOfFlow = (errors, source) =>
  errors
  |> List.map(((loc, ty)) => {
       let {Loc.line, column} = Loc.(loc.start);
       pp({line, column, source, reason: Parse_error.PP.error(ty)});
     })
  |> String.concat("\n----------------\n")
  |> (x => Error(x));

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
        ~line=pre === "\n" ? line + 1 : line
      );
    };

let diagnosticOfTs = sourceFile => {
  let {Typescript.text, parseDiagnostics} = sourceFile;
  parseDiagnostics
  |> Belt.List.ofArray
  |> List.map(d => {
       let {Typescript.start, messageText} = d;
       let (column, line) = computeColumnAndLine(text, start);
       pp({line, column: column - 1, source: text, reason: messageText});
     })
  |> String.concat("\n")
  |> (x => Error(x));
};
