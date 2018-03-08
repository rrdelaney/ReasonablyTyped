exception DiagnosticError(string);

type diagnostic = {
  source: string,
  line: int,
  column: int,
  reason: string
};

let pp = ({line, column, reason, source}) => {
  let offset = Belt.List.make(column, " ") |> String.concat("");
  source
    |> Js.String.split("\n")
    |> Belt.List.ofArray
    |> Belt.List.getExn(_, (line - 1))
    |> (s => Printf.sprintf("%s\n%s^ %s", s, offset, reason))
};

let diagnosticOfFlow = (errors, source) => {
  errors
    |> List.map(
      ((loc, ty)) => {
      let { Loc.line, column } = Loc.(loc.start);
      pp({
        line,
        column,
        source,
        reason: Parse_error.PP.error(ty),
      })
    })
    |> String.concat("\n")
  |> x => DiagnosticError(x)
};

let rec computeColumnAndLine = (~column = 1, ~line = 1, source) => fun
| 0 => (column, line)
| pos => {
  let head = Js.String.get(source, 0);
  let tail = Js.String.sliceToEnd(source, ~from = 1);
  computeColumnAndLine(
    tail,
    pos - 1,
    ~column = head === "\n" ? 1 : column + 1,
    ~line = head === "\n" ? line + 1 : line
  );
};

let diagnosticOfTs = (sourceFile) => {
  let { Typescript.text, parseDiagnostics } = sourceFile;
  parseDiagnostics
  |> Belt.List.ofArray
  |> List.map(d => {
      let {Typescript.start, messageText } = d;
      let (column, line) = computeColumnAndLine(text, start);
      pp({
        line,
        column,
        source: text,
        reason: messageText,
      })
    })
  |> String.concat("\n")
  |> x => DiagnosticError(x)
};
