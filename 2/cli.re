exception UnrecognizedInput(string);

module Args = {
  [@bs.deriving abstract]
  type t = {
    [@bs.optional]
    input: string,
    [@bs.optional]
    output: string,
    args: array(string),
  };
  external decode : Js.Json.t => t = "%identity";
};

let inferInputTypeFromFileExtension = filename =>
  switch (filename) {
  | _ when Js.String.endsWith(".d.ts", filename) => Compiler2.TypeScriptDefintion
  | _ when Js.String.endsWith(".js", filename) => Compiler2.FlowDefinition
  | _ when Js.String.endsWith(".css", filename) => Compiler2.CSS
  | _ when Js.String.endsWith(".gql", filename) => Compiler2.GraphQL
  | _ when Js.String.endsWith(".graphql", filename) => Compiler2.GraphQL
  | _ when Js.String.endsWith(".typed.js", filename) => Compiler2.Typed
  | _ when Js.String.endsWith(".typed", filename) => Compiler2.Typed
  | _ => raise(UnrecognizedInput(filename))
  };

let optionToFileType = input =>
  switch (input) {
  | "dts" => Compiler2.TypeScriptDefintion
  | "flow" => Compiler2.FlowDefinition
  | "css" => Compiler2.CSS
  | "graphql" => Compiler2.GraphQL
  | "typed" => Compiler2.Typed
  | _ => raise(UnrecognizedInput(input))
  };

let program =
  Commander.program
  |. Commander.version("2.0.0")
  |. Commander.usage("[options] <file ...>")
  |. Commander.option("--input [type]", "Input file type")
  |. Commander.option("--output [type]", "Output file type")
  |. Commander.parse(Commander.Process.argv)
  |. Args.decode;

let filename =
  program |. Args.args |. Belt.Array.get(0) |. Belt.Option.getExn;

let inputType =
  program
  |. Args.input
  |. Belt.Option.mapWithDefault(
       inferInputTypeFromFileExtension(filename),
       optionToFileType,
     );

let outputType =
  program
  |. Args.output
  |. Belt.Option.map(optionToFileType)
  |. Belt.Option.getWithDefault(Compiler2.Typed);

let inputSource = Node.Fs.readFileAsUtf8Sync(filename);

let inputFile =
  Compiler2.{type_: inputType, name: filename, source: inputSource};

let output = Compiler2.compile(inputFile, outputType);

Js.log(output);
