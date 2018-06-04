type fileType =
  | CSS
  | FlowDefinition
  | GraphQL
  | Reason
  | Typed
  | TypeScriptDefintion;

type file = {
  type_: fileType,
  name: string,
  source: string,
};

let compile = (file, target) => {
  let typedAst =
    switch (file.type_) {
    | FlowDefinition => ParseFlow.parse(~name=file.name, ~source=file.source)
    | _ => raise(Errors2.Unimplemented)
    };
  switch (target) {
  | FlowDefinition => GenerateFlow.compile(typedAst)
  | _ => raise(Errors2.Unimplemented)
  };
};
