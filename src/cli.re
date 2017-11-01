let load_file = (file_name) => {
  let input_channel = open_in(file_name);
  let input_bytes = in_channel_length(input_channel);
  let input_stream = Bytes.create(input_bytes);
  really_input(input_channel, input_stream, 0, input_bytes);
  close_in(input_channel);
  Bytes.to_string(input_stream)
};

let run_compile = (module_name, module_def) => {
  let (_, _) = Retyped.Compiler.compile(~debug=true, module_name, module_def);
  ()
};

let cmd = (file_name) => {
  let module_name = file_name;
  let module_def = load_file(file_name);
  try (run_compile(module_name, module_def)) {
  | Retyped.Modulegen.ModulegenDeclError(e) => print_endline(e)
  | Retyped.Modulegen.ModulegenStatementError(e) => print_endline(e)
  | Retyped.Modulegen.ModulegenTypeError(e) => print_endline(e)
  }
};

Arg.parse([], cmd, "ReasonablyTyped.native");
