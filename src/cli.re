let load_file fname => {
  let ic = open_in fname;
  let n = in_channel_length ic;
  let s = Bytes.create n;
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s
};

let run_compile fname module_name module_def => {
  let (_, _) = Retyped.Compiler.compile debug::true module_name module_def;
  ()
};

let cmd fname => {
  let module_name = fname;
  let module_def = load_file fname;
  try (run_compile fname module_name module_def) {
  | Retyped.Modulegen.ModulegenDeclError e => print_endline e
  | Retyped.Modulegen.ModulegenStatementError e => print_endline e
  | Retyped.Modulegen.ModulegenTypeError e => print_endline e
  }
};

Arg.parse [] cmd "ReasonablyTyped.native";
