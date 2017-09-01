let load_file fname => {
  let ic = open_in fname;
  let n = in_channel_length ic;
  let s = Bytes.create n;
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s
};

let run_compile fname module_name module_def => {
  let (module_id, flow_code, bs_code) =
    Retyped.Compiler.compile module_name module_def;
  print_endline "=== Flow Definition ===";
  print_endline flow_code;
  print_newline ();
  print_endline "=== Bucklescript Definition ===";
  print_endline ("/* Module " ^ module_id ^ " */");
  print_endline bs_code
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
