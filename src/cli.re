module Run (Io: Io.S) => {
  let debug = ref false;
  let usage_msg = "retyped Options available:";
  let speclist = [("-debug", Arg.Set debug, "Enables debugging mode")];

  /** */
  let replace_filename fname module_id => {
    let last_slash = String.rindex fname '/';
    let path = String.sub fname 0 last_slash;
    path ^ "/" ^ module_id ^ ".re"
  };

  /** */
  let run_compile fname module_name module_def => {
    let (module_id, flow_code, bs_code) = Retyped.Compiler.compile module_name module_def;
    if !debug {
      print_endline (">>> Module " ^ module_id ^ " <<<");
      print_newline ();
      print_endline "=== Flow Definition ===";
      print_endline flow_code;
      print_newline ();
      print_endline "=== Bucklescript Definition ===";
      print_endline ("/* Module " ^ module_id ^ " */");
      print_endline bs_code
    } else {
      let module_filename = replace_filename fname module_id;
      Io.write_file module_filename bs_code
    }
  };

  /** */
  let cmd fname => {
    let module_name = fname;
    let module_def = Io.read_file fname;
    try (run_compile fname module_name module_def) {
    | Retyped.Modulegen.ModulegenDeclError e => print_endline e
    | Retyped.Modulegen.ModulegenStatementError e => print_endline e
    | Retyped.Modulegen.ModulegenTypeError e => print_endline e
    }
  };

  /** */
  Arg.parse speclist cmd usage_msg;
};
