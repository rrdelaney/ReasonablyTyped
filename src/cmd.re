let debug = ref false;

let usage_msg = "retyped Options available:";

let speclist = [("-debug", Arg.Set debug, "Enables debugging mode")];

let load_file fname => {
  let ic = open_in fname;
  let n = in_channel_length ic;
  let s = String.create n;
  really_input ic s 0 n;
  close_in ic;
  s
};

let write_file fname contents => {
  let oc = open_out fname;
  output_string oc contents;
  close_out oc
};

let replace_filename fname module_id => {
  let last_slash = String.rindex fname '/';
  let path = String.sub fname 0 last_slash;
  path ^ "/" ^ module_id ^ ".re"
};

let run_compile fname module_name module_def => {
  let (module_id, flow_code, bs_code) = Retyped.compile module_name module_def;
  if !debug {
    print_endline ("Module " ^ module_id);
    print_newline ();
    print_endline "=== Flow Definition ===";
    print_endline flow_code;
    print_newline ();
    print_endline "=== Bucklescript Definition ===";
    print_endline ("/* Module " ^ module_id ^ " */");
    print_endline bs_code
  } else {
    let module_filename = replace_filename fname module_id;
    write_file module_filename bs_code
  }
};

let cmd fname => {
  let module_name = Loc.SourceFile fname;
  let module_def = load_file fname;
  try (run_compile fname module_name module_def) {
  | Modulegen.ModulegenDeclError e => print_endline e
  | Modulegen.ModulegenStatementError e => print_endline e
  | Modulegen.ModulegenTypeError e => print_endline e
  }
};

Arg.parse speclist cmd usage_msg;
