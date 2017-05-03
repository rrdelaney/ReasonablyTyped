/*if (Array.length Sys.argv < 2) {
    print_endline "Usage:\n  cmd <file_name> <file_contents>\n";
    exit 0
  };
  */
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

let cmd fname => {
  let module_name = Loc.SourceFile fname;
  let module_def = load_file fname;
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
    print_endline ("/* Module " ^ module_id ^ " */");
    print_endline bs_code
  }
};

Arg.parse speclist cmd usage_msg;
