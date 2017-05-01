if (Array.length Sys.argv < 2) {
  print_endline "Usage:\n  cmd <file_name> <file_contents>\n";
  exit 0
};

let module_name = Loc.SourceFile Sys.argv.(1);

let module_def = Sys.argv.(2);

let (module_id, flow_code, bs_code) = Retyped.compile module_name module_def;

print_endline ("Module " ^ module_id);

print_newline ();

print_endline "=== Flow Definition ===";

print_endline flow_code;

print_newline ();

print_endline "=== Bucklescript Definition ===";

print_endline ("/* Module " ^ module_id ^ " */");

print_endline bs_code;
