open Modulegen.JsDecl;

open Module_gen.JsType;

let stack_to_code =
  fun
  | ModuleDecl id statements =>
    Some (id, String.concat "\n" (List.map Modulegen.show_decl statements))
  | _ => None;
