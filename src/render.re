module VariableDeclaration = {
  let createElement ::name ::module_id ::type_of ::is_exports=false ::children () =>
    if is_exports {
      "external " ^ module_id ^ " : " ^ type_of ^ " = \"" ^ module_id ^ "\" [@@bs.module];"
    } else {
      "external " ^ name ^ " : " ^ type_of ^ " = \"\" [@@bs.module \"" ^ module_id ^ "\"];"
    };
};

module ModuleDeclaration = {
  let createElement ::name ::statements ::children () =>
    "module " ^ name ^ " = {\n" ^ String.concat "\n  " statements ^ "\n};";
};
