module VariableDeclaration = {
  let createElement ::name ::module_id ::type_of ::children () =>
    "external" ^ name ^ ": " ^ type_of ^ " = \"\" [@@bs.module \"" ^ module_id ^ "\"];";
};
