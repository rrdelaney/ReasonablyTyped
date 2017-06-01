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

module ClassDeclaration = {
  let createElement ::name ::exported_as ::module_id ::class_type ::ctor_type ::children () =>
    "type " ^
    name ^
    " = " ^
    class_type ^
    ";\n" ^
    "external create_" ^
    name ^
    " : " ^
    ctor_type ^ " = \"" ^ exported_as ^ "\" [@@bs.new] [@@bs.module \"" ^ module_id ^ "\"];";
};

module ObjectType = {
  let createElement ::statements ::children () =>
    "Js.t {. " ^
    (List.map (fun (key, type_of) => key ^ ": " ^ type_of) statements |> String.concat ", ") ^ " }";
};

module FunctionType = {
  let createElement ::params ::has_optional ::return_type ::children () =>
    (List.map (fun (name, param_type) => name ^ "::" ^ param_type) params |> String.concat " => ") ^
    " => " ^ (has_optional ? "unit => " : "") ^ return_type;
};

module TupleType = {
  let createElement ::types ::children () => "(" ^ String.concat ", " types ^ ")";
};
