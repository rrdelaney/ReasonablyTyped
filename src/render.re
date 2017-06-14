let variableDeclaration ::name ::module_id ::type_of ::is_exports=false () =>
  if is_exports {
    "external " ^ name ^ " : " ^ type_of ^ " = \"" ^ module_id ^ "\" [@@bs.module];"
  } else {
    "external " ^ name ^ " : " ^ type_of ^ " = \"\" [@@bs.module \"" ^ module_id ^ "\"];"
  };

let moduleDeclaration ::name ::statements () =>
  "module " ^ name ^ " = {\n" ^ String.concat "\n  " statements ^ "\n};";

let classDeclaration ::name ::exported_as ::module_id ::class_type ::ctor_type () =>
  "type " ^
  name ^
  " = " ^
  class_type ^
  ";\n" ^
  "external create_" ^
  name ^
  " : " ^ ctor_type ^ " = \"" ^ exported_as ^ "\" [@@bs.new] [@@bs.module \"" ^ module_id ^ "\"];";

let typeDeclaration ::name ::type_of () => "type " ^ name ^ " = " ^ type_of ^ ";";

let objectType ::statements () =>
  "Js.t {. " ^
  (List.map (fun (key, type_of) => key ^ ": " ^ type_of) statements |> String.concat ", ") ^ " }";

let functionType ::params ::has_optional ::return_type () =>
  (
    List.map
      (
        fun (name, param_type) =>
          if (name != "") {
            name ^ "::" ^ param_type
          } else {
            param_type
          }
      )
      params |>
    String.concat " => "
  ) ^
  " => " ^ (has_optional ? "unit => " : "") ^ return_type;

let tupleType ::types () => "(" ^ String.concat ", " types ^ ")";

let unionType ::name ::types () =>
  "type " ^
  name ^
  " = " ^
  (
    List.map (fun (type_name, type_of) => "\n| " ^ type_name ^ " (" ^ type_of ^ ")") types |>
    String.concat ""
  ) ^ ";\n";

let classType ::types () =>
  "Js.t {. " ^
  (
    List.filter (fun (key, type_of, _) => key != "constructor") types |>
    List.map (fun (key, type_of, is_meth) => key ^ ": " ^ type_of ^ (is_meth ? "[@bs.meth]" : "")) |>
    String.concat ", "
  ) ^ "}";
