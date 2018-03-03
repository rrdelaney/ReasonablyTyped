let wrapParens = l => l == [] ? "" : "(" ++ String.concat(", ", l) ++ ")";

let applyArgs = (name, l) => name ++ wrapParens(l);

type lambdaArg =
  | Labelled(string, bool)
  | Unlabelled(string);

let lambda = (l, body) => {
  let arg = a =>
    switch a {
    | Labelled(label, optional) => "~" ++ label ++ (optional ? "=?" : "")
    | Unlabelled(name) => name
    };
  wrapParens(List.map(arg, l)) ++ " => " ++ body;
};

let quote = x => "\"" ++ x ++ "\"";

let bsModule = name => {
  let quotedName = name == "" ? name : " " ++ quote(name);
  "[@bs.module" ++ quotedName ++ "]";
};

let jsObject = l =>
  "{"
  ++ (List.map(((k, v)) => quote(k) ++ ": " ++ v, l) |> String.concat(", "))
  ++ "}";

let variableDeclaration =
    (
      ~name,
      ~module_id,
      ~type_of,
      ~is_exports=false,
      ~splice=false,
      ~code="",
      ()
    ) =>
  if (is_exports) {
    bsModule("")
    ++ " external "
    ++ name
    ++ " : "
    ++ type_of
    ++ " = \""
    ++ module_id
    ++ "\" ;\n";
  } else {
    bsModule(module_id)
    ++ (splice ? "[@bs.splice]" : "")
    ++ "external "
    ++ name
    ++ " : "
    ++ type_of
    ++ " = \""
    ++ code
    ++ "\""
    ++ ";\n";
  };

let moduleDeclaration = (~name, ~statements, ()) =>
  "module " ++ name ++ " = {\n" ++ String.concat("\n  ", statements) ++ "\n};";

let classDeclaration =
    (
      ~name,
      ~exported_as,
      ~module_id,
      ~class_type,
      ~ctor_type,
      ~type_params,
      ()
    ) =>
  "module "
  ++ name
  ++ " = {\n  type "
  ++ applyArgs("t", type_params)
  ++ " = "
  ++ class_type
  ++ ";\n  "
  ++ "[@bs.new] "
  ++ bsModule(module_id)
  ++ " external make : "
  ++ ctor_type
  ++ " = \""
  ++ exported_as
  ++ "\""
  ++ ";\n};";

let typeDeclaration = (~name, ~type_of, ~type_params, ()) =>
  "type " ++ applyArgs(name, type_params) ++ " = " ++ type_of ++ ";";

let objectType = (~statements, ()) =>
  "{. "
  ++ (
    List.filter(
      ((key, _type_of, _optional)) => key != "__callProperty",
      statements
    )
    |> List.map(((key, type_of, optional)) =>
         "\""
         ++ key
         ++ "\""
         ++ ": "
         ++ (optional ? "Js.Nullable.t(" ++ type_of ++ ")" : type_of)
       )
    |> String.concat(", ")
  )
  ++ " }";

let functionType =
    (~formal_params, ~rest_param, ~has_optional, ~return_type, ()) => {
  let print = ((name, param_type)) =>
    if (name != "") {
      "(~" ++ name ++ ":" ++ param_type ++ ")";
    } else {
      param_type;
    };
  let formalCode =
    List.map(print, formal_params)
    |> String.concat(" => ")
    |> (
      it =>
        switch it {
        | "" => ""
        | s => s ++ " => "
        }
    );
  let optUnit = has_optional ? "unit => " : "";
  let restCode =
    switch rest_param {
    | Some(param) => print(param) ++ " => "
    | None => ""
    };
  formalCode ++ optUnit ++ restCode ++ return_type;
};

let tupleType = (~types, ()) => "(" ++ String.concat(", ", types) ++ ")";

let unionTypeStrings = (~types, ()) =>
  "(["
  ++ (List.map(type_name => "`" ++ type_name, types) |> String.concat(" | "))
  ++ "] [@bs.string])";

let unionType = (~name, ~types, ()) =>
  "type union_of_"
  ++ name
  ++ " = "
  ++ (
    List.map(
      ((type_name, type_of)) => "\n| " ++ type_name ++ " (" ++ type_of ++ ")",
      types
    )
    |> String.concat("")
  )
  ++ ";\n\ntype "
  ++ name
  ++ ";\n\nexternal "
  ++ name
  ++ " : union_of_"
  ++ name
  ++ " => "
  ++ name
  ++ " = \"Array.prototype.shift.call\" [@@bs.val];\n";

let inlineUnion = (~types, ()) =>
  "([@bs.unwrap] ["
  ++ (
    List.map(
      ((type_name, type_of)) => "`" ++ type_name ++ "(" ++ type_of ++ ")",
      types
    )
    |> String.concat(" | ")
  )
  ++ "])";

let classType = (~types, ()) =>
  "{. "
  ++ (
    List.filter(((key, _, _type_of, _)) => key != "constructor", types)
    |> List.map(((key, type_params, type_of, is_meth)) =>
         "\""
         ++ key
         ++ "\""
         ++ ": "
         ++ (
           switch type_params {
           | [] => ""
           | p => String.concat(" ", p) ++ " . "
           }
         )
         ++ (is_meth ? "[@bs.meth]" : "")
         ++ "("
         ++ type_of
         ++ ")"
       )
    |> String.concat(", ")
  )
  ++ "}";

let alias = (~name, ~value) => "let " ++ name ++ " = " ++ value ++ ";\n";

let react_component =
    (
      ~define_module,
      ~module_name,
      ~component_name,
      ~js_name,
      ~props,
      ~props_type_string
    ) =>
  (define_module ? "\nmodule " ++ component_name ++ " = {\n" : "\n")
  ++ "  "
  ++ bsModule(module_name)
  ++ "\n"
  ++ "  external "
  ++ String.uncapitalize(component_name)
  ++ " : ReasonReact.reactClass = \""
  ++ js_name
  ++ "\""
  ++ ";\n"
  ++ "  let make = "
  ++ lambda(
       List.map(
         ((name, _js, _t, optional, _is_bool)) => Labelled(name, optional),
         props
       )
       @ [Unlabelled("children")],
       "{"
       ++ "\n    let props: "
       ++ props_type_string
       ++ " = "
       ++ jsObject(
            {
              let wrapArg = (~optional, ~is_bool, x) =>
                switch (optional, is_bool) {
                | (true, false) => applyArgs("Js.Nullable.from_opt", [x])
                | (true, true) =>
                  let bind = (v, f) =>
                    applyArgs(
                      "Js.Nullable.bind",
                      [
                        v,
                        "[@bs]"
                        ++ lambda([Unlabelled("x")], applyArgs(f, ["x"]))
                      ]
                    );
                  bind(
                    applyArgs("Js.Nullable.from_opt", [x]),
                    "Js.Boolean.to_js_boolean"
                  );
                | (false, true) => applyArgs("Js.Boolean.to_js_boolean", [x])
                | (false, false) => x
                };
              List.map(
                ((name, js, _t, optional, is_bool)) => (
                  js,
                  wrapArg(~optional, ~is_bool, name)
                ),
                props
              );
            }
          )
       ++ ";"
       ++ "\n    "
       ++ applyArgs(
            "ReasonReact.wrapJsForReason",
            [
              "~reactClass=" ++ String.uncapitalize(component_name),
              "~props",
              "children"
            ]
          )
       ++ ";\n"
       ++ "  };"
     )
  ++ (define_module ? "\n};" : "");
