let unquote = str => {
  let has_start_quote = str.[0] == '"' || str.[0] == '\'';
  let has_end_quote =
    str.[String.length(str) - 1] == '"' || str.[String.length(str) - 1] == '\'';
  if (has_start_quote && has_end_quote) {
    String.sub(str, 1, String.length(str) - 2);
  } else {
    str;
  };
};

let normalize_chars = String.map(ch => ch == '-' || ch == '$' ? '_' : ch);

let normalize_keywords =
  fun
  | "type" => "_type"
  | "end" => "_end"
  | "to" => "_to"
  | str => str;

let normalize_name = name => normalize_chars(name) |> normalize_keywords;

let import_module_name = name => normalize_name(name) |> String.capitalize;

let to_module_name = str => normalize_name(unquote(str));

let to_type_param = str => "'" ++ String.uncapitalize(str) |> normalize_name;

let rec split = (sep, str, acc) => {
  open String;
  let len = length(str);
  let first_index =
    try (Some(index(str, sep))) {
    | Not_found => None
    };
  switch first_index {
  | None => List.append(acc, [str])
  | Some(i) =>
    let beginning = min(len, i + 1);
    split(
      sep,
      sub(str, beginning, len - beginning),
      List.append(acc, [sub(str, 0, max(0, beginning - 1))])
    );
  };
};

let rec uniq =
  fun
  | [] => []
  | [h, ...t] => {
      let no_dups = uniq(List.filter(x => x != h, t));
      [h, ...no_dups];
    };

module Is = {
  let optional = type_of =>
    switch type_of {
    | BsTypeAst.Optional(_) => true
    | _ => false
    };
  let type_param = (params, t) => List.exists(p => p == t, params);
  let class_type = (t, table) =>
    switch (Typetable.get(t, table)) {
    | Class => true
    | _ => false
    };
  let string_union =
    List.for_all(
      fun
      | BsTypeAst.StringLiteral(_) => true
      | _ => false
    );
  let react_component =
    fun
    | BsTypeAst.Class(Some(Named(_params, "React$Component", None)), _props) =>
      true
    | BsTypeAst.Class(Some(Named(_params, "Component", Some("React"))), _props) =>
      true
    | BsTypeAst.Named(_params, "React$ComponentType", None) => true
    | BsTypeAst.Named(_params, "ComponentType", Some("React")) => true
    | BsTypeAst.Named(_params, "React$StatelessFunctionalComponent", None) =>
      true
    | BsTypeAst.Named(_params, "StatelessFunctionalComponent", Some("React")) =>
      true
    | BsTypeAst.Function({
        returnType: Named(_ntype_params, "React$Element", None)
      }) =>
      true
    | Function({returnType: Named(_ntype_params, "Element", Some("React"))}) =>
      true
    | _ => false;
};

module React = {
  let extract_component_type = component =>
    switch component {
    | BsTypeAst.Class(
        Some(Named([props, ..._params], "React$Component", None)),
        _props
      ) => props
    | BsTypeAst.Class(
        Some(Named([props, ..._params], "Component", Some("React"))),
        _props
      ) => props
    | BsTypeAst.Named([props, ..._params], "React$ComponentType", None) => props
    | BsTypeAst.Named([props, ..._params], "ComponentType", Some("React")) => props
    | BsTypeAst.Named(
        [props, ..._params],
        "React$StatelessFunctionalComponent",
        None
      ) => props
    | BsTypeAst.Named(
        [props, ..._params],
        "StatelessFunctionalComponent",
        Some("React")
      ) => props
    | BsTypeAst.Named([], _, None) => component
    | BsTypeAst.Function({
        formalParams: [(_param_name, props), ..._params],
        returnType: Named(_ntype_params, "React$Element", None)
      }) => props
    | BsTypeAst.Function({
        formalParams: [(_param_name, props), ..._params],
        returnType: Named(_ntype_params, "Element", Some("React"))
      }) => props
    | BsTypeAst.Object(_) => component
    | _ => Unit
    };
  let extract_props = (type_table, component) => {
    let component_type = extract_component_type(component);
    switch component_type {
    | Named(_, type_name, _) =>
      switch (Typetable.get(type_name, type_table)) {
      | Type(t) => t
      | _ => component_type
      }
    | _ => component_type
    };
  };
};

let walk = replacer => {
  open BsTypeAst;
  let or_value = (transform, t) =>
    switch (transform(t)) {
    | Some(result) => result
    | None => t
    };
  let rec walk_type = (~recurse=true, walkable) =>
    switch walkable {
    | Function(func) as ft =>
      switch (replacer(ft)) {
      | Some(new_t) when recurse => walk_type(~recurse=false, new_t)
      | _ =>
        Function({
          ...func,
          formalParams:
            List.map(((name, t)) => (name, walk_type(t)), func.formalParams),
          restParam:
            switch func.restParam {
            | Some((name, t)) => Some((name, walk_type(t)))
            | None => None
            },
          returnType: walk_type(func.returnType)
        })
      }
    | Object(fields) as ot =>
      switch (replacer(ot)) {
      | Some(new_t) when recurse => walk_type(~recurse=false, new_t)
      | _ =>
        Object(
          List.map(
            ((name, t, optional)) => (name, walk_type(t), optional),
            fields
          )
        )
      }
    | Class(extends, fields) as ct =>
      switch (replacer(ct)) {
      | Some(new_t) when recurse => walk_type(~recurse=false, new_t)
      | _ =>
        Class(
          switch extends {
          | Some(t) => Some(walk_type(t))
          | None => None
          },
          List.map(((name, t)) => (name, walk_type(t)), fields)
        )
      }
    | Dict(t) as dt =>
      switch (replacer(dt)) {
      | Some(new_t) when recurse => walk_type(~recurse=false, new_t)
      | _ => Dict(walk_type(t))
      }
    | Array(t) as at =>
      switch (replacer(at)) {
      | Some(new_t) when recurse => walk_type(~recurse=false, new_t)
      | _ => Array(walk_type(t))
      }
    | Union(types) as ut =>
      switch (replacer(ut)) {
      | Some(new_t) when recurse => walk_type(~recurse=false, new_t)
      | _ => Union(List.map(walk_type, types))
      }
    | Tuple(types) as tt =>
      switch (replacer(tt)) {
      | Some(new_t) when recurse => walk_type(~recurse=false, new_t)
      | _ => Tuple(List.map(walk_type, types))
      }
    | Typeof(t) as tt =>
      switch (replacer(tt)) {
      | Some(new_t) when recurse => walk_type(~recurse=false, new_t)
      | _ => Typeof(walk_type(t))
      }
    | Named(types, value, module_name) as nt =>
      switch (replacer(nt)) {
      | Some(new_t) when recurse => walk_type(~recurse=false, new_t)
      | _ => Named(List.map(walk_type, types), value, module_name)
      }
    | Optional(t) as ot =>
      switch (replacer(ot)) {
      | Some(new_t) when recurse => walk_type(~recurse=false, new_t)
      | _ => Optional(walk_type(t))
      }
    | Promise(t) as pt =>
      switch (replacer(pt)) {
      | Some(new_t) when recurse => walk_type(~recurse=false, new_t)
      | _ => Promise(walk_type(t))
      }
    | t => or_value(replacer, t)
    };
  fun
  | VarDecl(name, t) => VarDecl(name, walk_type(t))
  | FuncDecl(name, t) => FuncDecl(name, walk_type(t))
  | TypeDecl(name, params, t) => TypeDecl(name, params, walk_type(t))
  | ClassDecl(name, params, t) => ClassDecl(name, params, walk_type(t))
  | s => s;
};
