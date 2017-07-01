open Modulegen.BsDecl;

open Modulegen.BsType;

exception CodegenTypeError string;

exception CodegenConstructorError string;

module Utils = {
  let unquote str => String.sub str 1 (String.length str - 2);
  let normalize_chars =
    String.map (
      fun ch =>
        if (ch == '-' || ch == '$') {
          '_'
        } else {
          ch
        }
    );
  let normalize_keywords =
    fun
    | "type" => "_type"
    | "end" => "_end"
    | "to" => "_to"
    | str => str;
  let normalize_name name => normalize_chars name |> normalize_keywords;
  let to_module_name str => normalize_name (unquote str);
  let rec uniq =
    fun
    | [] => []
    | [h, ...t] => {
        let no_dups = uniq (List.filter (fun x => x != h) t);
        [h, ...no_dups]
      };
  let is_optional (_, type_of) =>
    switch type_of {
    | Optional _ => true
    | _ => false
    };
};

module Uid = {
  let get () => string_of_int 1;
  let uniq prefix => prefix ^ "_" ^ get ();
};

let rec bstype_name =
  fun
  | Regex => "regex"
  | Unit => "unit"
  | Null => "null"
  | Any => Uid.uniq "any"
  | Object _ => "object"
  | AnyObject => "object"
  | AnyFunction => "function"
  | Number => "number"
  | Dict t => "dict_" ^ bstype_name t
  | String => "string"
  | Boolean => "bool"
  | Function _ => "func"
  | Typeof t => "typeof_" ^ bstype_name t
  | Array t => "array_" ^ bstype_name t
  | Tuple types => "tuple_of_" ^ (List.map bstype_name types |> String.concat "_")
  | Named s => String.uncapitalize_ascii s |> Utils.normalize_name
  | Union types => union_types_to_name types
  | Class props => raise (CodegenTypeError "Unable to translate class into type name")
  | Optional t => ""
  | StringLiteral _ =>
    raise (CodegenTypeError "Cannot use string literal outside the context of a union type")
and union_types_to_name types => {
  let is_string_union =
    List.for_all
      (
        fun
        | StringLiteral _ => true
        | _ => false
      )
      types;
  if is_string_union {
    let type_names =
      List.map
        (
          fun
          | StringLiteral s => s
          | _ => raise (CodegenTypeError "Expected a StringLiteral when converting to union type")
        )
        types;
    Render.unionTypeStrings types::type_names ()
  } else {
    let type_names = List.map bstype_name types;
    String.concat "_or_" type_names
  }
};

let rec bstype_to_code =
  fun
  | Regex => "Js.Re.t"
  | Dict t => "Js.Dict.t (" ^ bstype_to_code t ^ ")"
  | Optional t => bstype_to_code t ^ "?"
  | Unit => "unit"
  | Null => "null"
  | Array t => "array " ^ bstype_to_code t
  | Tuple types => Render.tupleType types::(List.map bstype_to_code types) ()
  | Any => "'any"
  | AnyObject => "'any"
  | AnyFunction => "'any"
  | Object props =>
    Render.objectType
      statements::(
        List.map (fun (key, type_of) => (Utils.normalize_name key, bstype_to_code type_of)) props
      )
      ()
  | Number => "float"
  | String => "string"
  | Boolean => "Js.boolean"
  | Named s => String.uncapitalize_ascii s |> Utils.normalize_name
  | Union types => union_types_to_name types
  | Typeof t => raise (CodegenTypeError "Typeof can only operate on variable declarations")
  | StringLiteral _ =>
    raise (CodegenTypeError "Cannot use string literal outside the context of a union type")
  | Function params rt =>
    Render.functionType
      params::(List.map (fun (name, param) => (name, bstype_to_code param)) params)
      has_optional::(List.exists Utils.is_optional params)
      return_type::(bstype_to_code rt)
      ()
  | Class props => {
      let class_types =
        List.map
          (
            fun (key, type_of) => {
              let is_meth =
                switch type_of {
                | Function _ => true
                | _ => false
                };
              (key, bstype_to_code type_of, is_meth)
            }
          )
          props;
      Render.classType types::class_types ()
    };

module Precode = {
  let rec bstype_precode def =>
    switch def {
    | Union types =>
      let types_precode = List.map bstype_precode types |> List.flatten;
      types_precode @ [string_of_union_types def types]
    | Function params rt => List.map (fun (id, t) => bstype_precode t) params |> List.flatten
    | Object types => List.map (fun (id, type_of) => bstype_precode type_of) types |> List.flatten
    | Class types => List.map (fun (id, type_of) => bstype_precode type_of) types |> List.flatten
    | Optional t => bstype_precode t
    | Array t => bstype_precode t
    | Dict t => bstype_precode t
    | _ => [""]
    }
  and string_of_union_types t types => {
    let is_string_union =
      List.for_all
        (
          fun
          | StringLiteral _ => true
          | _ => false
        )
        types;
    if is_string_union {
      ""
    } else {
      let union_name = bstype_name t;
      let union_types =
        List.map
          (fun type_of => (String.capitalize_ascii (bstype_name type_of), bstype_to_code type_of))
          types;
      Render.unionType name::union_name types::union_types ()
    }
  };
  let call_property_precode module_id var_name statements =>
    List.filter (fun (key, type_of) => key == "$$callProperty") statements |>
    List.map (
      fun (key, type_of) =>
        bstype_precode type_of @ [
          Render.variableDeclaration
            name::((var_name == "" ? Utils.to_module_name module_id : var_name) ^ "_apply")
            module_id::(Utils.to_module_name module_id)
            type_of::(bstype_to_code type_of)
            code::var_name
            ()
        ]
    ) |> List.flatten;
  let decl_to_precode module_id =>
    fun
    | VarDecl id type_of =>
      bstype_precode type_of @ (
        switch type_of {
        | Object types => call_property_precode module_id id types
        | _ => []
        }
      )
    | FuncDecl _ type_of => bstype_precode type_of
    | TypeDecl id type_of => {
        let precode = bstype_precode type_of;
        let type_decl =
          Render.typeDeclaration
            name::(String.uncapitalize_ascii id) type_of::(bstype_to_code type_of) ();
        List.append precode [type_decl]
      }
    | ClassDecl _ type_of => bstype_precode type_of
    | InterfaceDecl _ type_of => bstype_precode type_of
    | ExportsDecl type_of =>
      bstype_precode type_of @ (
        switch type_of {
        | Object types => call_property_precode module_id "" types
        | _ => []
        }
      )
    | _ => [""];
  let from_stack stack =>
    switch stack {
    | ModuleDecl id statements =>
      List.map (decl_to_precode id) statements |> List.flatten |> Utils.uniq |> String.concat "\n"
    | TypeDecl _ _ => decl_to_precode "" stack |> String.concat "\n"
    | _ => ""
    };
};

let constructor_type class_name =>
  fun
  | Class props => {
      let constructors = List.find_all (fun (id, _) => id == "constructor") props;
      if (List.length constructors == 0) {
        bstype_to_code (Function [("_", Unit)] (Named class_name))
      } else {
        let (_, cons_type) = List.hd constructors;
        bstype_to_code cons_type
      }
    }
  | _ => raise (CodegenConstructorError "Type has no constructor");

module TypeofTable = {
  type t =
    | Class
    | None
    | NotFound;
  let get key table =>
    try (List.assoc key table) {
    | Not_found => NotFound
    };
  let create statements =>
    List.map
      (
        fun
        | VarDecl id type_of => (id, None)
        | ClassDecl id type_of => (id, Class)
        | _ => ("", None)
      )
      statements |>
    List.filter (fun (key, _) => key != "");
  let show table => {
    print_endline "Types:";
    List.iter
      (
        fun (id, typeof) =>
          print_endline (
            "| " ^
            id ^
            " => " ^ (
              switch typeof {
              | Class => "Class"
              | None => "None"
              | NotFound => "NotFound"
              }
            )
          )
      )
      table;
    print_newline ()
  };
};

let rec declaration_to_code module_id types =>
  fun
  | VarDecl id type_of =>
    Render.variableDeclaration
      name::(Utils.normalize_name id)
      module_id::(Utils.unquote module_id)
      type_of::(bstype_to_code type_of)
      ()
  | FuncDecl id type_of =>
    Render.variableDeclaration
      name::(Utils.normalize_name id)
      module_id::(Utils.unquote module_id)
      type_of::(bstype_to_code type_of)
      ()
  | ExportsDecl type_of =>
    switch type_of {
    | Typeof (Named t) =>
      TypeofTable.(
        switch (TypeofTable.get t types) {
        | Class =>
          Render.alias
            name::(Utils.to_module_name module_id) value::("create_" ^ bstype_to_code (Named t)) ()
        | None => raise (CodegenTypeError "typeof can only operate on classes")
        | NotFound => raise (CodegenTypeError ("Unknown identifier: " ^ t))
        }
      )
    | _ =>
      Render.variableDeclaration
        name::(Utils.to_module_name module_id)
        type_of::(bstype_to_code type_of)
        module_id::(Utils.unquote module_id)
        is_exports::true
        ()
    }
  | ModuleDecl id statements =>
    Render.moduleDeclaration
      name::id statements::(List.map (declaration_to_code id types) statements) ()
  | TypeDecl id type_of => ""
  | ClassDecl id type_of => {
      let class_name = String.uncapitalize_ascii id;
      let ctor_type = constructor_type class_name type_of;
      let class_type = bstype_to_code type_of;
      Render.classDeclaration
        name::class_name
        exported_as::id
        module_id::(Utils.unquote module_id)
        ::class_type
        ::ctor_type
        ()
    }
  | InterfaceDecl id type_of =>
    Render.typeDeclaration
      name::(String.uncapitalize_ascii id) type_of::(bstype_to_code type_of) ();

let stack_to_code stack =>
  switch stack {
  | ModuleDecl id statements =>
    let typeof_table = TypeofTable.create statements;
    /* TypeofTable.show typeof_table; */
    Some (
      Utils.to_module_name id,
      Precode.from_stack stack ^
      String.concat "\n" (List.map (declaration_to_code id typeof_table) statements)
    )
  | TypeDecl _ _ => Some ("", Precode.from_stack stack ^ declaration_to_code "" [] stack)
  | _ => None
  };
