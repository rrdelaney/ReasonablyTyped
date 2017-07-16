open Modulegen.BsDecl;

open Modulegen.BsType;

exception CodegenTypeError string;

exception CodegenConstructorError string;

let rec bstype_name =
  fun
  | Regex => "regex"
  | Unit => "unit"
  | Null => "null"
  | Any => Genutils.Uid.uniq "any"
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
  | Named type_params s => String.uncapitalize_ascii s |> Genutils.normalize_name
  | Union types => union_types_to_name types
  | Class props => raise (CodegenTypeError "Unable to translate class into type name")
  | Optional t => bstype_name t
  | Promise t => "promise_" ^ bstype_name t
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

type context = {type_params: list string};

let intctx = {type_params: []};

let rec bstype_to_code ::ctx=intctx =>
  fun
  | Regex => "Js.Re.t"
  | Dict t => "Js.Dict.t (" ^ bstype_to_code ::ctx t ^ ")"
  | Optional t => bstype_to_code ::ctx t
  | Unit => "unit"
  | Null => "null"
  | Array t => "array (" ^ bstype_to_code ::ctx t ^ ")"
  | Tuple types => Render.tupleType types::(List.map (bstype_to_code ::ctx) types) ()
  | Any => "'any"
  | AnyObject => "'any"
  | AnyFunction => "'any"
  | Object props =>
    Render.objectType
      statements::(
        List.map
          (fun (key, type_of) => (Genutils.normalize_name key, bstype_to_code ::ctx type_of)) props
      )
      ()
  | Number => "float"
  | String => "string"
  | Boolean => "Js.boolean"
  | Named type_params s =>
    (Genutils.is_type_param ctx.type_params s ? "'" : "") ^
    (String.uncapitalize_ascii s |> Genutils.normalize_name) ^
    " " ^ (List.map bstype_to_code type_params |> String.concat " ")
  | Union types => union_types_to_name types
  | Typeof t => raise (CodegenTypeError "Typeof can only operate on variable declarations")
  | Promise t => "Js_promise.t (" ^ bstype_to_code ::ctx t ^ ")"
  | StringLiteral _ =>
    raise (CodegenTypeError "Cannot use string literal outside the context of a union type")
  | Function type_params params rest_param rt => {
      let ctx = {type_params: type_params @ ctx.type_params};
      let print (name, param) => (
        name,
        bstype_to_code ::ctx param ^ (Genutils.is_optional param ? "?" : "")
      );
      Render.functionType
        formal_params::(List.map print params)
        rest_param::(
          switch rest_param {
          | Some p => Some (print p)
          | None => None
          }
        )
        has_optional::(List.exists (fun (name, t) => Genutils.is_optional t) params)
        return_type::(bstype_to_code ::ctx rt)
        ()
    }
  | Class props => {
      let class_types =
        List.map
          (
            fun (key, type_of) => {
              let is_meth =
                switch type_of {
                | Function _ _ _ _ => true
                | _ => false
                };
              let type_of =
                switch type_of {
                | Function type_params params rest_param rt =>
                  let new_params = List.map (fun (_, t) => ("", t)) params;
                  Function type_params new_params rest_param rt
                | any => any
                };
              let method_type_params =
                switch type_of {
                | Function type_params _ _ _ => List.map Genutils.to_type_param type_params
                | any => []
                };
              (key, method_type_params, bstype_to_code ::ctx type_of, is_meth)
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
    | Function type_params params rest_param rt =>
      List.map (fun (id, t) => bstype_precode t) params |>
      List.append (
        switch rest_param {
        | Some (_, t) => [bstype_precode t]
        | None => []
        }
      ) |> List.flatten
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
            name::((var_name == "" ? Genutils.to_module_name module_id : var_name) ^ "_apply")
            module_id::(Genutils.to_module_name module_id)
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
  let from_program program =>
    switch program {
    | ModuleDecl id statements =>
      List.map (decl_to_precode id) statements |> List.flatten |> Genutils.uniq |>
      String.concat "\n"
    | TypeDecl _ _ => decl_to_precode "" program |> String.concat "\n"
    | _ => ""
    };
};

let constructor_type =
  fun
  | Class props => {
      let constructors = List.find_all (fun (id, _) => id == "constructor") props;
      if (List.length constructors == 0) {
        bstype_to_code (Function [] [("_", Unit)] None (Named [] "t"))
      } else {
        let (_, cons_type) = List.hd constructors;
        let cons_type =
          switch cons_type {
          | Function type_params params rest_param rt =>
            let new_params = List.map (fun (_, t) => ("", t)) params;
            Function type_params new_params rest_param (Named [] "t")
          | any => any
          };
        bstype_to_code cons_type
      }
    }
  | _ => raise (CodegenConstructorError "Type has no constructor");

let rec declaration_to_code module_id types =>
  fun
  | VarDecl id type_of =>
    Render.variableDeclaration
      name::(Genutils.normalize_name id)
      module_id::(Genutils.unquote module_id)
      type_of::(bstype_to_code type_of)
      ()
  | FuncDecl id type_of =>
    Render.variableDeclaration
      name::(Genutils.normalize_name id)
      module_id::(Genutils.unquote module_id)
      type_of::(bstype_to_code type_of)
      splice::
        Modulegen.(
          switch type_of {
          | Function _ _ (Some _) _ => true
          | _ => false
          }
        )
      ()
  | ExportsDecl type_of =>
    switch type_of {
    | Typeof (Named _ t) =>
      Typetable.(
        switch (Typetable.get t types) {
        | Class => Render.alias name::(Genutils.to_module_name module_id) value::(t ^ ".make") ()
        | None => raise (CodegenTypeError "typeof can only operate on classes")
        | NotFound => raise (CodegenTypeError ("Unknown identifier: " ^ t))
        | Variable s => raise (CodegenTypeError ("Cannot use typeof with variable: " ^ s))
        | _ => raise (CodegenTypeError "Invalid type from table being rendered")
        }
      )
    | _ =>
      Render.variableDeclaration
        name::(Genutils.to_module_name module_id)
        type_of::(bstype_to_code type_of)
        module_id::(Genutils.unquote module_id)
        is_exports::true
        ()
    }
  | ModuleDecl id statements =>
    Render.moduleDeclaration
      name::id statements::(List.map (declaration_to_code id types) statements) ()
  | TypeDecl id type_of => ""
  | ClassDecl id type_of => {
      let class_name = id;
      let ctor_type = constructor_type type_of;
      let class_type = bstype_to_code type_of;
      Render.classDeclaration
        name::class_name
        exported_as::id
        module_id::(Genutils.unquote module_id)
        ::class_type
        ::ctor_type
        ()
    }
  | InterfaceDecl id type_of =>
    Render.typeDeclaration
      name::(String.uncapitalize_ascii id) type_of::(bstype_to_code type_of) ();

let program_to_code program =>
  switch program {
  | ModuleDecl id statements =>
    let typeof_table = Typetable.create statements;
    /* is the module nested ? */
    let inner_module_name =
      switch (Regexp.split (Regexp.regexp_string "/") id) {
      | [_, x, ...xs] =>
        let module_name =
          [x, ...xs] |> List.map String.capitalize_ascii |> String.concat "" |> (
            fun names => Regexp.global_replace (Regexp.regexp "\\'") names ""
          );
        Some ("module " ^ module_name ^ " = {\n")
      | _ => None
      };
    let (module_prefix, module_postfix) =
      switch inner_module_name {
      | Some n => (n, "\n};\n")
      | None => ("", "")
      };
    /*Typetable.show typeof_table;*/
    Some (
      Genutils.to_module_name id,
      module_prefix ^
      Precode.from_program program ^
      String.concat "\n" (List.map (declaration_to_code id typeof_table) statements) ^ module_postfix
    )
  | TypeDecl _ _ => Some ("", Precode.from_program program ^ declaration_to_code "" [] program)
  | _ => None
  };