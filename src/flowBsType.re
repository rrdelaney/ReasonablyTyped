module Ast = Spider_monkey_ast;

open Ast.Statement;

open Ast.Literal;

open! Ast.Type;

open Ast.Type.Object;

open Ast.Expression.Object.Property;

type context = {
  loc: Loc.t,
  is_params: bool,
};

let intctx = {loc: Loc.none, is_params: false};

exception ModulegenDeclError(string);

exception ModulegenTypeError(string);

exception ModulegenStatementError(string);

let loc_to_msg = ({source, start, _end}: Loc.t) =>
  (
    switch (source) {
    | Some(fname) => " [in " ++ Loc.string_of_filename(fname) ++ " "
    | None => " ["
    }
  )
  ++ "from "
  ++ string_of_int(start.line)
  ++ ":"
  ++ string_of_int(start.column)
  ++ " to "
  ++ string_of_int(_end.line)
  ++ ":"
  ++ string_of_int(_end.column)
  ++ "]";

let not_supported = (interface, context: context) =>
  interface ++ " is not currently supported" ++ loc_to_msg(context.loc);

let sanity_check = (problem, context: context) =>
  problem ++ " should not happen" ++ loc_to_msg(context.loc);

let string_of_id = ((_loc: Loc.t, id: string)) => id;

let string_of_key = (key: Ast.Expression.Object.Property.key) =>
  switch (key) {
  | Identifier(id) => string_of_id(id)
  | Literal((loc, {value})) =>
    switch (value) {
    | String(s) => s
    | _ =>
      raise(
        ModulegenTypeError(
          sanity_check("Non-string as object property", {...intctx, loc}),
        ),
      )
    }
  | Computed((loc, _)) =>
    raise(
      ModulegenTypeError(
        not_supported("Computed object properties", {...intctx, loc}),
      ),
    )
  };

let extract_type_params = (ctx, type_parameters) => {
  open Ast.Type.ParameterDeclaration;
  let get_params = ((loc, {name, bound, variance, default}: TypeParam.t')) =>
    switch (bound, variance, default) {
    | (Some(_), _, _) =>
      raise(
        ModulegenTypeError(
          not_supported("Type parameter bounds", {...ctx, loc}),
        ),
      )
    | (_, Some(_), _) =>
      raise(
        ModulegenTypeError(
          not_supported("Type parameter variance", {...ctx, loc}),
        ),
      )
    | (_, _, Some(_)) =>
      raise(
        ModulegenTypeError(
          not_supported("Type parameter defaults", {...ctx, loc}),
        ),
      )
    | _ => name
    };
  switch (type_parameters) {
  | Some((_loc, {params})) => List.map(get_params, params)
  | None => []
  };
};

let rec type_annotation_to_bstype = (annotation: option(Ast.Type.annotation)) =>
  switch (annotation) {
  | Some((loc, (_, t))) => type_to_bstype({...intctx, loc}, t)
  | None => raise(ModulegenTypeError("Unknown type when parsing annotation"))
  }
and type_to_bstype = (ctx: context) =>
  fun
  | String => BsTypeAst.String
  | Object(o) =>
    if (List.length(o.properties) == 0) {
      BsTypeAst.Object([]);
    } else {
      let first_prop = List.hd(o.properties);
      switch (first_prop) {
      | Indexer((_, {value})) =>
        let (_, value_type) = value;
        BsTypeAst.Dict(type_to_bstype(ctx, value_type));
      | _ => BsTypeAst.Object(object_type_to_bstype(o))
      };
    }
  | Nullable((loc, t)) =>
    BsTypeAst.Optional(type_to_bstype({...ctx, loc}, t))
  | Void => BsTypeAst.Unit
  | Mixed => BsTypeAst.Any
  | Any => BsTypeAst.Any
  | Null => BsTypeAst.Null
  | Number => BsTypeAst.Number
  | Boolean => BsTypeAst.Boolean
  | Function(f) => function_type_to_bstype(ctx, f)
  | Array((loc, t)) => BsTypeAst.Array(type_to_bstype({...ctx, loc}, t))
  | Tuple(types) =>
    BsTypeAst.Tuple(
      List.map(((loc, t)) => type_to_bstype({...ctx, loc}, t), types),
    )
  | Intersection((_loc_a, _first), (_loc_b, _second), _rest) =>
    raise(ModulegenTypeError(not_supported("Intersection types", ctx)))
  | Union((loc_a, first), (loc_b, second), rest) =>
    BsTypeAst.Union([
      type_to_bstype({...ctx, loc: loc_a}, first),
      type_to_bstype({...ctx, loc: loc_b}, second),
      ...List.map(((_, t)) => type_to_bstype(ctx, t), rest),
    ])
  | Generic(g) => generic_type_to_bstype(ctx, g)
  | StringLiteral({value}) => BsTypeAst.StringLiteral(value)
  | NumberLiteral(_) => BsTypeAst.Number
  | BooleanLiteral(_) => BsTypeAst.Boolean
  | Typeof((loc, t)) => BsTypeAst.Typeof(type_to_bstype({...ctx, loc}, t))
  | Exists => raise(ModulegenTypeError(not_supported("Exists type", ctx)))
  | Empty => raise(ModulegenTypeError(not_supported("Empty type", ctx)))
and function_type_to_bstype = (ctx, f) => {
  let {params: (formal, rest), returnType: (rt_loc, rt), typeParameters}: Function.t = f;
  let type_params = extract_type_params(ctx, typeParameters);
  let arg_types =
      (
        (_, {typeAnnotation: (loc, t), name, optional}): Ast.Type.Function.Param.t,
      ) => (
    switch (name) {
    | Some(id) => string_of_id(id)
    | None => ""
    },
    if (optional) {
      BsTypeAst.Optional(type_to_bstype({...ctx, loc}, t));
    } else {
      type_to_bstype({...ctx, loc}, t);
    },
  );
  let formal_params = List.map(arg_types, formal);
  let rest_params =
    switch (rest) {
    | Some((_, {argument})) =>
      let base_type = arg_types(argument);
      /* rest params cannot be BS-optional */
      Some(
        switch (base_type) {
        | (id, BsTypeAst.Optional(t)) => (id, t)
        | t => t
        },
      );
    | None => None
    };
  /* because you can't have a zero-arity Reason function */
  let no_args = List.length(formal_params) == 0 && rest_params === None;
  let return_type = type_to_bstype({...ctx, loc: rt_loc}, rt);
  BsTypeAst.Function({
    typeParams: type_params,
    formalParams: no_args ? [("", BsTypeAst.Unit)] : formal_params,
    restParam: rest_params,
    returnType: return_type,
  });
}
and value_to_bstype = (value: Ast.Type.Object.Property.value) =>
  switch (value) {
  | Init((loc, t)) => type_to_bstype({...intctx, loc}, t)
  | Get((loc, func)) => function_type_to_bstype({...intctx, loc}, func)
  | Set((loc, func)) => function_type_to_bstype({...intctx, loc}, func)
  }
and object_type_to_bstype = ({properties}) =>
  List.map(
    fun
    | Property((_loc, {key, value, optional})) => (
        string_of_key(key),
        value_to_bstype(value),
        optional,
      )
    | CallProperty((_loc, props)) => {
        let {value: (loc, value), static}: Object.CallProperty.t' = props;
        if (static) {
          raise(
            ModulegenTypeError(
              not_supported(
                "static CallProperty on Object types",
                {...intctx, loc},
              ),
            ),
          );
        };
        (
          "$$callProperty",
          type_to_bstype({...intctx, loc}, Function(value)),
          false,
        );
      }
    | Indexer((loc, _)) =>
      raise(
        ModulegenTypeError(
          not_supported("Indexer on Object types", {...intctx, loc}),
        ),
      )
    | SpreadProperty((loc, _)) =>
      raise(
        ModulegenTypeError(
          not_supported("SpreadProperty on Object types", {...intctx, loc}),
        ),
      ),
    properties,
  )
and generic_type_to_bstype = (ctx, g) => {
  let {id, typeParameters}: Generic.t = g;
  switch (id) {
  | Qualified((_, q)) => BsTypeAst.Named([], string_of_id(q.id), None)
  | Unqualified(q) => named_to_bstype(ctx, typeParameters, q)
  };
}
and named_to_bstype = (ctx, type_params, (loc, id)) =>
  switch (id) {
  | "Date" => BsTypeAst.Date
  | "RegExp" => BsTypeAst.Regex
  | "Object" => BsTypeAst.AnyObject
  | "Array" =>
    open Ast.Type.ParameterInstantiation;
    let params =
      switch (type_params) {
      | Some((_, {params: []})) =>
        raise(
          ModulegenTypeError(
            not_supported("Array with no types", {...ctx, loc}),
          ),
        )
      | Some((_, {params})) => params
      | None =>
        raise(
          ModulegenTypeError(
            not_supported("Array with more than one type", {...ctx, loc}),
          ),
        )
      };
    let (loc, inner_type) = List.hd(params);
    BsTypeAst.Array(type_to_bstype({...ctx, loc}, inner_type));
  | "Function" => BsTypeAst.AnyFunction
  | "Class" =>
    open Ast.Type.ParameterInstantiation;
    let (loc, inner_type) =
      switch (type_params) {
      | Some((_, {params: [type_param]})) => type_param
      | None =>
        raise(
          ModulegenTypeError(
            "Class must have exactly one type parameter. Found none.",
          ),
        )
      | Some((_, {params})) =>
        raise(
          ModulegenTypeError(
            "Class must have exactly one type parameter. Got: "
            ++ string_of_int @@
            List.length(params),
          ),
        )
      };
    BsTypeAst.Typeof(type_to_bstype({...ctx, loc}, inner_type));
  | "Promise" =>
    open Ast.Type.ParameterInstantiation;
    let (loc, inner_type) =
      switch (type_params) {
      | Some((_, {params: [type_param]})) => type_param
      | None =>
        raise(
          ModulegenTypeError(
            "Promise must have exactly one type parameter. Found none.",
          ),
        )
      | Some((_, {params})) =>
        raise(
          ModulegenTypeError(
            "Promise must have exactly one type parameter. Got: "
            ++ string_of_int @@
            List.length(params),
          ),
        )
      };
    BsTypeAst.Promise(type_to_bstype({...ctx, loc}, inner_type));
  | _ =>
    if (String.length(id) > 0
        && id.[0] == '$'
        && String.sub(id, 0, 4) != "$npm") {
      raise(ModulegenTypeError(not_supported("Built-in type " ++ id, ctx)));
    } else {
      let type_params =
        switch (type_params) {
        | None => []
        | Some((_, {params})) =>
          List.map(
            ((loc, type_of)) => type_to_bstype({...ctx, loc}, type_of),
            params,
          )
        };
      BsTypeAst.Named(type_params, id, None);
    }
  };

let rec declaration_to_jsdecl = loc =>
  fun
  | DeclareExportDeclaration.Variable((_loc, {id, typeAnnotation})) =>
    BsTypeAst.VarDecl(
      string_of_id(id),
      type_annotation_to_bstype(typeAnnotation),
    )
  | Function((_loc, {id, typeAnnotation})) => {
      let bstype = type_annotation_to_bstype(Some(typeAnnotation));
      BsTypeAst.FuncDecl(string_of_id(id), bstype);
    }
  | Class((loc, c)) => declare_class_to_jsdecl(loc, c)
  | _ =>
    raise(
      ModulegenDeclError(
        "Unknown declaration when converting a module property declaration"
        ++ loc_to_msg(loc),
      ),
    )
and declare_interface_to_jsdecl = (_loc, s) => {
  let {id, body, typeParameters, extends}: Interface.t = s;
  switch (extends) {
  | [(loc, _extends), ..._t] =>
    raise(
      ModulegenStatementError(
        not_supported("Inheriting in interfaces", {...intctx, loc}),
      ),
    )
  | _ => ()
  };
  let (body_loc, obj_type) = body;
  let body_type = Object(obj_type);
  BsTypeAst.InterfaceDecl(
    string_of_id(id),
    extract_type_params(intctx, typeParameters),
    type_to_bstype({...intctx, loc: body_loc}, body_type),
  );
}
and declare_class_to_jsdecl = (loc, s) => {
  let {id, typeParameters, body: (_, interface), extends}: Interface.t = s;
  let inheritedClasses =
    switch (extends) {
    | [] => None
    | [(loc, parent)] =>
      Some(type_to_bstype({...intctx, loc}, Ast.Type.Generic(parent)))
    | _ =>
      raise(
        ModulegenStatementError(
          not_supported("Inheriting from multiple types", {...intctx, loc}),
        ),
      )
    };
  let properties = object_type_to_bstype(interface);
  let properties = List.map(((x, y, _z)) => (x, y), properties);
  BsTypeAst.ClassDecl(
    string_of_id(id),
    extract_type_params(intctx, typeParameters),
    BsTypeAst.Class(inheritedClasses, properties),
  );
};

let import_decl_to_jsdecl = (loc, s) => {
  open Ast.Statement.ImportDeclaration;
  let {importKind, source, specifiers} = s;
  let imported_module =
    switch (source) {
    | (_, {value: Ast.Literal.String(s)}) => s
    | (_, _) => ""
    };
  switch (importKind) {
  | ImportType =>
    let import_names =
      List.map(
        fun
        | Ast.Statement.ImportDeclaration.ImportNamedSpecifier({
            remote,
            local,
          }) => (
            string_of_id(remote),
            switch (local) {
            | Some(s) => string_of_id(s)
            | None => string_of_id(remote)
            },
          )
        | _ => ("", ""),
        specifiers,
      );
    BsTypeAst.ImportDecl(import_names, imported_module);
  | ImportTypeof =>
    raise(
      ModulegenStatementError(
        not_supported("'import typeof'", {...intctx, loc}),
      ),
    )
  | ImportValue =>
    raise(
      ModulegenStatementError(
        not_supported("Importing values", {...intctx, loc}),
      ),
    )
  };
};

let rec flowAstToBsTypeAst = ((loc, s)) =>
  switch (s) {
  | Ast.Statement.DeclareModuleExports(annotation) =>
    BsTypeAst.ExportsDecl(type_annotation_to_bstype(Some(annotation)))
  | Ast.Statement.DeclareExportDeclaration({declaration: Some(declaration)}) =>
    declaration_to_jsdecl(loc, declaration)
  | Ast.Statement.DeclareFunction(declare_function) =>
    declaration_to_jsdecl(loc, Function((loc, declare_function)))
  | Ast.Statement.DeclareClass(s) => declare_class_to_jsdecl(loc, s)
  | Ast.Statement.TypeAlias({id, typeParameters, right: (loc, t)}) =>
    BsTypeAst.TypeDecl(
      string_of_id(id),
      extract_type_params(intctx, typeParameters),
      type_to_bstype({...intctx, loc}, t),
    )
  | Ast.Statement.DeclareModule(s) => declare_module_to_jsdecl(loc, s)
  | Ast.Statement.DeclareVariable({id, typeAnnotation}) =>
    if (string_of_id(id) == "exports") {
      BsTypeAst.ExportsDecl(type_annotation_to_bstype(typeAnnotation));
    } else {
      BsTypeAst.VarDecl(
        string_of_id(id),
        type_annotation_to_bstype(typeAnnotation),
      );
    }
  | Ast.Statement.InterfaceDeclaration(s) =>
    declare_interface_to_jsdecl(loc, s)
  /* | Ast.Statement.DeclareInterface(s) => declare_interface_to_jsdecl(loc, s) */
  /* | Ast.Statement.DeclareTypeAlias({id, typeParameters, right: (loc, t)}) =>
     BsTypeAst.TypeDecl(
       string_of_id(id),
       extract_type_params(intctx, typeParameters),
       type_to_bstype({...intctx, loc}, t)
     ) */
  | Ast.Statement.ImportDeclaration(s) => import_decl_to_jsdecl(loc, s)
  /* | Ast.Statement.DeclareOpaqueType(_) =>
     raise(
       ModulegenStatementError(
         not_supported("Opaque types", {...intctx, loc})
       )
     ) */
  | Ast.Statement.ClassDeclaration({
      id: Some((_loc, className)),
      superClass:
        Some((
          _,
          Member({
            _object: (_, Identifier(oID)),
            property: PropertyIdentifier(pID),
          }),
        )),
      superTypeParameters: Some((_, {params: [(_, propt), ..._]})),
    })
      when
        string_of_id(oID) == "React"
        && (
          string_of_id(pID) == "Component"
          || string_of_id(pID) == "PureComponent"
        ) =>
    let propsType = type_to_bstype({...intctx, loc}, propt);
    BsTypeAst.ReactClass(className, propsType);
  | Ast.Statement.ClassDeclaration(_) =>
    raise(
      ModulegenStatementError(
        not_supported("Class declatations", {...intctx, loc}),
      ),
    )
  | Ast.Statement.Empty => BsTypeAst.Noop
  | Ast.Statement.ExportDefaultDeclaration(_) =>
    raise(
      ModulegenStatementError(
        not_supported("ExportDefaultDeclaration", {...intctx, loc}),
      ),
    )
  | Ast.Statement.ExportNamedDeclaration(_) =>
    raise(
      ModulegenStatementError(
        not_supported("ExportNamedDeclaration", {...intctx, loc}),
      ),
    )
  | Ast.Statement.VariableDeclaration(_) =>
    BsTypeAst.Ignore("variable declaration")
  | Ast.Statement.Expression(_) => BsTypeAst.Ignore("expression statement")
  | _ =>
    raise(
      ModulegenStatementError(
        "Unknown statement type when parsing libdef" ++ loc_to_msg(loc),
      ),
    )
  }
and block_to_program = ((_loc, {body}: Block.t)) =>
  List.map(flowAstToBsTypeAst, body)
and declare_module_to_jsdecl = (loc, s) => {
  let {id, body}: DeclareModule.t = s;
  switch (id) {
  | Literal((_loc, {raw})) =>
    BsTypeAst.ModuleDecl(raw, block_to_program(body))
  | _ =>
    raise(
      ModulegenDeclError(
        "Unknown declaration type when converting a module declaration"
        ++ loc_to_msg(loc),
      ),
    )
  };
};
