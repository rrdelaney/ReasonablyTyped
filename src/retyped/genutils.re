open Modulegen.BsType;

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

let import_module_name name => normalize_name name |> String.capitalize_ascii;

let to_module_name str => normalize_name (unquote str);

let to_type_param str => "'" ^ String.uncapitalize_ascii str |> normalize_name;

let rec split sep str acc => {
  open String;
  let len = length str;
  let first_index =
    try (Some (index str sep)) {
    | Not_found => None
    };
  switch first_index {
  | None => List.append acc [str]
  | Some i =>
    let beginning = min len (i + 1);
    split
      sep
      (sub str beginning (len - beginning))
      (List.append acc [sub str 0 (max 0 (beginning - 1))])
  }
};

let rec uniq =
  fun
  | [] => []
  | [h, ...t] => {
      let no_dups = uniq (List.filter (fun x => x != h) t);
      [h, ...no_dups]
    };

module Is = {
  let optional type_of =>
    switch type_of {
    | Optional _ => true
    | _ => false
    };
  let type_param params t => List.exists (fun p => p == t) params;
  let class_type t table =>
    switch (Typetable.get t table) {
    | Class => true
    | _ => false
    };
  let string_union =
    List.for_all (
      fun
      | StringLiteral _ => true
      | _ => false
    );
  let react_component =
    fun
    | Class extends _props => true
    | _ => false;
};

module React = {
  let extract_props component => {};
};

let walk replacer => {
  open Modulegen.BsDecl;
  open Modulegen.BsType;
  let or_value transform t =>
    switch (transform t) {
    | Some result => result
    | None => t
    };
  let rec walk_type ::recurse=true walkable =>
    switch walkable {
    | Function type_params formal_params rest_param return_type as ft =>
      switch (replacer ft) {
      | Some new_t when recurse => walk_type recurse::false new_t
      | _ =>
        Function
          type_params
          (List.map (fun (name, t) => (name, walk_type t)) formal_params)
          (
            switch rest_param {
            | Some (name, t) => Some (name, walk_type t)
            | None => None
            }
          )
          (walk_type return_type)
      }
    | Object fields as ot =>
      switch (replacer ot) {
      | Some new_t when recurse => walk_type recurse::false new_t
      | _ => Object (List.map (fun (name, t) => (name, walk_type t)) fields)
      }
    | Class extends fields as ct =>
      switch (replacer ct) {
      | Some new_t when recurse => walk_type recurse::false new_t
      | _ =>
        Class extends (List.map (fun (name, t) => (name, walk_type t)) fields)
      }
    | Dict t as dt =>
      switch (replacer dt) {
      | Some new_t when recurse => walk_type recurse::false new_t
      | _ => Dict (walk_type t)
      }
    | Array t as at =>
      switch (replacer at) {
      | Some new_t when recurse => walk_type recurse::false new_t
      | _ => Array (walk_type t)
      }
    | Union types as ut =>
      switch (replacer ut) {
      | Some new_t when recurse => walk_type recurse::false new_t
      | _ => Union (List.map walk_type types)
      }
    | Tuple types as tt =>
      switch (replacer tt) {
      | Some new_t when recurse => walk_type recurse::false new_t
      | _ => Tuple (List.map walk_type types)
      }
    | Typeof t as tt =>
      switch (replacer tt) {
      | Some new_t when recurse => walk_type recurse::false new_t
      | _ => Typeof (walk_type t)
      }
    | Named types value module_name as nt =>
      switch (replacer nt) {
      | Some new_t when recurse => walk_type recurse::false new_t
      | _ => Named (List.map walk_type types) value module_name
      }
    | Optional t as ot =>
      switch (replacer ot) {
      | Some new_t when recurse => walk_type recurse::false new_t
      | _ => Optional (walk_type t)
      }
    | Promise t as pt =>
      switch (replacer pt) {
      | Some new_t when recurse => walk_type recurse::false new_t
      | _ => Promise (walk_type t)
      }
    | t => or_value replacer t
    };
  fun
  | VarDecl name t => VarDecl name (walk_type t)
  | FuncDecl name t => FuncDecl name (walk_type t)
  | TypeDecl name params t => TypeDecl name params (walk_type t)
  | ClassDecl name params t => ClassDecl name params (walk_type t)
  | s => s
};
