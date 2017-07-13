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

let to_module_name str => normalize_name (unquote str);

let to_type_param str => "'" ^ String.uncapitalize_ascii str |> normalize_name;

let rec uniq =
  fun
  | [] => []
  | [h, ...t] => {
      let no_dups = uniq (List.filter (fun x => x != h) t);
      [h, ...no_dups]
    };

let is_optional type_of =>
  switch type_of {
  | Optional _ => true
  | _ => false
  };

let is_type_param params t => List.exists (fun p => p == t) params;

module Uid = {
  let get () => string_of_int 1;
  let uniq prefix => prefix ^ "_" ^ get ();
};
