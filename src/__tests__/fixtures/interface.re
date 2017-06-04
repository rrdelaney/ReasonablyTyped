type options = Js.t {. op : string};

external apply : x::float => y::float => options::options? => unit => float =
  "" [@@bs.module "interface"];
