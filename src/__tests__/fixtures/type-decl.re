type stringOptions = Js.t {. value : string};

external test : s::string => options::stringOptions => string = "" [@@bs.module "type-decl"];
