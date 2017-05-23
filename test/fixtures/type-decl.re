type stringOptions = Js.t {. value : string};

external test : string => stringOptions => string = "" [@@bs.module "type-decl"];
