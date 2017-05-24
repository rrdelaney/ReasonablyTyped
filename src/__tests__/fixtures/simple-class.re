type test = Js.t {. action : a::float => string [@bs.meth]};

external create_test : t::string => test = "Test" [@@bs.new] [@@bs.module "simple-class"];
