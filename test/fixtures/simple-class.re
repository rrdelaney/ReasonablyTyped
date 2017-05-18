type test = Js.t {. action : float => string [@bs.meth]};

external create_test : string => test = "Test" [@@bs.new] [@@bs.module "simple-class"];
