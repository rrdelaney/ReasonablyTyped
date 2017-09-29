module Test = {
  type t = Js.t {. action : (float => string) [@bs.meth]};
  external make : string => t = "Test" [@@bs.new] [@@bs.module "simple-class"];
};
