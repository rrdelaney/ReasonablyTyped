type thing 'x = Js.t {. lst : array 'x};

module Adder = {
  type t 'x = Js.t {. add : ('x => 'x) [@bs.meth]};
  external make : 'x => t 'x = "Adder" [@@bs.new] [@@bs.module "generics"];
};

type subOpts 'm 'n = Js.t {. m : 'm, n : Adder.t 'n};
