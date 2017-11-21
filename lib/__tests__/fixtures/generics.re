type thing('x) = {. "lst": array('x)};

module Adder = {
  type t('x) = {. "add": [@bs.meth] ('x => 'x)};
  [@bs.new] [@bs.module "generics"] external make : 'x => t('x) = "Adder";
};

type subOpts('m, 'n) = {. "m": 'm, "n": Adder.t('n)};