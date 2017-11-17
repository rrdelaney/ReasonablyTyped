module Test = {
  type t = {. "action": 't 's . [@bs.meth] (float => string)};
  [@bs.new] [@bs.module "simple-class"] external make : string => t = "Test";
};