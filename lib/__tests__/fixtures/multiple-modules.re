[@bs.module "multiple-modules"] external someTopLevel : unit => unit = "";

module First = {
  [@bs.module "multiple-modules/first"] external first : unit => float = "";
  [@bs.module "multiple-modules/first"] external second : unit => unit = "";
};

module Second = {
  [@bs.module "multiple-modules/second"] external second : unit => string = "";
  [@bs.module "multiple-modules/second"] external third : unit => unit = "";
};

module Third = {
  [@bs.module "multiple-modules/third"] external third : unit => string = "";
  [@bs.module "multiple-modules/third"] external fourth : unit => unit = "";
};

module ThirdInner = {
  [@bs.module "multiple-modules/third/inner"] external third : unit => string = "";
  [@bs.module "multiple-modules/third/inner"] external fourth : unit => unit = "";
};