external someTopLevel : unit => unit = "" [@@bs.module "multiple-modules"];

module First = {
  external first : unit => float = "" [@@bs.module "multiple-modules/first"];
  external second : unit => unit = "" [@@bs.module "multiple-modules/first"];
};

module Second = {
  external second : unit => string = "" [@@bs.module "multiple-modules/second"];
  external third : unit => unit = "" [@@bs.module "multiple-modules/second"];
};

module NestedThird = {
  external third : unit => string = "" [@@bs.module "multiple-modules/nested/third"];
  external fourth : unit => unit = "" [@@bs.module "multiple-modules/nested/third"];
};