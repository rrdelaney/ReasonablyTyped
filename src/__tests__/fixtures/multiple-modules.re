module First = {
  external first : unit => float = "" [@@bs.module "first"];
  external second : unit => unit = "" [@@bs.module "first"];
};

module Second = {
  external second : unit => string = "" [@@bs.module "second"];
  external third : unit => unit = "" [@@bs.module "second"];
};