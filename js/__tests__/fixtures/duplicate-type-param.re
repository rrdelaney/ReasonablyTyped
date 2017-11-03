external add :
  x::[ | `Number float | `String string] [@bs.unwrap] =>
  y::[ | `Number float | `String string] [@bs.unwrap] =>
  float =
  "" [@@bs.module "duplicate-type-param"];
