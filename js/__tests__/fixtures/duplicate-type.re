external add : x::[ | `Number float | `String string] [@bs.unwrap] => float =
  "" [@@bs.module "duplicate-type"];

external sub : x::[ | `Number float | `String string] [@bs.unwrap] => float =
  "" [@@bs.module "duplicate-type"];
