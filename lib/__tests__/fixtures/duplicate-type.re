[@bs.module "duplicate-type"]
external add : (~x: [@bs.unwrap] [ | `Number(float) | `String(string)]) => float =
  "";

[@bs.module "duplicate-type"]
external sub : (~x: [@bs.unwrap] [ | `Number(float) | `String(string)]) => float =
  "";