[@bs.module "union-type"]
external double : (~x: [@bs.unwrap] [ | `Number(float) | `String(string)]) => float =
  "";