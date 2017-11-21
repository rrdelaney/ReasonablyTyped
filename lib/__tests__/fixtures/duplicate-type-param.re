[@bs.module "duplicate-type-param"]
external add :
  (
    ~x: [@bs.unwrap] [ | `Number(float) | `String(string)],
    ~y: [@bs.unwrap] [ | `Number(float) | `String(string)]
  ) =>
  float =
  "";