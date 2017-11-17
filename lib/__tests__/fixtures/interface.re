type options = {. "op": string};

[@bs.module "interface"]
external apply : (~x: float, ~y: float, ~options: options=?, unit) => float =
  "";