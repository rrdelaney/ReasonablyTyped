type number_or_string =
  | Number float
  | String string;

external add : number_or_string => number_or_string => float =
  "" [@@bs.module "duplicate-type-param"];
