type number_or_string =
  | Number float
  | String string;

external add : x::number_or_string => float = "" [@@bs.module "duplicate-type"];

external sub : x::number_or_string => float = "" [@@bs.module "duplicate-type"];
