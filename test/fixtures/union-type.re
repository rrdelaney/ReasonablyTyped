type number_or_string =
  | Number float
  | String string;

external double : x::number_or_string => float = "" [@@bs.module "union-type"];
