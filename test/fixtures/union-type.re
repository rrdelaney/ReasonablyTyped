type number_or_string =
  | Number float
  | String string;

external double : number_or_string => float = "" [@@bs.module "union-type"];
