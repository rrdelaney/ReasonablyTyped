external foo : bars::array float => unit = "" [@@bs.module "spread-args"];
external optFoo : bars::array float? => unit => unit = "" [@@bs.module "spread-args"];
external bothFoo: anArg::string => bars::array float => unit = "" [@@bs.module "spread-args"];
external bothOptFoo : anArg::string => bars::array float? => unit => unit = "" [@@bs.module "spread-args"];
external soManyOpts: anArg::string? => bars::array float? => unit => unit = "" [@@bs.module "spread-args"];
external returns : anArg::string => bars::array float => float = "" [@@bs.module "spread-args"];