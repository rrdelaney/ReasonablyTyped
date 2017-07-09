external foo : bars::array float => unit = "" [@@bs.module "spread-args"][@@bs.splice];
external optFoo : bars::array float => unit = "" [@@bs.module "spread-args"][@@bs.splice];
external bothFoo: anArg::string => bars::array float => unit = "" [@@bs.module "spread-args"][@@bs.splice];
external bothOptFoo : anArg::string => bars::array float => unit = "" [@@bs.module "spread-args"][@@bs.splice];
external soManyOpts: anArg::string? => unit => bars::array float => unit = "" [@@bs.module "spread-args"][@@bs.splice];
external returns : anArg::string => bars::array float => float = "" [@@bs.module "spread-args"][@@bs.splice];
external multipleLists : foos::array float => bars::array float => unit = "" [@@bs.module "spread-args"][@@bs.splice];
