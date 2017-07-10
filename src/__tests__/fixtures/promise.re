external pOfString : unit => Js_promise.t string = "" [@@bs.module "promise"];
external pOfNumber : unit => Js_promise.t float = "" [@@bs.module "promise"];
external pOfArray : unit => Js_promise.t array string = "" [@@bs.module "promise"];
external pOfVoid : unit => Js_promise.t unit = "" [@@bs.module "promise"];
external argPromise : p::Js_promise.t string => unit = "" [@@bs.module "promise"];