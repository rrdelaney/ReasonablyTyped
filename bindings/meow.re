module Flag = {
  [@bs.deriving abstract]
  type flag = {
    [@bs.optional] [@bs.as "type"]
    type_: string,
  };
};

module Config = {
  [@bs.deriving abstract]
  type config = {flags: Js.Dict.t(Flag.flag)};
};

module CLI = {
  [@bs.deriving abstract]
  type cli = {
    flags: Js.Dict.t(Js.Json.t),
    input: array(string),
  };
};

[@bs.module] external make : (string, Config.config) => CLI.cli = "meow";
