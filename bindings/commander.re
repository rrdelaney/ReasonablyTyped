module Process = {
  type argv;
  [@bs.scope "process"] [@bs.val] external argv : argv = "";
};

type program;

[@bs.module] external program : program = "commander";

[@bs.send] external version : (program, string) => program = "";

[@bs.send] external option : (program, string, string) => program = "";

[@bs.send] external usage : (program, string) => program = "";

[@bs.send] external parse : (program, Process.argv) => Js.Json.t = "";
