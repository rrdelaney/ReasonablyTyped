type subscribeOptions = Js.t {.. start : float, stop : float};

type testOptions = Js.t {.. method : string, subscribe : subscribeOptions};

external test : options::testOptions => string = "" [@@bs.module "object-type"];
