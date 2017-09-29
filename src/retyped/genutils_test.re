let t1 =
  Coverall.Test.t
    "Genutils.unquote - Remove quotes from a string" (fun () => {});

let t2 =
  Coverall.Test.t
    "Genutils.unquote - Don't unquote a string with quotes"
    (fun () => assert false);

let all = [t1, t2];
