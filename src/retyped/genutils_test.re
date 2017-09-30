let t = Coverall.Test.m "Genutils";

t#test "unquote should remove quotes from a string" (fun () => ());

t#test
  "unquote shouldn't unquote a string with quotes" (fun () => assert false);

let all = t#all ();
