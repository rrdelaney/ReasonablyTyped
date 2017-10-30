let t = Coverall.Test.m("Genutils");

t#test(
  "unquote should remove single quotes from a string",
  () => {
    let str = "'hello world!'";
    let unquoted_str = Genutils.unquote(str);
    assert (unquoted_str.[0] != '\'');
    assert (unquoted_str.[String.length(unquoted_str) - 1] != '\'')
  }
);

t#test(
  "unquote should remove double quotes from a string",
  () => {
    let str = "\"hello world!\"";
    let unquoted_str = Genutils.unquote(str);
    assert (unquoted_str.[0] != '"');
    assert (unquoted_str.[String.length(unquoted_str) - 1] != '"')
  }
);

t#test(
  "unquote shouldn't unquote a string with quotes",
  () => {
    let str = "hello world!'";
    let unquoted_str = Genutils.unquote(str);
    assert (unquoted_str.[0] == 'h');
    assert (unquoted_str.[String.length(unquoted_str) - 1] == '\'')
  }
);

let all = t#all();
