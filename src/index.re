print_endline "hello world!!!";

let file = Node.Fs.readFileSync "package.json" `utf8;

print_endline file;