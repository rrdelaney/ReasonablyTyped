let decodeBoolean = booleanOption =>
  booleanOption
  |> (
    fun
    | Some(debugMode) => Js.Json.decodeBoolean(debugMode)
    | None => None
  )
  |> (
    fun
    | Some(bool) when bool == Js.true_ => true
    | Some(bool) when bool == Js.false_ => false
    | _ => false
  );

let cli =
  Meow.make(
    {|
  Usage:
    $ retyped ...files

  Examples:
    $ retyped file1.js file2.js file3.d.ts
|},
    Meow.Config.config(
      ~flags=
        Js.Dict.fromArray([|
          /*("debug", Meow.Flag.flag(~type_="boolean")),*/
        |]),
    ),
  );

let debugMode =
  cli |> Meow.CLI.flags |> Js.Dict.get(_, "debug") |> decodeBoolean;

let compileFile = _file => Js.Promise.resolve(true);

let compileFile = files =>
  files
  |> Array.map(compileFile)
  |> Js.Promise.all
  |> Js.Promise.then_(results => {
       let filesWritten =
         results |> Js.Array.filter(s => s == true) |> Js.Array.length;
       let errorCount =
         results |> Js.Array.filter(s => s == true) |> Js.Array.length;
       Js.Promise.resolve();
     });

Js.log(cli);
