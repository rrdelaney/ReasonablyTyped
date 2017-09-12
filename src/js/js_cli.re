module Cli =
  Cli.Run {
    let read_file fname => Node_fs.readFileSync fname `ascii;
    let write_file fname contents => Node_fs.writeFileSync fname contents `ascii;
  };
