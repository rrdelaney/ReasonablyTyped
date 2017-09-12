module Cli =
  Cli.Run {
    let read_file fname => {
      let ic = open_in fname;
      let n = in_channel_length ic;
      let s = Bytes.create n;
      really_input ic s 0 n;
      close_in ic;
      Bytes.to_string s
    };
    let write_file fname contents => {
      let oc = open_out fname;
      output_string oc contents;
      close_out oc
    };
  };
