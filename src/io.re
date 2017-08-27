/* This lets us use the same logic (cli.re) for js and native. Currently bsb doesn't support
   open_in / open_out from Pervasive so this is the temporary solution. */
module type S = {let read_file: string => string; let write_file: string => string => unit;};
