type loc = {
  source: string,
  line: int,
  column: int,
  length: int,
};

type error = {
  loc,
  message: string,
};

exception NotSupported(error);

exception UnknownType(error);

exception Unimplemented;
