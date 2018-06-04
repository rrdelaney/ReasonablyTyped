open Belt;

let rec fromDotTyped =
  fun
  | DotTyped.Regex => Rabel.regex()
  | DotTyped.Void => Rabel.unit()
  | DotTyped.Null => Rabel.null()
  | DotTyped.Float => Rabel.float()
  | DotTyped.String => Rabel.string()
  | DotTyped.Boolean => Rabel.bool()
  | DotTyped.Dict(_, t) => Rabel.dict(fromDotTyped(t))
  | DotTyped.Optional(t) => Rabel.optional(fromDotTyped(t))
  | DotTyped.Array(t) => Rabel.array(fromDotTyped(t))
  | DotTyped.Tuple(types) => types |. Array.map(fromDotTyped) |. Rabel.tuple;
