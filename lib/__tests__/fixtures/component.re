type props = {. "input": Js.Nullable.t(string), "b": Js.Nullable.t(Js.boolean)};

module ReactComponent = {
  [@bs.module "react-component"] external reactComponent : ReasonReact.reactClass =
    "ReactComponent";
  let make = (~input=?, ~b=?, children) => {
    let props: props = {
      "input": Js.Nullable.from_opt(input),
      "b": Js.Nullable.bind(Js.Nullable.from_opt(b), [@bs] ((x) => Js.Boolean.to_js_boolean(x)))
    };
    ReasonReact.wrapJsForReason(~reactClass=reactComponent, ~props, children)
  };
};