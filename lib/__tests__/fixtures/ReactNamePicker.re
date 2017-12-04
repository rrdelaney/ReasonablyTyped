type myProps = {. "width": float, "height": float};

[@bs.module] external reactNamePicker : ReasonReact.reactClass = "ReactNamePicker.react";

let make = (~width, ~height, children) => {
  let props: myProps = {"width": width, "height": height};
  ReasonReact.wrapJsForReason(~reactClass=reactNamePicker, ~props, children)
};