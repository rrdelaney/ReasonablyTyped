[@bs.module] external reactNamePickerInlinedProps : ReasonReact.reactClass =
  "ReactNamePickerInlinedProps.react";

let make = (~width, ~height, children) => {
  let props: {. "width": float, "height": float} = {"width": width, "height": height};
  ReasonReact.wrapJsForReason(~reactClass=reactNamePickerInlinedProps, ~props, children)
};