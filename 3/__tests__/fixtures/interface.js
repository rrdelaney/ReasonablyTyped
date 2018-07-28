interface Props {
  innerRef?: ElementRef<*>;
  htmlType?: "submit" | "button";
  text: React$Node;
  type: ButtonType;
  onClick?: (SyntheticEvent<*>) => void | Promise<*>;
  className?: string;
  loading?: boolean;
  disabled: boolean;
  size: ButtonSize;
  style?: Object;
}
