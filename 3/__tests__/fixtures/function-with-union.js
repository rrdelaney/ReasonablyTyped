type ButtonSize = "small" | "default" | "large";

function getCorrectSize(size: ButtonSize): number {
  return {
    small: 10,
    default: 20,
    large: 30
  }[size];
}
