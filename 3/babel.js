const babel = require("@babel/core");
const plugin = require("./plugin");

module.exports = function(input) {
  const { code } = babel.transform(input, {
    plugins: [plugin, "@babel/syntax-jsx", "@babel/syntax-flow"]
  });
  return code
    .replace(/^type/gm, "declare type")
    .replace(/^function/gm, "declare function")
    .replace(/^interface/gm, "declare interface")
    .replace(/^class/gm, "declare class");
};
