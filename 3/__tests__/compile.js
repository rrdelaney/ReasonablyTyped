const fs = require("fs");
const path = require("path");
const babel = require("../babel");

const fixtures = path.join(__dirname, "fixtures");
const fixture = file => {
  return fs.readFileSync(path.join(fixtures, file));
};

const testFiles = fs.readdirSync(fixtures).map(file => {
  it(file, () => {
    const input = fixture(file);
    const code = babel(input);
    expect(code).toMatchSnapshot();
  });
});
