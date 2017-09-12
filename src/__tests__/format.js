const { format } = require('../../jslib')

test('Formatted code should equal itself', () => {
  const testCode = `
    let x = "hello";
    print_endline x;
  `

  expect(format(testCode)).toEqual(format(testCode))
})
