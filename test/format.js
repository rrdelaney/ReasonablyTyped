import test from 'ava'
import { format } from '../lib'

test('Formatted code should equal itself', t => {
  const testCode = `
    let x = "hello";
    print_endline x;
  `

  t.is(format(testCode), format(testCode))
})
