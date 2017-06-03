const path = require('path')
const { readdirSync, readFile } = require('fs')
const { compile, format } = require('../../lib')

const fixtures = path.join(__dirname, 'fixtures')
const fixture = file => path.join(fixtures, file)

const testFiles = readdirSync(fixtures)
  .filter(file => file.endsWith('.js'))
  .map(file => ({
    [file]: {
      js: new Promise((resolve, reject) => {
        readFile(fixture(file), (err, data) => {
          if (err) return reject(err)
          resolve(data.toString())
        })
      }),
      re: new Promise((resolve, reject) => {
        readFile(fixture(file.replace('.js', '.re')), (err, data) => {
          if (err) return reject(err)
          resolve(data.toString())
        })
      })
    }
  }))
  .reduce((all, mod) => Object.assign({}, all, mod), {})

const compareSources = (fname, { js, re }) => async () => {
  const result = compile(await js, fname)
  const expected = format(await re)
  expect(result).toBe(expected)
  expect(result).toBe(format(result))
  expect(result).toMatchSnapshot()
}

Object.entries(testFiles).forEach(([moduleName, source]) => {
  test(`Compile ${moduleName}`, compareSources(moduleName, source))
})
