const test = require('ava')
const path = require('path')
const { readdirSync, readFileSync } = require('fs')
const { compile } = require('../')
const fixtures = path.join(__dirname, 'fixtures')
const fixture = file => path.join(fixtures, file)

const testFiles = readdirSync(fixtures)
  .filter(file => file.endsWith('.js'))
  .map(file => ({
    [file]: {
      js: readFileSync(fixture(file)).toString(),
      re: readFileSync(fixture(file.replace('.js', '.re'))).toString()
    }
  }))
  .reduce((all, mod) => Object.assign({}, all, mod), {})


const compareSources = (t, { js, re }) => {
  t.is(compile(js), re)
}

Object.entries(testFiles).forEach(([moduleName, source]) => {
  test(`Compile ${moduleName}`, compareSources, source)
})