const test = require('ava')
const path = require('path')
const { readdirSync, readFile } = require('fs')
const { compile, format } = require('../')
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


const compareSources = async (t, { js, re }) => {
  t.is(compile(await js), format(await re))
}

Object.entries(testFiles).forEach(([moduleName, source]) => {
  test(`Compile ${moduleName}`, compareSources, source)
})