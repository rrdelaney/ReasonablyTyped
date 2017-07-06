const path = require('path')
const { readdirSync, readFile } = require('fs')
const { compile, format } = require('../../lib')

const fixtures = path.join(__dirname, 'fixtures')
const fixture = file => path.join(fixtures, file)

const testFiles = readdirSync(fixtures)
  .filter(file => file.endsWith('.re'))
  .map(file => ({
    [file]: {
      js: new Promise((resolve, reject) => {
        readFile(fixture(file.replace('.re', '.js')), (err, data) => {
          if (err) return reject(err)
          resolve(data.toString())
        })
      }),
      re: new Promise((resolve, reject) => {
        readFile(fixture(file), (err, data) => {
          if (err) return resolve(null)
          resolve(data.toString())
        })
      }),
      ts: new Promise((resolve, reject) => {
        readFile(fixture(file.replace('.re', '.d.ts')), (err, data) => {
          if (err) return resolve(null)
          resolve(data.toString())
        })
      })
    }
  }))
  .reduce((all, mod) => Object.assign({}, all, mod), {})

const compareSources = (fname, { js, re, ts }) => async () => {
  const reSrc = await re
  const jsSrc = await js
  const tsSrc = await ts

  if (jsSrc) {
    const jsResult = compile(jsSrc, fname.replace('.re', '.js'))
    const jsExpected = format(reSrc)
    expect(jsResult).toBe(jsExpected)
    expect(jsResult).toBe(format(jsResult))
    expect(jsResult).toMatchSnapshot()
  }

  if (tsSrc) {
    const tsResult = compile(tsSrc, fname.replace('.re', '.d.ts'))
    const tsExpected = format(reSrc)
    expect(tsResult).toBe(tsExpected)
    expect(tsResult).toBe(format(tsResult))
    expect(tsResult).toMatchSnapshot()
  }
}

Object.entries(testFiles).forEach(([moduleName, source]) => {
  test(`Compile ${moduleName}`, compareSources(moduleName, source))
})
