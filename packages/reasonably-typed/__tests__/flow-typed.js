const path = require('path')
const { readFile } = require('fs')
const glob = require('glob')
const { compile } = require('../../lib')

const getLibDef = fname =>
  new Promise((resolve, reject) => {
    readFile(fname, (err, data) => {
      if (err) return reject(err)
      resolve(data.toString())
    })
  })

const files = glob.sync(
  'src/__tests__/flow-typed/definitions/npm/**/!(test)*.js'
)

files.forEach(f => {
  const libraryName = path.basename(f, '.js')

  test(`Flow Library ${libraryName}`, async () => {
    const libDef = await getLibDef(f)
    const bsInterface = compile(libDef, libraryName)

    expect(bsInterface).toBeTruthy()
  })
})
