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

const basedir = 'src/__tests__/definitely-typed/types'
const files = glob.sync(`${basedir}/**/index.d.ts`)

files.forEach(f => {
  const libraryName = f.replace(basedir, '')

  test(`TypeScript Library ${libraryName}`, async () => {
    const libDef = await getLibDef(f)
    const bsInterface = compile(libDef, libraryName)

    expect(bsInterface).toBeTruthy()
  })
})
