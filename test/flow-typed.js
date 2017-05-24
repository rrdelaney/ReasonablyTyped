import test from 'ava'
import path from 'path'
import { readFile } from 'fs'
import glob from 'glob'
import { compile } from '../lib'

const getLibDef = fname =>
  new Promise((resolve, reject) => {
    readFile(fname, (err, data) => {
      if (err) return reject(err)
      resolve(data.toString())
    })
  })

const testFile = async (t, fileName) => {
  const libDef = await getLibDef(fileName)
  const bsInterface = compile(libDef)
  t.pass()
}

const files = glob.sync('test/flow-typed/definitions/npm/**/!(test)*.js')

files.forEach(f => {
  const libraryName = path.basename(f, '.js')
  test(`Library ${libraryName}`, testFile, f)
})
